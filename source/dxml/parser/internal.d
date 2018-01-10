// Written in the D programming language

/+
    Copyright: Copyright 2017
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   Jonathan M Davis
  +/
module dxml.parser.internal;

import std.range : takeExactly;
import std.range.primitives;
import std.traits;
import std.utf : byCodeUnit;

package:

// Like std.algorithm.equal except that it won't decode string, and it allows
// comparing a string with a range with a character type other than char.
// However, the text must be ASCII.
bool equalCU(R)(R range, string text)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    static if(hasLength!R)
    {
        if(range.length != text.length)
            return false;
    }

    foreach(c; text)
    {
        if(range.empty || range.front != c)
            return false;
        range.popFront();
    }

    return range.empty;
}

unittest
{
    import std.algorithm : filter;
    import std.meta : AliasSeq;

    static foreach(str; AliasSeq!("hello world", "hello world"w, "hello world"d))
    {
        assert(equalCU(str, "hello world"));
        assert(equalCU(byCodeUnit(str), "hello world"));
        assert(equalCU(filter!"true"(str), "hello world"));
        assert(equalCU(filter!"true"(byCodeUnit(str)), "hello world"));

        assert(!equalCU(str, "hello worl"));
        assert(!equalCU(byCodeUnit(str), "hello worl"));
        assert(!equalCU(filter!"true"(str), "hello worl"));
        assert(!equalCU(filter!"true"(byCodeUnit(str)), "hello worl"));

        assert(!equalCU(str, "hello world "));
        assert(!equalCU(byCodeUnit(str), "hello world "));
        assert(!equalCU(filter!"true"(str), "hello world "));
        assert(!equalCU(filter!"true"(byCodeUnit(str)), "hello world "));
    }
}


/+
    This is used for the cases where we need to take a range and strip
    ByCodeUnit from it if it's a wrapped string and return the original
    otherwise - e.g. when we want to return a slice of the XML text via a
    property of EntityCursor.

    Typically, we're either operating on a string that we needed to wrap using
    byCodeUnit, or we're operating on a range that didn't need to be wrapped,
    but it's possible for someone to give us a range that's the result of
    calling byCodeUnit on a range that gets wrapped by byCodeUnit, and in that
    case, when we call byCodeUnit on it (e.g. in ParserState's constructor), it
    would just return the same type, meaning that if we just always stripped off
    the byCodeUnit wrapper with stripBCU, we wouldn't be returning the range
    type that we were given, which is the whole point of stripBCU. So, by taking
    into account the range type we were given with Orig, we're able to correctly
    determine whether we need to strip off the byCodeUnit wrapper.
  +/
pragma(inline, true) auto stripBCU(Orig, R)(R range)
    if(is(typeof(takeExactly(byCodeUnit(Orig.init), 42)) == R))
{
    static if(isNarrowString!Orig)
        return range.source;
    else
        return range;
}

unittest
{
    import std.algorithm : equal, filter;
    import std.range : takeExactly;

    auto bcu = byCodeUnit("hello");
    auto bcuResult = bcu.stripBCU!string();
    assert(equal(bcuResult, "hello"));
    static assert(is(typeof(bcuResult) == string));

    auto unstripped = bcu.stripBCU!(typeof(bcu))();
    assert(equal(bcuResult, "hello"));
    static assert(is(typeof(unstripped) == typeof(bcu)));

    auto filtered = filter!"true"("hello").takeExactly(3);
    auto filterResult = filtered.stripBCU!(typeof(filtered))();
    assert(equal(filterResult, "hel"));
    static assert(is(typeof(filterResult) == typeof(filtered)));
}


//------------------------------------------------------------------------------
// Unit test helpers
//------------------------------------------------------------------------------

version(unittest):

struct FwdCharRange(C)
{
public:

    @property bool empty() @safe const pure nothrow @nogc
    {
        return _str.empty;
    }

    @property C front() @safe const pure nothrow @nogc
    {
        return _str[0];
    }

    void popFront() @safe pure nothrow @nogc
    {
        _str = _str[1 .. $];
    }

    auto save() @safe pure nothrow @nogc
    {
        return this;
    }

    this(C[] str)
    {
        _str = str;
    }

private:

    C[] _str;
}

auto fwdCharRange(R)(R range)
    if(isSomeString!R && !is(R == enum))
{
    return FwdCharRange!(ElementEncodingType!R)(range);
}

static assert(isForwardRange!(FwdCharRange!char));
static assert(!isBidirectionalRange!(FwdCharRange!char));


class FwdRefCharRange(C)
{
public:

    @property bool empty() @safe const pure nothrow @nogc
    {
        return _str.empty;
    }

    @property C front() @safe const pure nothrow @nogc
    {
        return _str[0];
    }

    void popFront() @safe pure nothrow @nogc
    {
        _str = _str[1 .. $];
    }

    auto save() @safe pure nothrow
    {
        return new FwdRefCharRange(_str);
    }

    this(C[] str)
    {
        _str = str;
    }

private:

    C[] _str;
}

auto fwdRefCharRange(R)(R range)
    if(isSomeString!R && !is(R == enum))
{
    return new FwdRefCharRange!(ElementEncodingType!R)(range);
}

static assert(isForwardRange!(FwdRefCharRange!char));
static assert(!isBidirectionalRange!(FwdRefCharRange!char));


struct RACharRange(C)
{
public:

    @property bool empty() @safe const pure nothrow @nogc
    {
        return _str.empty;
    }

    @property C front() @safe const pure nothrow @nogc
    {
        return _str[0];
    }

    void popFront() @safe pure nothrow @nogc
    {
        _str = _str[1 .. $];
    }

    auto save() @safe pure nothrow @nogc
    {
        return this;
    }

    @property C back() @safe const pure nothrow @nogc
    {
        return _str[$ - 1];
    }

    void popBack() @safe pure nothrow @nogc
    {
        _str = _str[0 .. $ - 1];
    }

    @property size_t length() @safe const pure nothrow @nogc
    {
        return _str.length;
    }

    C opIndex(size_t i) @safe const pure nothrow @nogc
    {
        return _str[i];
    }

    this(C[] str)
    {
        _str = str;
    }

private:

    C[] _str;
}

auto raCharRange(R)(R range)
    if(isSomeString!R && !is(R == enum))
{
    return RACharRange!(ElementEncodingType!R)(range);
}

static assert(isForwardRange!(RACharRange!char));
static assert(isRandomAccessRange!(RACharRange!char));
static assert(!hasSlicing!(RACharRange!char));


struct RASCharRange(C)
{
public:

    @property bool empty() @safe const pure nothrow @nogc
    {
        return _str.empty;
    }

    @property C front() @safe const pure nothrow @nogc
    {
        return _str[0];
    }

    void popFront() @safe pure nothrow @nogc
    {
        _str = _str[1 .. $];
    }

    auto save() @safe pure nothrow @nogc
    {
        return this;
    }

    @property C back() @safe const pure nothrow @nogc
    {
        return _str[$ - 1];
    }

    void popBack() @safe pure nothrow @nogc
    {
        _str = _str[0 .. $ - 1];
    }

    @property size_t length() @safe const pure nothrow @nogc
    {
        return _str.length;
    }

    C opIndex(size_t i) @safe const pure nothrow @nogc
    {
        return _str[i];
    }

    auto opSlice(size_t i, size_t j) @safe pure nothrow @nogc
    {
        return RASCharRange(_str[i .. j]);
    }

    this(C[] str)
    {
        _str = str;
    }

private:

    C[] _str;
}

version(unittest) auto rasCharRange(R)(R range)
    if(isSomeString!R && !is(R == enum))
{
    return RASCharRange!(ElementEncodingType!R)(range);
}

static assert(isForwardRange!(RASCharRange!char));
static assert(isRandomAccessRange!(RASCharRange!char));
static assert(hasSlicing!(RASCharRange!char));


class RASRefCharRange(C)
{
public:

    @property bool empty() @safe const pure nothrow @nogc
    {
        return _str.empty;
    }

    @property C front() @safe const pure nothrow @nogc
    {
        return _str[0];
    }

    void popFront() @safe pure nothrow @nogc
    {
        _str = _str[1 .. $];
    }

    auto save() @safe pure nothrow
    {
        return new RASRefCharRange(_str);
    }

    @property C back() @safe const pure nothrow @nogc
    {
        return _str[$ - 1];
    }

    void popBack() @safe pure nothrow @nogc
    {
        _str = _str[0 .. $ - 1];
    }

    @property size_t length() @safe const pure nothrow @nogc
    {
        return _str.length;
    }

    C opIndex(size_t i) @safe const pure nothrow @nogc
    {
        return _str[i];
    }

    auto opSlice(size_t i, size_t j) @safe pure nothrow
    {
        return new RASRefCharRange(_str[i .. j]);
    }

    this(C[] str)
    {
        _str = str;
    }

private:

    C[] _str;
}

version(unittest) auto rasRefCharRange(R)(R range)
    if(isSomeString!R && !is(R == enum))
{
    return new RASRefCharRange!(ElementEncodingType!R)(range);
}

static assert(isForwardRange!(RASRefCharRange!char));
static assert(isRandomAccessRange!(RASRefCharRange!char));
static assert(hasSlicing!(RASRefCharRange!char));
