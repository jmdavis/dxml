// Written in the D programming language

/+
    Copyright: Copyright 2017
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   Jonathan M Davis
  +/
module dxml.reader.internal;

import std.range.primitives;
import std.traits;

package:


template isWrappedString(R)
    if(isInputRange!R && isSomeChar!(ElementType!R))
{
    import std.utf : byCodeUnit;
    static if(!is(typeof(byCodeUnit(R.init)) == R))
        enum isWrappedString = false;
    else static if(is(typeof(R.init.source)))
        enum isWrappedString = isNarrowString!(typeof(R.init.source));
    else
        enum isWrappedString = false;
}

unittest
{
    import std.algorithm : filter;
    import std.meta : AliasSeq;
    import std.utf : byCodeUnit;
    foreach(T; AliasSeq!(string, wstring, dstring, typeof(filter!(a => true)("hello"))))
        static assert(!isWrappedString!T);
    static assert(isWrappedString!(typeof(byCodeUnit("hello"))));
    static assert(isWrappedString!(typeof(byCodeUnit("hello"w))));
    static assert(!isWrappedString!(typeof(byCodeUnit("hello"d))));
}


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
    import std.utf : byCodeUnit;

    foreach(str; AliasSeq!("hello world", "hello world"w, "hello world"d))
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


// This is used for the cases where we need to take a range and strip ByCodeUnit
// from it if it's a wrapped string and return the original otherwise.
pragma(inline, true) auto stripBCU(R)(R range)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    static assert(!isNarrowString!R);
    static if(isWrappedString!R)
        return range.source;
    else
        return range;
}

unittest
{
    import std.algorithm : equal, filter;
    import std.range : takeExactly;
    import std.utf : byCodeUnit, codeLength;

    auto bcuResult = byCodeUnit("hello").stripBCU();
    assert(equal(bcuResult, "hello"));
    static assert(is(typeof(bcuResult) == string));

    auto filterResult = filter!"true"("hello").stripBCU();
    assert(equal(filterResult, "hello"));
    static assert(is(typeof(filterResult) == typeof(filter!"true"("foo"))));
}


// Used for keeping track of the names of start tags so that end tags can be
// verified.
struct TagStack(R)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    import std.range : takeExactly;

    void push(R tagName)
    {
        tags ~= tagName;
    }

    void pop()
    {
        --tags.length;
        tags.assumeSafeAppend();
    }

    R back()
    {
        return tags.back;
    }

    bool empty()
    {
        return tags.empty;
    }

    R[] tags;
}

unittest
{
    TagStack!string stack;
    stack.push("hello");
    assert(stack.tags == ["hello"]);
    stack.push("world");
    assert(stack.tags == ["hello", "world"]);
    stack.pop();
    assert(stack.tags == ["hello"]);
    stack.push("sally");
    stack.push("poe");
    stack.push("foo");
    assert(stack.tags == ["hello", "sally", "poe", "foo"]);
    stack.pop();
    stack.pop();
    assert(stack.tags == ["hello", "sally"]);
    stack.pop();
    stack.pop();
    assert(stack.tags.empty);
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
