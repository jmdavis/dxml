// Written in the D programming language

/++
    Copyright: Copyright 2017
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Author:   Jonathan M Davis
  +/
module dxml.reader.internal;

import std.range.primitives;
import std.traits;

package:

// We have this rather than using std.utf.byCodeUnit, because
// std.utf.byCodeUnit does not provide access to the underlying string.
auto byCodeUnit(R)(R range)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    static if(isNarrowString!R)
        return ByCodeUnit!R(range);
    else
        return range;
}

struct ByCodeUnit(S)
    if(isNarrowString!S && !is(S == enum))
{
    alias C = ElementEncodingType!S;

    @property C front() @safe const pure nothrow @nogc
    {
        return source[0];
    }

    @property bool empty() @safe const pure nothrow @nogc
    {
        return source.empty;
    }

    void popFront() @safe pure nothrow @nogc
    {
        source = source[1 .. $];
    }

    auto save() @safe pure nothrow @nogc
    {
        return this;
    }

    size_t length() @safe const pure nothrow @nogc
    {
        return source.length;
    }

    C opIndex(size_t i) @safe pure nothrow @nogc
    {
        return source[i];
    }

    auto opSlice(size_t i, size_t j) @safe pure nothrow @nogc
    {
        return ByCodeUnit(source[i .. j]);
    }

    S source;
}

@system pure nothrow @nogc unittest
{
    import std.algorithm : filter;
    import std.meta : AliasSeq;

    foreach(C; AliasSeq!(char, wchar, dchar))
    {
        static if(is(C == char))
            enum str = "hello";
        else static if(is(C == wchar))
            enum str = "hello"w;
        else
            enum str = "hello"d;

        foreach(S; AliasSeq!(C[], const(C)[], immutable(C)[]))
        {
            static if(is(C == dchar))
                static assert(is(typeof(byCodeUnit(cast(S)str)) == S));
            else
                static assert(is(typeof(byCodeUnit(cast(S)str)) == ByCodeUnit!(typeof(S.init[]))));

            static assert(is(typeof(byCodeUnit(filter!"true"(cast(S)str))) == typeof(filter!"true"(cast(S)str))));
        }

        foreach(D; AliasSeq!(C, const C, immutable C))
            static assert(is(typeof(byCodeUnit(FwdCharRange!D(cast(D[])str))) == FwdCharRange!D));
    }
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;

    foreach(str; AliasSeq!("hello"c, "ディラン"c, "hello"w, "ディラン"w, "hello"d, "ディラン"d))
    {
        foreach(range; AliasSeq!(str, raCharRange(str)))
        {
            auto bcu = byCodeUnit(range);
            assert(bcu.length == str.length);

            auto bcu2 = bcu.save;
            foreach(c; str)
            {
                assert(bcu.front == c);
                bcu.popFront();
            }

            foreach(i, c; str)
                assert(bcu2[i] == c);
        }
    }

    auto str = "hello world";
    auto bcu = byCodeUnit(str);
    assert(str is bcu.source);
    str = str[4 .. $];
    assert(str !is bcu.source);
    bcu.popFrontN(4);
    assert(str is bcu.source);
}

template isWrappedString(T)
{
    enum isWrappedString = isInstanceOf!(ByCodeUnit, T);
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
