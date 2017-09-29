// Written in the D programming language

/++
    Copyright: Copyright 2017
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Author:   Jonathan M Davis
  +/
module dxml.parser.entity;

import std.range : takeExactly;
import std.range.primitives;
import std.traits;

/++
  +/
struct Entity(R)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    static if(is(R == EntityCompileTests))
        private enum compileInTests = true;
    else
        private enum compileInTests = false;

    /++
        The type used when any slice of the original text is used. If $(D R)
        is a string or supports slicing, then SliceOfR is the same as $(D R);
        otherwise, it's the result of calling $(D takeExactly) on the text.
      +/
    static if(isDynamicArray!R || hasSlicing!R)
        alias SliceOfR = R;
    else
        alias SliceOfR = typeof(takeExactly(R.init, 42));

    ///
    static if(compileInTests) unittest
    {
        import std.algorithm : filter;
        import std.range : takeExactly;

        static assert(is(Entity!string.SliceOfR == string));

        auto range = filter!(a => true)("some xml");

        static assert(is(Entity!(typeof(range)).SliceOfR == typeof(takeExactly(range, 4))));
    }

    /++
      +/
    EntityType type;

    /++
        If $(D type) is $(D EntityType.cdata), $(D EntityType.comment), or
        $(D EntityType.text), this returns the value for that entity. It as
        an error to call it for any other $(D EntityType).
      +/
    @property SliceOfR text()
    {
        assert(0);
    }

    //@property SliceOfR
}

// This is purely to provide a way to trigger the unittest blocks in Entity
// without compiling them in normally.
private struct EntityCompileTests
{
    @property bool empty() { assert(0); }
    @property char front() { assert(0); }
    void popFront() { assert(0); }
    @property typeof(this) save() { assert(0); }
}

unittest
{
    Entity!EntityCompileTests foo;
}


/++
  +/
enum EntityType
{
    cdata,
    comment,
    attribute,
    dtdStartTag,
    dtdEndTag,
    elementStartTag,
    elementEndTag,
    elementEmptyTag,
    processingInstruction,
    text
}
