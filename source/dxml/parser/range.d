// Written in the D programming language

/++
    Copyright: Copyright 2017
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Author:   Jonathan M Davis
  +/
module dxml.parser.range;

import std.range.primitives;
import std.traits;

import dxml.parser.common;
import dxml.parser.internal;


/++
  +/
struct XMLFragment(R)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    /++
      +/
    FragmentType type;

    /++
        The position in the file where this XML fragment starts.
      +/
    SourcePos pos;
}

/++
  +/
enum FragmentType
{
    cdata,
    comment,
    attribute,
    elementStart,
    elementEnd,
    processingInstructionStart,
    processingInstructionEnd,
}

/++
    Used to configure how the parser works.
  +/
struct Config
{
    /++
        Whether the comments should be skipped while parsing.

        If $(D true), comments will still be checked for validity like they
        would be if kept in the parsing results (not that there's much validity
        checking for comments), but the comments themselves will need show up
        as $(D XMLFragment)s in the range.
      +/
    bool skipComments = true;

    ///
    PositionType posType = PositionType.lineAndCol;

    /++
      +/
    this(bool skipComments, PositionType posType)
    {
        this.skipComments = skipComments;
        this.posType = posType;
    }

    /++
      +/
    this(bool skipComments)
    {
        this.skipComments = skipComments;
    }

    /++
      +/
    this(PositionType posType)
    {
        this.posType = posType;
    }
}

/++
    Returns a lazy, forward range of $(D XMLFragment)s.
  +/
auto parseXML(Config config = Config.init, R)(R xmlText)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    return Parser!config(startParsing!config(xmlText));
}

/++
    Returns a lazy, forward range of $(D XMLFragment)s.

    The difference between this and parseXML is that it does not expect the
    prolog containing the XML version information and instead expects the first
    XML tag. This is, of course, not compliant with the standard, but it can
    make testing code that operates on sections of XML much more pleasant,
    since it negates the need to prepend the prolog on every piece XML that's
    going to be parsed in a test.
  +/
auto parseNakedXML(Config config = Config.init, R)(R xmlText)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    return Parser!config(startNakedParsing!config(xmlText));
}

//------------------------------------------------------------------------------
// Private stuff
//------------------------------------------------------------------------------

private:

struct Parser(Config config, R)
{
public:

    @property bool empty()
    {
        // Because we store front rather than calculating it every time, we
        // need to be able to distinguish between when the input range is
        // empty but front is valid and when this range is actually empty.
        // So, _lastElement is true when _input first becomes empty but
        // false otherwise.
        return _state.lastElement ? false : _state.input.empty;
    }

    @property XMLFragment front()
    {
        return _state.front;
    }

    void popFront()
    {
        nextFragment(_state);
    }

    auto save()
    {
        return this;
    }

private:

    ParserState!(config, R) _state;
}


// We could just have this be directly in Parser and have all of the various
// parsing functions be member functions, but that doesn't work as well with
// the unit tests, since if everything was in Parser, we'd then either need to
// to deal with ensuring that the unit tests only got instantiated for one
// particular instantiation of Parser, or we'd have to put all of the tests
// outside of Parser, separately from what they're testing. By breaking things
// out like this, we avoid that problem. In theory, the compiler should optimize
// out any extra overhead that results from it.
struct ParserState(Config cfg, R)
{
    alias config = cfg;

    XMLFragment!R front;
    typeof(byCodeUnit(R.init)) input;

    static if(config.posType == PositionType.lineAndCol)
        SourcePos pos = SourcePos(1, 1);
    else static if(config.posType == PositionType.line)
        SourcePos pos = SourcePos(1, -1);
    else
        SourcePos pos;

    bool lastElement;

    this(R input)
    {
        this.input = byCodeUnit(input);
    }
}

auto parserState(Config config, R)(R range)
{
    return ParserState!(config, R)(range);
}


auto startParsing(Config config, R)(R input)
{
    auto state = State(input);

    if(!stripStartsWith(state, "<?xml"))
        throw new XParsingException("XML must start with <?xml", state.pos);

    return state;
}


auto startNakedParsing(Config config, R)(R input)
{
    auto state = State(input);

    return state;
}


// Similar to startsWith except that it consumes the part of the range that
// matches. It also deals with incrementing state.pos.col.
//
// It is assumed that there are no newlines.
bool stripStartsWith(PS)(ref PS state, string text)
{
    alias R = typeof(PS.input);

    static if(hasLength!R)
    {
        if(state.input.length < text.length)
            return false;

        // This branch is separate so that we can take advantage of whatever
        // speed boost comes from comparing strings directly rather than
        // comparing individual characters.
        static if(isWrappedString!R && is(Unqual!(ElementType!R) == char))
        {
            if(state.input.orig[0 .. text.length] != text)
                return false;
            state.input.popFrontN(text.length);
        }
        else
        {
            foreach(c; text)
            {
                if(state.input.front != c)
                    return false;
                state.input.popFront();
            }
        }
    }
    else
    {
        foreach(c; text)
        {
            if(state.input.empty)
                return false;
            if(state.input.front != c)
                return false;
            state.input.popFront();
        }
    }

    static if(state.config.posType == PositionType.lineAndCol)
        state.pos.col += text.length;

    return true;
}

unittest
{
    import std.algorithm : equal, filter;
    import std.conv : to;
    import std.meta : AliasSeq;
    import std.typecons : tuple;

    enum origHaystack = "hello world";
    enum needle = "hello";
    enum remainder = " world";

    foreach(func; AliasSeq!(a => to!string(a), a => to!wstring(a), a => to!dstring(a), a => filter!"true"(a),
                            a => fwdCharRange(a), a => rasRefCharRange(a)))
    {
        auto haystack = func(origHaystack);

        foreach(t; AliasSeq!(tuple(Config.init, SourcePos(1, needle.length + 1)),
                             tuple(Config(PositionType.line), SourcePos(1, -1)),
                             tuple(Config(PositionType.none), SourcePos(-1, -1))))
        {
            auto state = parserState!(t[0])(haystack.save);
            assert(state.stripStartsWith(needle));
            assert(equal(state.input, remainder));
            assert(state.pos == t[1]);
        }

        foreach(t; AliasSeq!(tuple(Config.init, SourcePos(1, origHaystack.length + 1)),
                             tuple(Config(PositionType.line), SourcePos(1, -1)),
                             tuple(Config(PositionType.none), SourcePos(-1, -1))))
        {
            auto state = parserState!(t[0])(haystack.save);
            assert(state.stripStartsWith(origHaystack));
            assert(state.input.empty);
            assert(state.pos == t[1]);
        }

        foreach(t; AliasSeq!(tuple(Config.init, SourcePos(1, 1)),
                             tuple(Config(PositionType.line), SourcePos(1, -1)),
                             tuple(Config(PositionType.none), SourcePos(-1, -1))))
        {
            {
                auto state = parserState!(t[0])(haystack.save);
                assert(!state.stripStartsWith("foo"));
                assert(state.pos == t[1]);
            }
            {
                auto state = parserState!(t[0])(haystack.save);
                assert(!state.stripStartsWith("hello sally"));
                assert(state.pos == t[1]);
            }
            {
                auto state = parserState!(t[0])(haystack.save);
                assert(!state.stripStartsWith("hello world "));
                assert(state.pos == t[1]);
            }
        }
    }
}
