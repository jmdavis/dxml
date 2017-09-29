// Written in the D programming language

/++
    Copyright: Copyright 2017
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Author:   Jonathan M Davis
  +/
module dxml.reader.range;

import std.range.primitives;
import std.traits;

import dxml.reader.internal;


/+

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
    import std.ascii : isDigit;

    auto state = State(input);

    // if <?xml
    //    <--
    //    <?
    //    WS
    //    <!DOCTYPE
    //    <

    enum errorMsg = "Invalid XML prolog";

    // <?xml version="1.x"
    if(!stripStartsWith(state, "<?xml"))
        throw new XMLParsingException(errorMsg, state.pos);
    if(!stripWS(state))
        throw new XMLParsingException(errorMsg, state.pos);

    if(!stripStartsWith(state, "version"))
        throw new XMLParsingException(errorMsg, state.pos);
    stripWS(state);
    if(!stripStartsWith(state, "="))
        throw new XMLParsingException(errorMsg, state.pos);
    stripWS(state);

    if(state.input.empty)
        throw new XMLParsingException(errorMsg, state.pos);
    immutable quote = state.input.front;
    if(quote != '\'' && quote != '"')
        throw new XMLParsingException(errorMsg, state.pos);
    popFrontAndIncCol(state);
    if(!stripStartsWith(state, "1."))
        throw new XMLParsingException(errorMsg, state.pos);
    if(state.input.empty || !isDigit(state.input.front))
        throw new XMLParsingException("Unsupported XML version", state.pos);
    popFrontAndIncCol(state);
    if(state.input.empty || state.input.front != quote)
        throw new XMLParsingException(errorMsg, state.pos);

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


// Strips whitespace while dealing with state.pos accordingly. Newlines are not
// ignored.
// Returns whether any whitespace was stripped.
bool stripWS(PS)(ref PS state)
{
    alias R = typeof(PS.input);
    enum hasLengthAndCol = hasLength!R && PS.config.posType == PositionType.lineAndCol;

    bool strippedSpace = false;

    static if(hasLengthAndCol)
        size_t lineStart = state.input.length;

loop: while(!state.input.empty)
    {
        switch(state.input.front)
        {
            case ' ':
            case '\t':
            case '\r':
            {
                strippedSpace = true;
                state.input.popFront();
                static if(!hasLength!R)
                    nextCol!(PS.config)(state.pos);
                break;
            }
            case '\n':
            {
                strippedSpace = true;
                state.input.popFront();
                static if(hasLengthAndCol)
                    lineStart = state.input.length;
                nextLine!(PS.config)(state.pos);
                break;
            }
            default: break loop;
        }
    }

    static if(hasLengthAndCol)
        state.pos.col += lineStart - state.input.length;

    return strippedSpace;
}

unittest
{
    import std.algorithm : equal, filter;
    import std.conv : to;
    import std.meta : AliasSeq;
    import std.typecons : tuple;

    enum origHaystack1 = "  \t\rhello world";
    enum origHaystack2 = "  \n \n \n  \nhello world";
    enum origHaystack3 = "  \n \n \n  \n  hello world";
    enum origHaystack4 = "hello world";

    enum remainder = "hello world";

    foreach(func; AliasSeq!(a => to!string(a), a => to!wstring(a), a => to!dstring(a), a => filter!"true"(a),
                            a => fwdCharRange(a), a => rasRefCharRange(a)))
    {
        auto haystack1 = func(origHaystack1);

        foreach(t; AliasSeq!(tuple(Config.init, SourcePos(1, 5)),
                             tuple(Config(PositionType.line), SourcePos(1, -1)),
                             tuple(Config(PositionType.none), SourcePos(-1, -1))))
        {
            auto state = parserState!(t[0])(haystack1.save);
            assert(state.stripWS());
            assert(equal(state.input, remainder));
            assert(state.pos == t[1]);
        }

        auto haystack2 = func(origHaystack2);

        foreach(t; AliasSeq!(tuple(Config.init, SourcePos(5, 1)),
                             tuple(Config(PositionType.line), SourcePos(5, -1)),
                             tuple(Config(PositionType.none), SourcePos(-1, -1))))
        {
            auto state = parserState!(t[0])(haystack2.save);
            assert(state.stripWS());
            assert(equal(state.input, remainder));
            assert(state.pos == t[1]);
        }

        auto haystack3 = func(origHaystack3);

        foreach(t; AliasSeq!(tuple(Config.init, SourcePos(5, 3)),
                             tuple(Config(PositionType.line), SourcePos(5, -1)),
                             tuple(Config(PositionType.none), SourcePos(-1, -1))))
        {
            auto state = parserState!(t[0])(haystack3.save);
            assert(state.stripWS());
            assert(equal(state.input, remainder));
            assert(state.pos == t[1]);
        }

        auto haystack4 = func(origHaystack4);

        foreach(t; AliasSeq!(tuple(Config.init, SourcePos(1, 1)),
                             tuple(Config(PositionType.line), SourcePos(1, -1)),
                             tuple(Config(PositionType.none), SourcePos(-1, -1))))
        {
            auto state = parserState!(t[0])(haystack4.save);
            assert(!state.stripWS());
            assert(equal(state.input, remainder));
            assert(state.pos == t[1]);
        }
    }
}


pragma(inline, true) void popFrontAndIncCol(PS)(ref PS state)
{
    state.input.popFront();
    nextCol!(PS.config)(state.pos);
}

pragma(inline, true) void nextLine(Config config)(ref SourcePos pos)
{
    static if(config.posType != PositionType.none)
        ++pos.line;
    static if(config.posType == PositionType.lineAndCol)
        pos.col = 1;
}

pragma(inline, true) void nextCol(Config config)(ref SourcePos pos)
{
    static if(config.posType == PositionType.lineAndCol)
        ++pos.col;
}
+/
