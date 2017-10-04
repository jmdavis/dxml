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
+/
