// Written in the D programming language

/++
    Copyright: Copyright 2017
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Author:   Jonathan M Davis
  +/
module dxml.reader.parser;

import std.range.primitives;
import std.traits;
import std.typecons : Flag;

import dxml.reader.entity;


/++
    Where in the XML text an $(D Entity) is. If the $(D PositionType) when
    parsing does not keep track of a given field, then that field will always be
    $(D -1).
  +/
struct SourcePos
{
    /// A line number in the XML file.
    int line = -1;

    /++
        A column number in a line of the XML file.

        Each code unit is considered a column, so depending on what a program
        is looking to do with the column number, it may need to examine the
        actual text on that line and calculate the number that represents
        what the program wants to display (e.g. the number of graphemes).
      +/
    int col = -1;
}


/++
    At what level of granularity the position in the XML file should be kept
    track of (be it for error reporting or any other use the application might
    have for that information). $(D none) is the most efficient but least
    informative, whereas $(D lineAndCol) is the most informative but least
    efficient.
  +/
enum PositionType
{
    /// Both the line number and the column number are kept track of.
    lineAndCol,

    /// The line number is kept track of but not the column.
    line,

    /// The position is not tracked.
    none,
}


///
alias SkipComments = Flag!"SkipComments";

///
alias SkipDTD = Flag!"SkipDTD";

///
alias SkipProlog = Flag!"SkipProlog";


/++
    Used to configure how the parser works.
  +/
struct Config
{
    /++
        Whether the comments should be skipped while parsing.

        If $(D true), any $(D Entity)s of type $(D EntityType.comment) will be
        omitted from the parsing results.
      +/
    auto skipComments = SkipComments.no;

    /++
        Whether the DOCTYPE entities should be skipped while parsing.

        If $(D true), any $(D Entity)s of type $(D EntityType.dtdStartTag) and
        $(D EntityType.dtdEndTag) and any entities in between will be omitted
        from the parsing results.
      +/
    auto skipDTD = SkipDTD.no;

    /++
        Whether the prolog should be skipped while parsing.

        If $(D true), any $(D Entity)s prior to the root element will omitted
        from the parsing results.
      +/
    auto skipProlog = SkipProlog.no;

    ///
    PositionType posType = PositionType.lineAndCol;
}


/++
    This $(D Config) will skip comments, DOCTYPE entities, and the prolog, and
    it's posType is $(D PositionType.lineAndCol).
  +/
enum simpleXML = Config(SkipComments.yes, SkipDTD.yes, SkipProlog.yes, PositionType.lineAndCol);


/++
    The exception type thrown when the XML parser runs into invalid XML.
  +/
class XMLParsingException : Exception
{
    /++
        The position in the XML input where the problem is.

        How informative it is depends on the $(D PositionType) used when parsing
        the XML.
      +/
    SourcePos pos;

package:

    this(string msg, SourcePos sourcePos, string file = __FILE__, size_t line = __LINE__)
    {
        pos = sourcePos;
        super(msg, file, line);
    }
}
