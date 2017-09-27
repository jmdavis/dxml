// Written in the D programming language

/++
    Copyright: Copyright 2017
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Author:   Jonathan M Davis
  +/
module dxml.parser.common;

/++
    Where in the XML text an XML fragment is. If the $(D PositionType) when
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
