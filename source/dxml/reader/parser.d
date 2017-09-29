// Written in the D programming language

/++
    Copyright: Copyright 2017
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Author:   Jonathan M Davis
  +/
module dxml.reader.parser;

import std.range.primitives;
import std.range : takeExactly;
import std.traits;
import std.typecons : Flag, Nullable;


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


/// Flag for use with Config.
alias SkipComments = Flag!"SkipComments";

/// Flag for use with Config.
alias SkipDTD = Flag!"SkipDTD";

/// Flag for use with Config.
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
    Lazily parses the given XML.

    Due to XML's tree structure, returning a range of XML elements doesn't
    really work, but the resulting $(D Entity) does parse the XML lazily like
    would be typical with a range.

    If invalid XML is encountered at any point during the parsing process, an
    $(D XMLParsingException) will be thrown.

    However, note that the minimal validation required to correctly parse the
    document will be done. So, if the given Config indicates that any parts of
    the XML should be skipped, then the only validation that will be done on
    those portions is the validation that is required to correctly determine
    where the skipped portion teriminates. Similarly, when calling functions
    on an $(D Entity) which would skip portions of the XML (e.g. calling
    $(D next) to skip any attributes or child entities and go directly to the
    next entity at the same level), the skipped portions will only be validated
    enough to correctly determine where those portion terminate. So, to fully
    validate the XML, it must be fully parsed with no portions skipped.
  +/
Entity parseXML(Config config, R)(R xmlText)
{
    assert(0);
}


/++
  +/
enum EntityType
{
    /// A cdata section: $(D <![CDATA[ ... ]]>).
    cdata,

    /// An XML comment: $(D <!-- ... -->).
    comment,

    /// The $(D <!DOCTYPE .. >) tag.
    docType,

    /++
        The start tag for an element. e.g. $(D <foo name="value">).

        End tags are not explicitly represented, as they're represented by
        reaching the end of entities within content of an element tag or simply
        by skipping the contents and going to the next tag at the same level.

        Similarly, an empty element tag and a start tag immediately followed by
        an end tag are not distinguished from one another.
      +/
    element,

    /++
        A processing instruction such as <?foo?>. Note that $(D <?xml ... ?>) is
        an $(D xmlDecl) and not a processingInstruction.
      +/
    processingInstruction,

    /++
        The content of an element tag that is simple text.

        If there is an entity other than the end tag following the text, then
        the text includes up to that entity.
      +/
    text,

    /++
        The $(D <?xml ... ?>) entity that can start an XML 1.0 document and must
        start an XML 1.1 document.
      +/
    xmlDecl
}


/++
    Represents an entity in an XML document. Which operations are legal depend
    on the $(D type) of the Entity, and each function or property indicates
    what the valid values of $(D type) are to call it. It is an error to call
    any function or property when $(D type) is not one of its supported
    $(D EntityType)s. Typically, that is checked with an assertion.

  +/
struct Entity(R)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    import std.algorithm : canFind;
    import std.range : only;

    private enum compileInTests = is(R == EntityCompileTests);

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
        The type of entity in the XML document that this $(D Entity) represents.

        The value of this member determines which member functions and
        properties are allowed to be called.
      +/
    EntityType type;

    /++
        Gives the name of the entity.

        Note that this is the direct name in the XML for this entity and does
        not contain any of the names of any of the parent entities that this
        entity has.

        $(TABLE,
          $(TR $(TH Supported $(D EntityType)s)),
          $(TR $(TD $(D EntityType.docType))),
          $(TR $(TD $(D EntityType.element))),
          $(TR $(TD $(D EntityType.processingInstruction))))
      +/
    @property SliceOfR name()
    {
        with(EntityType)
            assert(only(docType, element, processingInstruction).canFind(type));

        assert(0);
    }

    /++
        Returns a range of attributes where each attribute is represented as a
        $(D Tuple!(SliceOfR, "name", SliceOfR, "value")).

        $(TABLE,
          $(TR $(TH Supported $(D EntityType)s)),
          $(TR $(TD $(D EntityType.element))))
      +/
    @property auto attributes()
    {
        with(EntityType)
            assert(type == element);

        import std.typecons : Tuple;
        alias Attribute = Tuple!(SliceOfR, "name", SliceOfR, "value");
        assert(0);
    }

    /++
        Returns the value of the entity.

        In the case of $(D EntityType.processingInstruction), this is the text
        that follows the name.

        $(TABLE,
          $(TR $(TH Supported $(D EntityType)s)),
          $(TR $(TD $(D EntityType.cdata))),
          $(TR $(TD $(D EntityType.comment))),
          $(TR $(TD $(D EntityType.processingInstruction))),
          $(TR $(TD $(D EntityType.text))))
      +/
    @property SliceOfR text()
    {
        with(EntityType)
            assert(only(cdata, comment, processingInstruction, text).canFind(type));

        assert(0);
    }

    /++
        Returns the first entity between a start tag and end tag. If there is
        nothing between them, then it's an $(D EntityType.text) with empty text.

        $(TABLE,
          $(TR $(TH Supported $(D EntityType)s)),
          $(TR $(TD $(D EntityType.element))))
      +/
    Entity content()
    {
        with(EntityType)
            assert(type == element);
        assert(0);
    }

    /++
        Returns the contents between a start tag and end tag as text, leaving
        any markup in between as unprocessed text.

        $(TABLE,
          $(TR $(TH Supported $(D EntityType)s)),
          $(TR $(TD $(D EntityType.element))))
      +/
    @property SliceOfR contentAsText()
    {
        with(EntityType)
            assert(type == element);

        assert(0);
    }

    /++
        Returns the $(D XMLDecl) corresponding to this entity.

        $(TABLE,
          $(TR $(TH Supported $(D EntityType)s)),
          $(TR $(TD $(D EntityType.xmlDecl))))
      +/
    @property XMLDecl!R xmlDecl()
    {
        assert(type == EntityType.xmlDecl);
        assert(0);
    }
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
    Information parsed from a <?xml ... ?> declaration.

    Note that while XML 1.1 requires this declaration, it's optional in XML
    1.0.
  +/
struct XMLDecl(R)
{
    import std.typecons : Nullable;

    /++
        The type used when any slice of the original text is used. If $(D R)
        is a string or supports slicing, then SliceOfR is the same as $(D R);
        otherwise, it's the result of calling $(D takeExactly) on the text.
      +/
    static if(isDynamicArray!R || hasSlicing!R)
        alias SliceOfR = R;
    else
        alias SliceOfR = typeof(takeExactly(R.init, 42));

    /++
        The version of XML that the document contains.
      +/
    SliceOfR xmlVersion;

    /++
        The encoding of the text in the XML document.

        Note that dxml only supports UTF-8, UTF-16, and UTF-32, and it is
        assumed that the encoding matches the character type. The parser
        ignores this field aside from providing its value as part of an XMLDecl.

        And as the current XML spec points out, including the encoding as part
        of the XML itself doesn't really work anyway, because you have to know
        the encoding before you can read the text. One possible enhancement
        would be to provide a function specifically for parsing the XML
        declaration and attempting to determine the encoding in the process
        (which the spec discusses), in which case, the caller could then use
        that information to convert the text to UTF-8, UTF-16, or UTF-32 before
        passing it to $(D parseXML), but dxml does not currently have such a
        function.
      +/
    Nullable!SliceOfR encoding;

    /++
        $(D true) if the XML document does $(I not) contain any external
        references. $(D false) if it does or may contain external references.
        It's null if the $(D "standalone") declaration was not included in the
        <?xml declaration.
      +/
    Nullable!bool standalone;
}
