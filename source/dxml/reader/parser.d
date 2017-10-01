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

/// Flag for use with Config.
alias SplitEmpty = Flag!"SplitEmpty";


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

    /++
        Whether the parser should report empty element tags as if they were a
        start tag followed by an end tag with nothing in between.

        If $(D true), then whenever an $(D EntityType.elementEmpty) is
        encountered, the parser will claim that that entity is an
        $(D EntityType.elementStart), and then it will provide an
        $(D EntityType.elementEnd) as the next entity before the entity that
        actually follows it.

        The purpose of this is to simplify the code using the parser, since most
        code does not care about the difference between an empty tag and a start
        and end tag with nothing in between. But since some code will care about
        the difference, the behavior is configurable rather than simply always
        treating them as the same.
      +/
    auto splitEmpty = SplitEmpty.no;

    ///
    PositionType posType = PositionType.lineAndCol;
}


/++
    Helper function for creating a custom config. It makes it easy to set one
    or more of the member variables to something other than the default without
    having to worry about explicitly setting them individually or setting them
    all at once via a constructor.

    The order of the arguments does not matter. The types of each of the members
    of Config are unique, so that information alone is sufficient to determine
    which argument should be assigned to which member.
  +/
Config makeConfig(Args...)(Args args)
{
    Config config;

    foreach(arg; args)
    {
        foreach(memberName; __traits(allMembers, Config))
        {
            static if(is(typeof(__traits(getMember, config, memberName)) == typeof(arg)))
                mixin("config." ~ memberName ~ " = arg;");
        }
    }

    return config;
}

@safe pure nothrow @nogc unittest
{
    {
        auto config = makeConfig(SkipComments.yes);
        assert(config.skipComments == SkipComments.yes);
        assert(config.skipDTD == Config.init.skipDTD);
        assert(config.skipProlog == Config.init.skipProlog);
        assert(config.splitEmpty == Config.init.splitEmpty);
        assert(config.posType == Config.init.posType);
    }

    {
        auto config = makeConfig(SkipComments.yes, PositionType.none);
        assert(config.skipComments == SkipComments.yes);
        assert(config.skipDTD == Config.init.skipDTD);
        assert(config.skipProlog == Config.init.skipProlog);
        assert(config.splitEmpty == Config.init.splitEmpty);
        assert(config.posType == PositionType.none);
    }

    {
        auto config = makeConfig(SplitEmpty.yes, SkipComments.yes, PositionType.line);
        assert(config.skipComments == SkipComments.yes);
        assert(config.skipDTD == Config.init.skipDTD);
        assert(config.skipProlog == Config.init.skipProlog);
        assert(config.splitEmpty == SplitEmpty.yes);
        assert(config.posType == PositionType.line);
    }
}


/++
    This config is intended for making it easy to parse XML that does not
    contain some of the more advanced XML features such as DTDs. It skips
    everything that can be configured to skip.
  +/
enum simpleXML = makeConfig(SkipComments.yes, SkipDTD.yes, SkipProlog.yes, SplitEmpty.yes, PositionType.lineAndCol);

@safe pure nothrow @nogc unittest
{
    static assert(simpleXML.skipComments == SkipComments.yes);
    static assert(simpleXML.skipDTD == SkipDTD.yes);
    static assert(simpleXML.skipProlog == SkipProlog.yes);
    static assert(simpleXML.splitEmpty == SplitEmpty.yes);
    static assert(simpleXML.posType == PositionType.lineAndCol);
}


/++
    Lazily parses the given XML.

    The resulting EntityParser is similar to an input range with how it's
    iterated until it's consumed, and its state cannot be saved (since
    unfortunately, saving it would require allocating additional memory), but
    because there are essentially multiple ways to pop the front (e.g. choosing
    to skip all of contents between a start tag and end tag), the input range
    API didn't seem to appropriate, even if the result functions similarly.

    Also, unlike a range the, "front" is integrated into the EntityParser rather
    than being a value that can be extracted. So, while an entity can be queried
    while it is the "front", it can't be kept around after the EntityParser has
    moved to the next entity. Only the information that has been queried for
    that entity can be retained (e.g. its name, attributes, or textual value).
    While that does place some restrictions on how an algorithm operating on an
    EntityParser can operate, it does allow for more efficient processing.

    A range wrapper for an EntityParser could definitely be written, but it
    would be less efficient and less flexible, and if something like that is
    really needed, it may make more sense to simply use dxml.reader.dom. Some
    of the aspects of XML's design simply make it difficult to avoid allocating
    for each entity if an entity is treated as an element that can be returned
    by front and retained after a call to popFront.

    If invalid XML is encountered at any point during the parsing process, an
    $(D XMLParsingException) will be thrown.

    However, note that EntityParser does not care about XML validition beyond
    what is required to correctly parse what has been asked to parse. For
    instance, any portions that are skipped (either due to the values in the
    Config or because a function such as skipContents is called) will only be
    validated enough to correctly determine where those portions terminated.
    Similarly, if the functions to process the value of an entity are not
    called (e.g. $(D attributes) for $(D EntityType.elementStart) and
    $(D xmlSpec) for $(D EntityType.xmlSpec)), then those portions of the XML
    will not be validated beyond what is required to iterate to the next entity.

    A possible enhancement would be to add a validateXML function that
    corresponds to parseXML and fully validates the XML, but for now, no such
    function exists.
  +/
EntityParser parseXML(Config config, R)(R xmlText)
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
      +/
    elementStart,

    /++
        The end tag for an element. e.g. $(D </foo>).
      +/
    elementEnd,

    /++
        The tag for an element with no contents. e.g. $(D <foo name="value"/>).
      +/
    elementEmpty,

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
final class EntityParser(R)
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

        static assert(is(EntityParser!string.SliceOfR == string));

        auto range = filter!(a => true)("some xml");

        static assert(is(EntityParser!(typeof(range)).SliceOfR == typeof(takeExactly(range, 4))));
    }

    /++
        The type of the current entity.

        The value of this member determines which member functions and
        properties are allowed to be called, since some are only appropriate
        for specific entity types (e.g. $(D attributes) would not be appropriate
        for $(D EntityType.elementEnd)).
      +/
    EntityType type;

    /++
        The position of the current entity in the XML document.

        How accurate the position is depends on the parser configuration that's
        used.
      +/
    @property SourcePos pos()
    {
        assert(0);
    }

    /++
        Move to the next entity.

        The next entity is the next one that is linearly in the XML document.
        So, if the current entity has child entities, the next entity will be
        the child entity, whereas if it has no child entities, it will be the
        next entity at the same level.
      +/
    void next()
    {
        assert(0);
    }

    /++
        $(D true) if there is no more XML to process. It as en error to call
        $(D next) once $(D empty) is $(D true).
      +/
    void empty()
    {
        assert(0);
    }

    /++
        Gives the name of the current entity.

        Note that this is the direct name in the XML for this entity and does
        not contain any of the names of any of the parent entities that this
        entity has.

        $(TABLE,
          $(TR $(TH Supported $(D EntityType)s)),
          $(TR $(TD $(D EntityType.docType))),
          $(TR $(TD $(D EntityType.elementStart))),
          $(TR $(TD $(D EntityType.elementEnd))),
          $(TR $(TD $(D EntityType.elementEmpty))),
          $(TR $(TD $(D EntityType.processingInstruction))))
      +/
    @property SliceOfR name()
    {
        with(EntityType)
            assert(only(docType, elementStart, elementEnd, elementEmpty, processingInstruction).canFind(type));

        assert(0);
    }

    /++
        Returns a range of attributes for a start tag where each attribute is
        represented as a $(D Tuple!(SliceOfR, "name", SliceOfR, "value")).

        $(TABLE,
          $(TR $(TH Supported $(D EntityType)s)),
          $(TR $(TD $(D EntityType.elementStart))))
          $(TR $(TD $(D EntityType.elementEmpty))))
      +/
    @property auto attributes()
    {
        with(EntityType)
            assert(only(elementStart, elementEmpty).canFind(type));

        import std.typecons : Tuple;
        alias Attribute = Tuple!(SliceOfR, "name", SliceOfR, "value");
        assert(0);
    }

    /++
        Returns the value of the current entity.

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
        When at a start tag, moves the parser to the entity after the
        corresponding end tag tag and returns the contents between the two tags
        as text, leaving any markup in between as unprocessed text.

        $(TABLE,
          $(TR $(TH Supported $(D EntityType)s)),
          $(TR $(TD $(D EntityType.elementStart))))
      +/
    @property SliceOfR contentAsText()
    {
        with(EntityType)
            assert(type == elementStart);

        assert(0);
    }

    /++
        When at a start tag, moves the parser to the entity after the
        corresponding end tag.

        $(TABLE,
          $(TR $(TH Supported $(D EntityType)s)),
          $(TR $(TD $(D EntityType.elementStart))))
      +/
    void skipContents()
    {
        with(EntityType)
            assert(type == elementStart);

        assert(0);
    }

    /++
        Returns the $(D XMLDecl) corresponding to the current entity.

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
    EntityParser!EntityCompileTests foo;
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
