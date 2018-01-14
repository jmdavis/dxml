// Written in the D programming language

/++
    This implements a $(LINK2 https://en.wikipedia.org/wiki/StAX, StAX parser)
    for XML 1.0 (which will work with XML 1.1 documents assuming that they
    don't use any 1.1-specific features). For the sake of simplicity, sanity,
    and efficiency, the
    $(LINK2 https://en.wikipedia.org/wiki/Document_type_definition, DTD) section
    is not supported beyond what is required to parse past it. As such, the
    parser parses "well-formed" XML but is not what the
    $(LINK2 http://www.w3.org/TR/REC-xml, XML spec) calls a "validating parser."

    Start tags, end tags, comments, cdata sections, and processing instructions
    are all supported and reported to the application. Anything in the DTD is
    skipped (though it's parsed enough to parse past it correctly, and that
    $(I can) result in an $(LREF XMLParsingException) if that XML isn't valid
    enough to be correctly skipped), and the
    $(LINK2 http://www.w3.org/TR/REC-xml/#NT-XMLDecl, XML declaration) at the
    top is skipped if present (XML 1.1 requires that it be there, XML 1.0 does
    not).

    Regardless of what the XML declaration says (if present), any range of
    $(D char) will be treated as being encoded in UTF-8, any range of $(D wchar)
    will be treated as being encoded in UTF-16, and any range of $(D dchar) will
    be treated as having been encoded in UTF-32. Strings will be treated as
    ranges of their code units, not code points.

    Since the DTD section is skipped, all XML documents will be treated as
    $(LINK2 http://www.w3.org/TR/REC-xml/#sec-rmd, standalone) regardless of
    what the XML declaration says (so no references will be pulled in from the
    DTD or other XML documents, and no references will be replaced with
    whatever they refer to).

    $(LREF parseXML) is the function used to initiate the parsing of an XML
    document, and it returns an $(LREF EntityRange), which is a StAX parser.
    $(LREF Config) can be used to configure some of the parser's behavior (e.g.
    $(LREF SkipComments.yes) would tell it to not report comments to the
    application). $(LREF makeConfig) is a helper function to create custom
    $(LREF Config)s more easily, and $(LREF simpleXML) is a $(LREF Config) which
    provides simpler defaults than $(D $(LREF Config).init).

    Copyright: Copyright 2017 - 2018
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   Jonathan M Davis
    Source:    $(LINK_TO_SRC dxml/parser/_cursor.d)

    See_Also: $(LINK2 http://www.w3.org/TR/REC-xml/, Official Specification for XML 1.0)
  +/
module dxml.parser.stax;

import std.range.primitives;
import std.traits;
import std.typecons : Flag;

import dxml.parser.internal;


/++
    The exception type thrown when the XML parser runs into invalid XML.
  +/
class XMLParsingException : Exception
{
    /++
        The position in the XML input where the problem is.

        How informative it is depends on the $(LREF PositionType) used when
        parsing the XML.
      +/
    SourcePos pos;

package:

    this(string msg, SourcePos sourcePos, string file = __FILE__, size_t line = __LINE__)
    {
        import std.format : format;
        pos = sourcePos;
        if(pos.line != -1)
        {
            if(pos.col != -1)
                msg = format!"[%s:%s]: %s"(pos.line, pos.col, msg);
            else
                msg = format!"[Line %s]: %s"(pos.line, msg);
        }
        super(msg, file, line);
    }
}


/++
    Where in the XML text an entity is. If the $(LREF PositionType) when
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
    track of (it's used in $(LREF XMLParsingException) to indicate where the
    in the text the invalid XML was found). $(LREF _text.positionType.none) is the
    most efficient but least informative, whereas
    $(LREF _text.positionType.lineAndCol) is the most informative but least
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
    Used to configure how the parser works.

    See_Also:
        $(LREF makeConfig)$(BR)
        $(LREF parseXML)
  +/
struct Config
{
    /++
        Whether the comments should be skipped while parsing.

        If $(D true), any entities of type $(LREF EntityType.comment) will be
        omitted from the parsing results.

        Defaults to $(D SkipComments.no).
      +/
    auto skipComments = SkipComments.no;

    /++
        Whether processing instructions should be skipped.

        If $(D true), any entities with the type
        $(LREF EntityType.pi) will be skipped.

        Defaults to $(D SkipPI.no).
      +/
    auto skipPI = SkipPI.no;

    /++
        Whether the parser should report empty element tags as if they were a
        start tag followed by an end tag with nothing in between.

        If $(D true), then whenever an $(LREF EntityType.elementEmpty) is
        encountered, the parser will claim that that entity is an
        $(LREF EntityType.elementStart), and then it will provide an
        $(LREF EntityType.elementEnd) as the next entity before the entity that
        actually follows it.

        The purpose of this is to simplify the code using the parser, since most
        code does not care about the difference between an empty tag and a start
        and end tag with nothing in between. But since some code may care about
        the difference, the behavior is configurable rather than simply always
        treating them as the same.

        Defaults to $(D SplitEmpty.no).
      +/
    auto splitEmpty = SplitEmpty.no;

    ///
    unittest
    {
        enum configSplitYes = makeConfig(SplitEmpty.yes);

        {
            auto range = parseXML("<root></root>");
            assert(range.front.type == EntityType.elementStart);
            assert(range.front.name == "root");
            range.popFront();
            assert(range.front.type == EntityType.elementEnd);
            assert(range.front.name == "root");
            range.popFront();
            assert(range.empty);
        }
        {
            // No difference if the tags are already split.
            auto range = parseXML!configSplitYes("<root></root>");
            assert(range.front.type == EntityType.elementStart);
            assert(range.front.name == "root");
            range.popFront();
            assert(range.front.type == EntityType.elementEnd);
            assert(range.front.name == "root");
            range.popFront();
            assert(range.empty);
        }
        {
            // This treats <root></root> and <root/> as distinct.
            auto range = parseXML("<root/>");
            assert(range.front.type == EntityType.elementEmpty);
            assert(range.front.name == "root");
            range.popFront();
            assert(range.empty);
        }
        {
            // This is parsed as if it were <root></root> insead of <root/>.
            auto range = parseXML!configSplitYes("<root/>");
            assert(range.front.type == EntityType.elementStart);
            assert(range.front.name == "root");
            range.popFront();
            assert(range.front.type == EntityType.elementEnd);
            assert(range.front.name == "root");
            range.popFront();
            assert(range.empty);
        }
    }

    /++
        This affects how precise the position information is in
        $(LREF XMLParsingException)s that get thrown when invalid XML is
        encountered while parsing.

        Defaults to $(LREF PositionType.lineAndCol).

        See_Also: $(LREF PositionType)$(BR)$(LREF SourcePos)
      +/
    PositionType posType = PositionType.lineAndCol;
}


/// See_Also: $(LREF2 skipComments, Config)
alias SkipComments = Flag!"SkipComments";

/// See_Also: $(LREF2 skipPI, Config)
alias SkipPI = Flag!"SkipPI";

/// See_Also: $(LREF2 splitEmpty, Config)
alias SplitEmpty = Flag!"SplitEmpty";


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
    import std.format : format;
    import std.meta : AliasSeq, staticIndexOf, staticMap;

    template isValid(T, Types...)
    {
        static if(Types.length == 0)
            enum isValid = false;
        else static if(is(T == Types[0]))
            enum isValid = true;
        else
            enum isValid = isValid!(T, Types[1 .. $]);
    }

    Config config;

    alias TypeOfMember(string memberName) = typeof(__traits(getMember, config, memberName));
    alias MemberTypes = staticMap!(TypeOfMember, AliasSeq!(__traits(allMembers, Config)));

    foreach(i, arg; args)
    {
        static assert(isValid!(typeof(arg), MemberTypes),
                      format!"Argument %s does not match the type of any members of Config"(i));

        static foreach(j, Other; Args)
        {
            static if(i != j)
                static assert(!is(typeof(arg) == Other), format!"Argument %s and %s have the same type"(i, j));
        }

        foreach(memberName; __traits(allMembers, Config))
        {
            static if(is(typeof(__traits(getMember, config, memberName)) == typeof(arg)))
                mixin("config." ~ memberName ~ " = arg;");
        }
    }

    return config;
}

///
@safe pure nothrow @nogc unittest
{
    {
        auto config = makeConfig(SkipComments.yes);
        assert(config.skipComments == SkipComments.yes);
        assert(config.skipPI == Config.init.skipPI);
        assert(config.splitEmpty == Config.init.splitEmpty);
        assert(config.posType == Config.init.posType);
    }
    {
        auto config = makeConfig(SkipComments.yes, PositionType.none);
        assert(config.skipComments == SkipComments.yes);
        assert(config.skipPI == Config.init.skipPI);
        assert(config.splitEmpty == Config.init.splitEmpty);
        assert(config.posType == PositionType.none);
    }
    {
        auto config = makeConfig(SplitEmpty.yes,
                                 SkipComments.yes,
                                 PositionType.line);
        assert(config.skipComments == SkipComments.yes);
        assert(config.splitEmpty == SplitEmpty.yes);
        assert(config.posType == PositionType.line);
    }
}

unittest
{
    import std.typecons : Flag;
    static assert(!__traits(compiles, makeConfig(42)));
    static assert(!__traits(compiles, makeConfig("hello")));
    static assert(!__traits(compiles, makeConfig(Flag!"SomeOtherFlag".yes)));
    static assert(!__traits(compiles, makeConfig(SplitEmpty.yes, SplitEmpty.no)));
}


/++
    This config is intended for making it easy to parse XML that does not
    contain some of the more advanced XML features such as DTDs. It skips
    everything that can be configured to skip.
  +/
enum simpleXML = makeConfig(SkipComments.yes, SkipPI.yes, SplitEmpty.yes, PositionType.lineAndCol);

///
@safe pure nothrow @nogc unittest
{
    static assert(simpleXML.skipComments == SkipComments.yes);
    static assert(simpleXML.skipPI == SkipPI.yes);
    static assert(simpleXML.splitEmpty == SplitEmpty.yes);
    static assert(simpleXML.posType == PositionType.lineAndCol);
}


/++
    Represents the type of an XML entity. Used by EntityRange.
  +/
enum EntityType
{
    /++
        A cdata section: `<![CDATA[ ... ]]>`.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#sec-cdata-sect)
      +/
    cdata,

    /++
        An XML comment: `<!-- ... -->`.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#sec-comments)
      +/
    comment,

    /++
        The start tag for an element. e.g. `<foo name="value">`.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#sec-starttags)
      +/
    elementStart,

    /++
        The end tag for an element. e.g. `</foo>`.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#sec-starttags)
      +/
    elementEnd,

    /++
        The tag for an element with no contents. e.g. `<foo name="value"/>`.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#sec-starttags)
      +/
    elementEmpty,

    /++
        A processing instruction such as `<?foo?>`. Note that the
        `<?xml ... ?>` is skipped and not treated as an $(LREF EntityType.pi).

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#sec-pi)
      +/
    pi,

    /++
        The content of an element tag that is simple text.

        If there is an entity other than the end tag following the text, then
        the text includes up to that entity.

        Note however that character references (e.g. $(D "&#42")) and entity
        references (e.g. $(D "&Name;")) are left unprocessed in the text
        (primarily because the most user-friendly thing to do with
        them is to convert them in place, and that's best to a higher level
        parser or helper function).

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#sec-starttags)
      +/
    text,
}


/++
    Lazily parses the given XML.

    EntityRange is essentially a
    $(LINK2 https://en.wikipedia.org/wiki/StAX, StAX) parser, though it evolved
    into that rather than being based on what Java did, and it's range-based
    rather than iterator-based, so its API is likely to differ from other
    implementations. The basic concept should be the same though.

    One of the core design goals of this parser is to slice the original input
    rather than having to allocate strings for the output or wrap it in a range
    that produces a mutated version of the data. So, all of the text that the
    parser provides is either a slice or $(PHOBOS_REF takeExactly, std, range)
    of the input. However, in some cases, for the parser to be fully compliant,
    $(LREF normalize) must be called on the text to mutate certain constructs
    (e.g. removing any $(D '\r') in the text or converting $(D "&lt") to
    $(D '<')). But that's left up to the application.

    The parser is not @nogc, but it allocates memory very minimally. It
    allocates some of its state on the heap so that it can retain a stack of
    tag names so that end tags can be validated, but that stack is shared among
    all ranges that came from the same call to parseXML (only the range
    farthest along in parsing validates the end tags), so $(LREF save) does not
    allocate memory. The stack itself is currently implemented using a dynamic
    array and may be reallocated a few times as the stack grows to its maximum,
    but for most documents, it probably won't need to reallocate more than once
    or twice, if that. The only other time that the parser would allocate would
    be if an exception were thrown.

    If invalid XML is encountered at any point during the parsing process, an
    $(LREF XMLParsingException) will be thrown. If an exception has been thrown,
    then the parser is in an invalid state, and it is an error to call any
    functions on it.

    However, note that EntityRange does not generally care about XML validation
    beyond what is required to correctly parse what it has been told to parse.
    In particular, any portions that are skipped (either due to the values in
    its $(LREF Config) or because a function such as $(LREF skipContents) is
    called) will only be validated enough to correctly determine where those
    portions terminated. Similarly, if the functions to process the value of an
    entity are not called (e.g. $(LREF2 attributes, _EntityRange) for
    $(LREF EntityType.elementStart), then those portions of the XML will not be
    validated beyond what is required to iterate to the next entity.

    And as the module documentation says, this parser does not provide any DTD
    support. It's not possible to properly support the DTD section while
    returning slices of the original input, and the DTD portion of the spec
    makes parsing XML exponentially more complicated. Support for that may or
    may not be added in the future with a separate parser (possibly one that
    parses the DTD section to create an object to then validate an EntityRange),
    but for now at least, dxml isn't getting involved in that mess.

    See_Also: $(MREF dxml, parser, dom)
  +/
struct EntityRange(Config cfg, R)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
public:

    import std.algorithm : canFind;
    import std.range : only, takeExactly;
    import std.typecons : Nullable;
    import std.utf : byCodeUnit;

    private enum compileInTests = is(R == EntityCompileTests);

    /// The Config used for when parsing the XML.
    alias config = cfg;

    /++
        The type used when any slice of the original input is used. If $(D R)
        is a string or supports slicing, then SliceOfR is the same as $(D R);
        otherwise, it's the result of calling
        $(PHOBOS_REF takeExactly, std, range) on the input.
      +/
    static if(isDynamicArray!R || hasSlicing!R)
        alias SliceOfR = R;
    else
        alias SliceOfR = typeof(takeExactly(R.init, 42));

    // TODO re-enable these. Whenever EntityRange doesn't compile correctly due
    // to a change, these static assertions fail, and the actual errors are masked.
    // So, rather than continuing to deal with that whenever I change somethnig,
    // I'm leaving these commented out for now.
    // Also, unfortunately, https://issues.dlang.org/show_bug.cgi?id=11133
    // currently prevents this from showing up in the docs, and I don't know of
    // a workaround that works.
    /+
    ///
    static if(compileInTests) @safe unittest
    {
        import std.algorithm : filter;
        import std.range : takeExactly;

        static assert(is(EntityRange!(Config.init, string).SliceOfR == string));

        auto range = filter!(a => true)("some xml");

        static assert(is(EntityRange!(Config.init, typeof(range)).SliceOfR ==
                         typeof(takeExactly(range, 4))));
    }
    +/


    /++
        Represents an entity in the XML document.

        Note that the $(LREF2 type, EntityRange.Entity.type) determines which
        properties can be used, and it can determine whether functions which
        an Entity or EntityRange is passed to are allowed to be called. Each
        function lists which $(LREF EntityType)s are allowed, and it is an error
        to call them with any other $(LREF EntityType).
      +/
    struct Entity
    {
    public:

        /++
            The $(LREF EntityType) for this Entity.
          +/
        @property EntityType type() @safe const pure nothrow @nogc
        {
            return _type;
        }


        /++
            Gives the name of this Entity.

            Note that this is the direct name in the XML for this entity and
            does not contain any of the names of any of the parent entities that
            this entity has.

            $(TABLE
                $(TR $(TH Supported $(LREF EntityType)s:))
                $(TR $(TD $(LREF2 elementStart, EntityType)))
                $(TR $(TD $(LREF2 elementEnd, EntityType)))
                $(TR $(TD $(LREF2 elementEmpty, EntityType)))
                $(TR $(TD $(LREF2 pi, EntityType)))
            )
          +/
        @property SliceOfR name()
        {
            with(EntityType)
                assert(only(elementStart, elementEnd, elementEmpty, pi).canFind(_type));
            return stripBCU!R(_name.save);
        }


        /++
            Returns a lazy range of attributes for a start tag where each
            attribute is represented as a
            $(D Tuple!($(LREF2 SliceOfR, EntityRange), "name", $(LREF2 SliceOfR, EntityRange), "value")).

            $(TABLE
                $(TR $(TH Supported $(LREF EntityType)s:))
                $(TR $(TD $(LREF2 elementStart, EntityType)))
                $(TR $(TD $(LREF2 elementEmpty, EntityType)))
            )

            Throws: $(LREF XMLParsingException) on invalid XML.
          +/
        @property auto attributes()
        {
            with(EntityType)
                assert(_type == elementStart || _type == elementEmpty);

            // STag         ::= '<' Name (S Attribute)* S? '>'
            // Attribute    ::= Name Eq AttValue
            // EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'

            import std.typecons : Tuple;
            alias Attribute = Tuple!(SliceOfR, "name", SliceOfR, "value");

            static struct AttributeRange
            {
                @property Attribute front()
                {
                    return _front;
                }

                void popFront()
                {
                    immutable wasWS = stripWS(_text);
                    if(_text.input.empty)
                    {
                        empty = true;
                        return;
                    }
                    if(!wasWS)
                        throw new XMLParsingException("Whitespace missing before attribute name", _text.pos);

                    auto name = stripBCU!R(_text.takeName!'='());
                    stripWS(_text);

                    checkNotEmpty(_text);
                    if(_text.input.front != '=')
                        throw new XMLParsingException("= missing", _text.pos);
                    popFrontAndIncCol(_text);

                    stripWS(_text);
                    _front = Attribute(name, stripBCU!R(takeEnquotedText(_text)));
                }

                @property auto save()
                {
                    auto retval = this;
                    retval._front = Attribute(_front[0].save, _front[1].save);
                    retval._text.input = retval._text.input.save;
                    return retval;
                }

                this(typeof(_text) text)
                {
                    _front = Attribute.init; // This is utterly stupid. https://issues.dlang.org/show_bug.cgi?id=13945
                    _text = text;
                    if(_text.input.empty)
                        empty = true;
                    else
                        popFront();
                }

                bool empty;
                Attribute _front;
                typeof(_savedText) _text;
            }

            return AttributeRange(_savedText);
        }

        ///
        static if(compileInTests) unittest
        {
            import std.algorithm.comparison : equal;
            import std.algorithm.iteration : filter;
            {
                auto xml = "<root/>";
                auto range = parseXML(xml);
                assert(range.front.type == EntityType.elementEmpty);
                assert(range.front.attributes.empty);
            }
            {
                auto xml = "<root a='42' q='29' w='hello'/>";
                auto range = parseXML(xml);
                assert(range.front.type == EntityType.elementEmpty);
                auto attrs = range.front.attributes;
                assert(attrs.front.name == "a");
                assert(attrs.front.value == "42");
                attrs.popFront();
                assert(attrs.front.name == "q");
                assert(attrs.front.value == "29");
                attrs.popFront();
                assert(attrs.front.name == "w");
                assert(attrs.front.value == "hello");
                attrs.popFront();
                assert(attrs.empty);
            }
            // Because the type of name and value is SliceOfR, == with a string
            // only works if the range passed to parseXML was string.
            {
                auto xml = filter!(a => true)("<root a='42' q='29' w='hello'/>");
                auto range = parseXML(xml);
                assert(range.front.type == EntityType.elementEmpty);
                auto attrs = range.front.attributes;
                assert(equal(attrs.front.name, "a"));
                assert(equal(attrs.front.value, "42"));
                attrs.popFront();
                assert(equal(attrs.front.name, "q"));
                assert(equal(attrs.front.value, "29"));
                attrs.popFront();
                assert(equal(attrs.front.name, "w"));
                assert(equal(attrs.front.value, "hello"));
                attrs.popFront();
                assert(attrs.empty);
            }
        }

        static if(compileInTests) unittest
        {
            import core.exception : AssertError;
            import std.algorithm.comparison : equal;
            import std.exception : assertNotThrown, assertThrown, enforce;
            import std.typecons : Tuple, tuple;

            static void test(alias func)(string text, EntityType type, Tuple!(string, string)[] expected,
                                         int row, int col, size_t line = __LINE__)
            {
                auto xml = func(text);
                static foreach(i, config; testConfigs)
                {{
                    auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                    auto range = assertNotThrown!XMLParsingException(parseXML!config(xml.save));
                    enforce!AssertError(range.front.type == type, "unittest failure 1", __FILE__, line);
                    enforce!AssertError(equal!((a, b) => equal(a[0], b[0]) && equal(a[1], b[1]))
                                              (range.front.attributes, expected), "unittest failure 2", __FILE__, line);
                    enforce!AssertError(range._text.pos == pos, "unittest failure 3", __FILE__, line);
                }}
            }

            static void testFail(alias func)(string text, size_t line = __LINE__)
            {
                auto xml = func(text);
                static foreach(i, config; testConfigs)
                {{
                    auto range = parseXML!config(xml.save);
                    assertThrown!XMLParsingException(walkLength(range.front.attributes),
                                                     "unittest failure", __FILE__, line);
                }}
            }

            static foreach(func; testRangeFuncs)
            {
                test!func("<root a='b'/>", EntityType.elementEmpty, [tuple("a", "b")], 1, 14);
                test!func("<root a = 'b' />", EntityType.elementEmpty, [tuple("a", "b")], 1, 17);
                test!func("<root \n\n a \n\n = \n\n 'b' \n\n />", EntityType.elementEmpty, [tuple("a", "b")], 9, 4);
                test!func("<root a='b'></root>", EntityType.elementStart, [tuple("a", "b")], 1, 13);
                test!func("<root a = 'b' ></root>", EntityType.elementStart, [tuple("a", "b")], 1, 16);
                test!func("<root \n a \n = \n 'b' \n ></root>", EntityType.elementStart, [tuple("a", "b")], 5, 3);

                test!func("<root foo='\n\n\n'/>", EntityType.elementEmpty, [tuple("foo", "\n\n\n")], 4, 4);
                test!func(`<root foo='"""'/>`, EntityType.elementEmpty, [tuple("foo", `"""`)], 1, 18);
                test!func(`<root foo="'''"/>`, EntityType.elementEmpty, [tuple("foo", `'''`)], 1, 18);
                test!func(`<root foo.=""/>`, EntityType.elementEmpty, [tuple("foo.", "")], 1, 16);

                test!func("<root foo='bar' a='b' hello='world'/>", EntityType.elementEmpty,
                          [tuple("foo", "bar"), tuple("a", "b"), tuple("hello", "world")], 1, 38);
                test!func(`<root foo="bar" a='b' hello="world"/>`, EntityType.elementEmpty,
                          [tuple("foo", "bar"), tuple("a", "b"), tuple("hello", "world")], 1, 38);

                testFail!func(`<root a=""">`);
                testFail!func(`<root a='''>`);
                testFail!func("<root a=>");
                testFail!func("<root a='>");
                testFail!func("<root a='>");
                testFail!func("<root =''>");
                testFail!func(`<root a "">`);
                testFail!func(`<root a"">`);
                testFail!func(`<root a>`);
                testFail!func("<root foo='bar' a=>");
                testFail!func("<root foo='bar' a='>");
                testFail!func("<root foo='bar' a='>");
                testFail!func("<root foo='bar' =''>");
                testFail!func("<root foo='bar' a= hello='world'>");
                testFail!func("<root foo='bar' a=' hello='world'>");
                testFail!func("<root foo='bar' a=' hello='world'>");
                testFail!func("<root foo='bar' ='' hello='world'>");
                testFail!func("<root foo='bar'a='b'>");
                testFail!func(`<root .foo="bar">`);

                // FIXME The contents of the attribute value aren't currently
                // validated at all. Validation does get a bit involved though
                // thanks to the fact that & is legal if the attribute is a
                // reference and illegal otherwise, and full validation would
                // also involve validating that the reference is well-formed
                // as well.
            }
        }


        /++
            Returns the textual value of this Entity.

            In the case of $(LREF EntityType.pi), this is the
            text that follows the name, whereas in the other cases, the text is
            the entire contents of the entity (save for the delimeters on the
            ends if that entity has them).

            $(TABLE
                $(TR $(TH Supported $(LREF EntityType)s:))
                $(TR $(TD $(LREF2 cdata, EntityType)))
                $(TR $(TD $(LREF2 comment, EntityType)))
                $(TR $(TD $(LREF2 pi, EntityType)))
                $(TR $(TD $(LREF2 _text, EntityType)))
            )
          +/
        @property SliceOfR text()
        {
            with(EntityType)
                assert(only(cdata, comment, pi, text).canFind(_type));
            return stripBCU!R(_savedText.input.save);
        }

        ///
        static if(compileInTests) unittest
        {
            auto xml = "<?xml version='1.0'?>\n" ~
                       "<?instructionName?>\n" ~
                       "<?foo here is something to say?>\n" ~
                       "<root>\n" ~
                       "    <![CDATA[ Yay! random text >> << ]]>\n" ~
                       "    <!-- some random comment -->\n" ~
                       "    <p>something here</p>\n" ~
                       "    <p>\n" ~
                       "       something else\n" ~
                       "       here</p>\n" ~
                       "</root>";
            auto range = parseXML(xml);

            // "<?instructionName?>\n" ~
            assert(range.front.type == EntityType.pi);
            assert(range.front.name == "instructionName");
            assert(range.front.text.empty);

            // "<?foo here is something to say?>\n" ~
            range.popFront();
            assert(range.front.type == EntityType.pi);
            assert(range.front.name == "foo");
            assert(range.front.text == "here is something to say");

            // "<root>\n" ~
            range.popFront();
            assert(range.front.type == EntityType.elementStart);

            // "    <![CDATA[ Yay! random text >> << ]]>\n" ~
            range.popFront();
            assert(range.front.type == EntityType.cdata);
            assert(range.front.text == " Yay! random text >> << ");

            // "    <!-- some random comment -->\n" ~
            range.popFront();
            assert(range.front.type == EntityType.comment);
            assert(range.front.text == " some random comment ");

            // "    <p>something here</p>\n" ~
            range.popFront();
            assert(range.front.type == EntityType.elementStart);
            assert(range.front.name == "p");
            range.popFront();
            assert(range.front.type == EntityType.text);
            assert(range.front.text == "something here");
            range.popFront();
            assert(range.front.type == EntityType.elementEnd);
            assert(range.front.name == "p");

            // "    <p>\n" ~
            // "       something else\n" ~
            // "       here</p>\n" ~
            range.popFront();
            assert(range.front.type == EntityType.elementStart);
            range.popFront();
            assert(range.front.type == EntityType.text);
            assert(range.front.text == "\n       something else\n       here");
            range.popFront();
            assert(range.front.type == EntityType.elementEnd);

            // "</root>"
            range.popFront();
            assert(range.front.type == EntityType.elementEnd);
            range.popFront();
            assert(range.empty);
        }


    private:

        this(EntityType type)
        {
            _type = type;

            // None of these initializations should be required. https://issues.dlang.org/show_bug.cgi?id=13945
            _name = typeof(_name).init;
            _savedText = typeof(_savedText).init;
        }

        EntityType _type;
        Taken _name;
        typeof(EntityRange._savedText) _savedText;
    }


    /++
        Returns the $(LREF Entity) representing the entity in the XML document
        which was most recently parsed.
      +/
    @property Entity front()
    {
        auto retval = Entity(_type);
        with(EntityType) final switch(_type)
        {
            case cdata: retval._savedText = _savedText.save; break;
            case comment: goto case cdata;
            case elementStart: retval._name = _name.save; retval._savedText = _savedText.save; break;
            case elementEnd: retval._name = _name.save; break;
            case elementEmpty: goto case elementStart;
            case text: goto case cdata;
            case pi: goto case elementStart;
        }
        return retval;
    }


    /++
        Move to the next entity.

        The next entity is the next one that is linearly in the XML document.
        So, if the current entity has child entities, the next entity will be
        the child entity, whereas if it has no child entities, it will be the
        next entity at the same level.

        Throws: $(LREF XMLParsingException) on invalid XML.
      +/
    void popFront()
    {
        final switch(_grammarPos) with(GrammarPos)
        {
            case documentStart: _parseDocumentStart(); break;
            case prologMisc1: _parseAtPrologMisc!1(); break;
            case prologMisc2: _parseAtPrologMisc!2(); break;
            case splittingEmpty:
            {
                _type = EntityType.elementEnd;
                _grammarPos = _tagStack.depth == 0 ? GrammarPos.endMisc : GrammarPos.contentCharData2;
                break;
            }
            case contentCharData1:
            {
                assert(_type == EntityType.elementStart);
                _tagStack.push(_name.save);
                _parseAtContentCharData();
                break;
            }
            case contentMid: _parseAtContentMid(); break;
            case contentCharData2: _parseAtContentCharData(); break;
            case endTag: _parseElementEnd(); break;
            case endMisc: _parseAtEndMisc(); break;
            case documentEnd:
                assert(0, "It's illegal to call popFront() on an empty EntityRange.");
        }
    }


    /++
        Whether the end of the XML document has been reached.

        Note that because an $(LREF XMLParsingException) will be thrown an
        invalid XML, it's actually possible to call
        $(LREF2 front, EntityRange.front) and
        $(LREF2 popFront, EntityRange.popFront) without checking empty if the
        only way that empty would be $(D true) is if the XML were invalid (e.g.
        if at a start tag, it's a given that there's at least one end tag left
        in the document unless it's invalid XML).

        However, of course, caution should be used to ensure that incorrect
        assumptions are not made that allow the document to reach its end
        earlier than predicted without throwing an $(LREF XMLParsingException),
        since it's still illegal to call $(LREF2 front, EntityRange.front) or
        $(LREF2 popFront, EntityRange.popFront) if empty would return
        $(D false).
      +/
    @property bool empty() @safe const pure nothrow @nogc
    {
        return _grammarPos == GrammarPos.documentEnd;
    }


    /++
        Forward range function for obtaining a copy of the range which can then
        be iterated independently of the original.
      +/
    @property auto save()
    {
        return this;
    }


    /++
        Returns an empty range. This corresponds to
        $(PHOBOS_REF takeNone, std, range) except that it doesn't create a
        wrapper type.
      +/
    auto takeNone()
    {
        auto retval = save;
        retval._grammarPos = GrammarPos.documentEnd;
        return retval;
    }


    /++
        When $(LREF2 front, EntityRange.front) is at a start tag, this can be
        used instead of $(LREF2 popFront, EntityRange.popFront) to parse over
        the entities between the start tag and its corresponding end tag and
        return the content between them as text, leaving any markup in between
        as unprocessed text.

        $(TABLE
            $(TR $(TH Supported $(LREF EntityType)s:))
            $(TR $(TD $(LREF2 elementStart, EntityType)))
        )

        Throws: $(LREF XMLParsingException) on invalid XML.
      +/
    @property SliceOfR contentAsText()
    {
        assert(_type == EntityType.elementStart);

        assert(0);
    }


private:

    void _parseDocumentStart()
    {
        auto orig = _text.save;
        if(_text.stripStartsWith("<?xml"))
        {
            checkNotEmpty(_text);
            if(_text.input.front == '?' || isSpace(_text.input.front))
                _text.skipUntilAndDrop!"?>"();
            else
                _text = orig;
        }
        _grammarPos = GrammarPos.prologMisc1;
        _parseAtPrologMisc!1();
    }

    static if(compileInTests) unittest
    {
        import core.exception : AssertError;
        import std.exception : assertNotThrown, enforce;
        import std.range : iota, lockstep, only;

        static void test(alias func)(string xml, int row, int col, size_t line = __LINE__)
        {
            static foreach(i, config; testConfigs)
            {{
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto range = assertNotThrown!XMLParsingException(parseXML!config(func(xml)));
                enforce!AssertError(range._type == EntityType.elementEmpty, "unittest failure 1", __FILE__, line);
                enforce!AssertError(range._text.pos == pos, "unittest failure 2", __FILE__, line);
            }}
        }

        static foreach(func; testRangeFuncs)
        {
            test!func("<root/>", 1, 8);
            test!func("\n\t\n <root/>   \n", 3, 9);
            test!func("<?xml\n\n\nversion='1.8'\n\n\n\nencoding='UTF-8'\n\n\nstandalone='yes'\n?><root/>", 12, 10);
            test!func("<?xml\n\n\n    \r\r\r\n\nversion='1.8'?><root/>", 6, 23);
            test!func("<?xml\n\n\n    \r\r\r\n\nversion='1.8'?>\n     <root/>", 7, 13);
            test!func("<root/>", 1, 8);
            test!func("\n\t\n <root/>   \n", 3, 9);

            // FIXME add some cases where the document starts with commenst or PIs.
        }
    }


    // Parse at GrammarPos.prologMisc1 or GrammarPos.prologMisc2.
    void _parseAtPrologMisc(int miscNum)()
    {
        static assert(miscNum == 1 || miscNum == 2);

        // document ::= prolog element Misc*
        // prolog   ::= XMLDecl? Misc* (doctypedecl Misc*)?
        // Misc ::= Comment | PI | S

        stripWS(_text);
        checkNotEmpty(_text);
        if(_text.input.front != '<')
            throw new XMLParsingException("Expected <", _text.pos);
        popFrontAndIncCol(_text);
        checkNotEmpty(_text);

        switch(_text.input.front)
        {
            // Comment     ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
            // doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
            case '!':
            {
                popFrontAndIncCol(_text);
                if(_text.stripStartsWith("--"))
                {
                    _parseComment();
                    break;
                }
                static if(miscNum == 1)
                {
                    if(_text.stripStartsWith("DOCTYPE"))
                    {
                        if(!_text.stripWS())
                            throw new XMLParsingException("Whitespace must follow <!DOCTYPE", _text.pos);
                        _parseDoctypeDecl();
                        break;
                    }
                    throw new XMLParsingException("Expected DOCTYPE or --", _text.pos);
                }
                else
                {
                    if(_text.stripStartsWith("DOCTYPE"))
                    {
                        throw new XMLParsingException("Only one <!DOCTYPE ...> declaration allowed per XML document",
                                                      _text.pos);
                    }
                    throw new XMLParsingException("Expected --", _text.pos);
                }
            }
            // PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
            case '?':
            {
                _parsePI();
                static if(config.skipPI == SkipPI.yes)
                    popFront();
                break;
            }
            // element ::= EmptyElemTag | STag content ETag
            default:
            {
                _parseElementStart();
                break;
            }
        }
    }


    // Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
    // Parses a comment. <!-- was already removed from the front of the input.
    void _parseComment()
    {
        static if(config.skipComments == SkipComments.yes)
            _text.skipUntilAndDrop!"--"();
        else
        {
            _type = EntityType.comment;
            _savedText.pos = _text.pos;
            _savedText.input = _text.takeUntilAndDrop!"--"();
        }
        if(_text.input.empty || _text.input.front != '>')
            throw new XMLParsingException("Comments cannot contain -- and cannot be terminated by --->", _text.pos);
        popFrontAndIncCol(_text);
    }

    static if(compileInTests) unittest
    {
        import core.exception : AssertError;
        import std.algorithm.comparison : equal;
        import std.exception : assertNotThrown, assertThrown, enforce;

        static void test(alias func)(string text, string comment, int row, int col, size_t line = __LINE__)
        {
            auto xml = func(text ~ "<root/>");
            static foreach(i, config; testConfigs)
            {{
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto range = assertNotThrown!XMLParsingException(parseXML!config(xml.save));
                enforce!AssertError(range.front.type == EntityType.comment, "unittest failure 1", __FILE__, line);
                enforce!AssertError(equal(range.front.text, comment), "unittest failure 2", __FILE__, line);
                enforce!AssertError(range._text.pos == pos, "unittest failure 3", __FILE__, line);
            }}
        }

        static void testFail(alias func)(string text, size_t line = __LINE__)
        {
            auto xml = func(text ~ "<root/>");
            static foreach(i, config; testConfigs)
                assertThrown!XMLParsingException(parseXML!config(xml.save), "unittest failure", __FILE__, line);
        }

        static foreach(func; testRangeFuncs)
        {
            test!func("<!--foo-->", "foo", 1, 11);
            test!func("<!-- foo -->", " foo ", 1, 13);
            test!func("<!-- -->", " ", 1, 9);
            test!func("<!---->", "", 1, 8);
            test!func("<!--- comment -->", "- comment ", 1, 18);
            test!func("<!-- \n foo \n -->", " \n foo \n ", 3, 5);

            testFail!func("<!- comment -->");
            testFail!func("<!-- comment ->");
            testFail!func("<!-- comment --->");
            testFail!func("<!---- comment -->");
            testFail!func("<!-- comment -- comment -->");
            testFail!func("<!->");
            testFail!func("<!-->");
            testFail!func("<!--->");
            testFail!func("<!----->");
            testFail!func("<!blah>");
            testFail!func("<! blah>");

            {
                auto xml = func("<!DOCTYPE foo><!-- comment --><root/>");
                auto range = assertNotThrown!XMLParsingException(parseXML(xml));
                assert(range.front.type == EntityType.comment);
                assert(equal(range.front.text, " comment "));
            }
            {
                auto xml = func("<root><!-- comment --></root>");
                auto range = assertNotThrown!XMLParsingException(parseXML(xml));
                assertNotThrown!XMLParsingException(range.popFront());
                assert(range.front.type == EntityType.comment);
                assert(equal(range.front.text, " comment "));
            }
            {
                auto xml = func("<root/><!-- comment -->");
                auto range = assertNotThrown!XMLParsingException(parseXML(xml));
                assertNotThrown!XMLParsingException(range.popFront());
                assert(range.front.type == EntityType.comment);
                assert(equal(range.front.text, " comment "));
            }

            static foreach(comment; ["<!foo>", "<! foo>", "<!->", "<!-->", "<!--->"])
            {
                {
                    auto xml = func("<!DOCTYPE foo>" ~ comment ~ "<root/>");
                    assertThrown!XMLParsingException(parseXML(xml));
                }
                {
                    auto xml = func("<root>" ~ comment ~ "<root>");
                    auto range = assertNotThrown!XMLParsingException(parseXML(xml));
                    assertThrown!XMLParsingException(range.popFront());
                }
                {
                    auto xml = func("<root/>" ~ comment);
                    auto range = assertNotThrown!XMLParsingException(parseXML(xml));
                    assertThrown!XMLParsingException(range.popFront());
                }
            }
        }
    }


    // PI       ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
    // PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
    // Parses a processing instruction. < was already removed from the input.
    void _parsePI()
    {
        assert(_text.input.front == '?');
        popFrontAndIncCol(_text);
        static if(config.skipPI == SkipPI.yes)
            _text.skipUntilAndDrop!"?>"();
        else
        {
            immutable posAtName = _text.pos;
            _type = EntityType.pi;
            _name = takeName!'?'(_text);
            if(stripWS(_text))
            {
                checkNotEmpty(_text);
                if(_text.input.front == '?')
                {
                    throw new XMLParsingException("There cannot be whitespace after the name if there is no text " ~
                                                  "before the terminating ?>", _text.pos);
                }
            }
            else
                checkNotEmpty(_text);
            _savedText.pos = _text.pos;
            _savedText.input = _text.takeUntilAndDrop!"?>"();
            if(walkLength(_name.save) == 3)
            {
                // FIXME icmp doesn't compile right now due to an issue with
                // byUTF that needs to be looked into.
                /+
                import std.uni : icmp;
                if(icmp(_name.save, "xml") == 0)
                    throw new XMLParsingException("Processing instructions cannot be named xml", posAtName);
                +/
                auto temp = _name.save;
                if(temp.front == 'x' || temp.front == 'X')
                {
                    temp.popFront();
                    if(temp.front == 'm' || temp.front == 'M')
                    {
                        temp.popFront();
                        if(temp.front == 'l' || temp.front == 'L')
                            throw new XMLParsingException("Processing instructions cannot be named xml", posAtName);
                    }
                }
            }
        }
    }

    static if(compileInTests) unittest
    {
        import core.exception : AssertError;
        import std.algorithm.comparison : equal;
        import std.exception : assertNotThrown, assertThrown, enforce;

        static void test(alias func)(string text, string name, string expected,
                                     int row, int col, size_t line = __LINE__)
        {
            auto xml = func(text ~ "<root/>");
            static foreach(i, config; testConfigs)
            {{
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto range = assertNotThrown!XMLParsingException(parseXML!config(xml.save));
                enforce!AssertError(range.front.type == EntityType.pi, "unittest failure 1", __FILE__, line);
                enforce!AssertError(equal(range.front.name, name), "unittest failure 2", __FILE__, line);
                enforce!AssertError(equal(range.front.text, expected), "unittest failure 3", __FILE__, line);
                enforce!AssertError(range._text.pos == pos, "unittest failure 4", __FILE__, line);
            }}
        }

        static void testFail(alias func)(string text, size_t line = __LINE__)
        {
            auto xml = func(text ~ "<root/>");
            static foreach(i, config; testConfigs)
                assertThrown!XMLParsingException(parseXML!config(xml.save), "unittest failure", __FILE__, line);
        }

        static foreach(func; testRangeFuncs)
        {
            test!func("<?a?>", "a", "", 1, 6);
            test!func("<?foo?>", "foo", "", 1, 8);
            test!func("<?foo.?>", "foo.", "", 1, 9);
            test!func("<?foo bar?>", "foo", "bar", 1, 12);
            test!func("<?xmf bar?>", "xmf", "bar", 1, 12);
            test!func("<?xmlfoo bar?>", "xmlfoo", "bar", 1, 15);
            test!func("<?dlang is awesome?>", "dlang", "is awesome", 1, 21);
            test!func("<?dlang is awesome! ?>", "dlang", "is awesome! ", 1, 23);
            test!func("<?dlang\n\nis\n\nawesome\n\n?>", "dlang", "is\n\nawesome\n\n", 7, 3);

            testFail!func("<?xml?><?xml?>");
            testFail!func("<?XML?>");
            testFail!func("<?xMl?>");
            testFail!func("<?foo>");
            testFail!func("<? foo?>");
            testFail!func("<?\nfoo?>");
            testFail!func("<?foo ?>");
            testFail!func("<?foo ??>");
            testFail!func("<??foo?>");
            testFail!func("<?.foo?>");

            {
                auto xml = func("<!DOCTYPE foo><?foo bar?><root/>");
                auto range = assertNotThrown!XMLParsingException(parseXML(xml));
                assert(range.front.type == EntityType.pi);
                assert(equal(range.front.name, "foo"));
                assert(equal(range.front.text, "bar"));
            }
            {
                auto xml = func("<root><?foo bar?></root>");
                auto range = assertNotThrown!XMLParsingException(parseXML(xml));
                assertNotThrown!XMLParsingException(range.popFront());
                assert(equal(range.front.name, "foo"));
                assert(equal(range.front.text, "bar"));
            }
            {
                auto xml = func("<root/><?foo bar?>");
                auto range = assertNotThrown!XMLParsingException(parseXML(xml));
                assertNotThrown!XMLParsingException(range.popFront());
                assert(equal(range.front.name, "foo"));
                assert(equal(range.front.text, "bar"));
            }

            static foreach(pi; ["<?foo>", "<foo?>", "<? foo>"])
            {
                {
                    auto xml = func("<!DOCTYPE foo>" ~ pi ~ "<root/>");
                    assertThrown!XMLParsingException(parseXML(xml));
                }
                {
                    auto xml = func("<root>" ~ pi ~ "<root>");
                    auto range = assertNotThrown!XMLParsingException(parseXML(xml));
                    assertThrown!XMLParsingException(range.popFront());
                }
                {
                    auto xml = func("<root/>" ~ pi);
                    auto range = assertNotThrown!XMLParsingException(parseXML(xml));
                    assertThrown!XMLParsingException(range.popFront());
                }
            }

            {
                auto xml = "<?foo?>\n" ~
                           "<root>\n" ~
                           "    <?pi?>\n" ~
                           "</root>\n" ~
                           "<?bar?>";
                auto range = assertNotThrown!XMLParsingException(parseXML!simpleXML(func(xml)));
                assert(range.front.type == EntityType.elementStart);
                assert(equal(range.front.name, "root"));
                assert(range._text.pos == SourcePos(2, 7));
                assertNotThrown!XMLParsingException(range.popFront());
                assert(range.front.type == EntityType.elementEnd);
                assert(equal(range.front.name, "root"));
                assert(range._text.pos == SourcePos(4, 8));
                assertNotThrown!XMLParsingException(range.popFront());
                assert(range.empty);
                assert(range._text.pos == SourcePos(5, 8));
            }
        }
    }


    // CDSect  ::= CDStart CData CDEnd
    // CDStart ::= '<![CDATA['
    // CData   ::= (Char* - (Char* ']]>' Char*))
    // CDEnd   ::= ']]>'
    // Parses a CDATA. <!CDATA[ was already removed from the front of the input.
    void _parseCDATA()
    {
        _type = EntityType.cdata;
        _savedText.pos = _text.pos;
        _savedText.input = _text.takeUntilAndDrop!"]]>";
        _grammarPos = GrammarPos.contentCharData2;
    }


    // doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
    // DeclSep     ::= PEReference | S
    // intSubset   ::= (markupdecl | DeclSep)*
    // markupdecl  ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
    // Parse doctypedecl after GrammarPos.prologMisc1.
    // <!DOCTYPE and any whitespace after it should have already been removed
    // from the input.
    void _parseDoctypeDecl()
    {
        _text.skipToOneOf!('"', '\'', '[', '>')();
        switch(_text.input.front)
        {
            case '"':
            {
                _text.skipUntilAndDrop!`"`();
                checkNotEmpty(_text);
                _text.skipToOneOf!('[', '>')();
                if(_text.input.front == '[')
                    goto case '[';
                else
                    goto case '>';
            }
            case '\'':
            {
                _text.skipUntilAndDrop!`'`();
                checkNotEmpty(_text);
                _text.skipToOneOf!('[', '>')();
                if(_text.input.front == '[')
                    goto case '[';
                else
                    goto case '>';
            }
            case '[':
            {
                popFrontAndIncCol(_text);
                while(1)
                {
                    checkNotEmpty(_text);
                    _text.skipToOneOf!('"', '\'', ']')();
                    switch(_text.input.front)
                    {
                        case '"':
                        {
                            _text.skipUntilAndDrop!`"`();
                            continue;
                        }
                        case '\'':
                        {
                            _text.skipUntilAndDrop!`'`();
                            continue;
                        }
                        case ']':
                        {
                            _text.skipUntilAndDrop!`>`();
                            _parseAtPrologMisc!2();
                            return;
                        }
                        default: assert(0);
                    }
                }
            }
            case '>':
            {
                popFrontAndIncCol(_text);
                _parseAtPrologMisc!2();
                break;
            }
            default: assert(0);
        }
    }

    static if(compileInTests) unittest
    {
        import core.exception : AssertError;
        import std.exception : assertNotThrown, assertThrown, enforce;

        static void test(alias func)(string text, int row, int col, size_t line = __LINE__)
        {
            enum int tagLen = "<root/>".length;
            auto xml = func(text ~ "<root/>");
            static foreach(i, config; testConfigs)
            {{
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col + tagLen : -1);
                auto range = assertNotThrown!XMLParsingException(parseXML!config(xml.save));
                enforce!AssertError(range.front.type == EntityType.elementEmpty, "unittest failure 1", __FILE__, line);
                enforce!AssertError(range._text.pos == pos, "unittest failure 2", __FILE__, line);
            }}
        }

        static void testFail(alias func)(string text, size_t line = __LINE__)
        {
            auto xml = func(text);
            static foreach(i, config; testConfigs)
                assertThrown!XMLParsingException(parseXML!config(xml.save), "unittest failure", __FILE__, line);
        }

        static foreach(func; testRangeFuncs)
        {
            test!func("<!DOCTYPE name>", 1, 16);
            test!func("<!DOCTYPE \n\n\n name>", 4, 7);
            test!func("<!DOCTYPE name \n\n\n >", 4, 3);

            test!func("<!DOCTYPE name []>", 1, 19);
            test!func("<!DOCTYPE \n\n\n name []>", 4, 10);
            test!func("<!DOCTYPE name \n\n\n []>", 4, 5);

            test!func(`<!DOCTYPE name PUBLIC "'''" '"""'>`, 1, 35);
            test!func(`<!DOCTYPE name PUBLIC "'''" '"""' []>`, 1, 38);
            test!func(`<!DOCTYPE name PUBLIC 'foo' "'''">`, 1, 35);
            test!func(`<!DOCTYPE name PUBLIC 'foo' '"""' []>`, 1, 38);

            test!func("<!DOCTYPE name [ <!ELEMENT foo EMPTY > ]>", 1, 42);
            test!func("<!DOCTYPE name [ <!ELEMENT bar ANY > ]>", 1, 40);
            test!func("<!DOCTYPE name [ <!ELEMENT mixed (#PCDATA) > ]>", 1, 48);
            test!func("<!DOCTYPE name [ <!ELEMENT mixed (#PCDATA | foo)> ]>", 1, 53);
            test!func("<!DOCTYPE name [ <!ELEMENT kids (foo) > ]>", 1, 43);
            test!func("<!DOCTYPE name [ <!ELEMENT kids (foo | bar)> ]>", 1, 48);

            test!func("<!DOCTYPE name [ <!ATTLIST foo> ]>", 1, 35);
            test!func("<!DOCTYPE name [ <!ATTLIST foo def CDATA #REQUIRED> ]>", 1, 55);

            test!func(`<!DOCTYPE name [ <!ENTITY foo "bar"> ]>`, 1, 40);
            test!func(`<!DOCTYPE name [ <!ENTITY foo 'bar'> ]>`, 1, 40);
            test!func(`<!DOCTYPE name [ <!ENTITY foo SYSTEM 'sys'> ]>`, 1, 47);
            test!func(`<!DOCTYPE name [ <!ENTITY foo PUBLIC "'''" 'sys'> ]>`, 1, 53);

            test!func(`<!DOCTYPE name [ <!NOTATION note PUBLIC 'blah'> ]>`, 1, 51);

            test!func("<!DOCTYPE name [ <?pi> ]>", 1, 26);

            test!func("<!DOCTYPE name [ <!-- coment --> ]>", 1, 36);

            test!func("<!DOCTYPE name [ <?pi> <!----> <!ELEMENT blah EMPTY> ]>", 1, 56);
            test!func("<!DOCTYPE \nname\n[\n<?pi> \n <!---->\n<!ENTITY foo '\n\n'\n>\n]>", 10, 3);

            testFail!func("<!DOCTYP name>");
            testFail!func("<!DOCTYPEname>");
            testFail!func("<!DOCTYPE >");
            testFail!func("<!DOCTYPE>");
            testFail!func("<!DOCTYPE name1><!DOCTYPE name2>");
        }
    }


    // Parse a start tag or empty element tag. It could be the root element, or
    // it could be a sub-element.
    // < was already removed from the front of the input.
    void _parseElementStart()
    {
        _savedText.pos = _text.pos;
        _savedText.input = _text.takeUntilAndDrop!">"();

        if(_savedText.input.empty)
            throw new XMLParsingException("Tag missing name", _savedText.pos);
        if(_savedText.input.front == '/')
            throw new XMLParsingException("Invalid end tag", _savedText.pos);

        if(_savedText.input.length > 1)
        {
            auto temp = _savedText.input.save;
            temp.popFrontN(temp.length - 1);
            if(temp.front == '/')
            {
                _savedText.input = _savedText.input.takeExactly(_savedText.input.length - 1);

                static if(config.splitEmpty == SplitEmpty.no)
                {
                    _type = EntityType.elementEmpty;
                    _grammarPos = _tagStack.depth == 0 ? GrammarPos.endMisc : GrammarPos.contentCharData2;
                }
                else
                {
                    _type = EntityType.elementStart;
                    _grammarPos = GrammarPos.splittingEmpty;
                }
            }
            else
            {
                _type = EntityType.elementStart;
                _grammarPos = GrammarPos.contentCharData1;
            }
        }
        else
        {
            _type = EntityType.elementStart;
            _grammarPos = GrammarPos.contentCharData1;
        }

        _name = _savedText.takeName();
        // The attributes should be all that's left in savedText.
    }

    static if(compileInTests) unittest
    {
        import core.exception : AssertError;
        import std.algorithm.comparison : equal;
        import std.exception : assertNotThrown, assertThrown, enforce;
        import std.typecons : Tuple, tuple;

        static void test(alias func)(string text, EntityType type, string name,
                                     int row, int col, size_t line = __LINE__)
        {
            auto xml = func(text);
            static foreach(i, config; testConfigs)
            {{
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto range = assertNotThrown!XMLParsingException(parseXML!config(xml.save));
                enforce!AssertError(range.front.type == type, "unittest failure 1", __FILE__, line);
                enforce!AssertError(equal(range.front.name, name), "unittest failure 2", __FILE__, line);
                enforce!AssertError(range._text.pos == pos, "unittest failure 3", __FILE__, line);
            }}
        }

        static void testFail(alias func)(string text, size_t line = __LINE__)
        {
            auto xml = func(text);
            static foreach(i, config; testConfigs)
                assertThrown!XMLParsingException(parseXML!config(xml.save));
        }

        static foreach(func; testRangeFuncs)
        {
            test!func("<a/>", EntityType.elementEmpty, "a", 1, 5);
            test!func("<a></a>", EntityType.elementStart, "a", 1, 4);
            test!func("<root/>", EntityType.elementEmpty, "root", 1, 8);
            test!func("<root></root>", EntityType.elementStart, "root", 1, 7);
            test!func("<foo/>", EntityType.elementEmpty, "foo", 1, 7);
            test!func("<foo></foo>", EntityType.elementStart, "foo", 1, 6);
            test!func("<foo       />", EntityType.elementEmpty, "foo", 1, 14);
            test!func("<foo       ></foo>", EntityType.elementStart, "foo", 1, 13);
            test!func("<foo  \n\n\n />", EntityType.elementEmpty, "foo", 4, 4);
            test!func("<foo  \n\n\n ></foo>", EntityType.elementStart, "foo", 4, 3);
            test!func("<foo.></foo.>", EntityType.elementStart, "foo.", 1, 7);

            testFail!func(`<.foo/>`);
            testFail!func(`<>`);
            testFail!func(`</>`);

            {
                auto range = assertNotThrown!XMLParsingException(parseXML!simpleXML(func("<root/>")));
                assert(range.front.type == EntityType.elementStart);
                assert(equal(range.front.name, "root"));
                assert(range._text.pos == SourcePos(1, 8));
                assertNotThrown!XMLParsingException(range.popFront());
                assert(range.front.type == EntityType.elementEnd);
                assert(equal(range.front.name, "root"));
                assert(range._text.pos == SourcePos(1, 8));
            }
        }
    }


    // Parse an end tag. It could be the root element, or it could be a
    // sub-element.
    // </ was already removed from the front of the input.
    void _parseElementEnd()
    {
        _type = EntityType.elementEnd;
        immutable namePos = _text.pos;
        _name = _text.takeUntilAndDrop!">"();
        _tagStack.pop(_name.save, namePos);
        _grammarPos = _tagStack.depth == 0 ? GrammarPos.endMisc : GrammarPos.contentCharData2;
    }


    // GrammarPos.contentCharData1
    // content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
    // Parses at either CharData?. Nothing from the CharData? (or what's after it
    // if it's not there) has been consumed.
    void _parseAtContentCharData()
    {
        checkNotEmpty(_text);
        auto orig = _text.save;
        stripWS(_text);
        checkNotEmpty(_text);
        if(_text.input.front != '<')
            _text = orig;
        if(_text.input.front != '<')
        {
            _type = EntityType.text;
            _savedText.input = _text.takeUntilAndDrop!"<"();
            checkNotEmpty(_text);
            if(_text.input.front == '/')
            {
                popFrontAndIncCol(_text);
                _grammarPos = GrammarPos.endTag;
            }
            else
                _grammarPos = GrammarPos.contentMid;
        }
        else
        {
            popFrontAndIncCol(_text);
            checkNotEmpty(_text);
            if(_text.input.front == '/')
            {
                popFrontAndIncCol(_text);
                _parseElementEnd();
            }
            else
                _parseAtContentMid();
        }
    }


    // GrammarPos.contentMid
    // content     ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
    // The text right after the start tag was what was parsed previously. So,
    // that first CharData? was what was parsed last, and this parses starting
    // right after. The < should have already been removed from the input.
    void _parseAtContentMid()
    {
        // Note that References are treated as part of the CharData and not
        // parsed out by the EntityRange (see EntityRange.text).

        switch(_text.input.front)
        {
            // Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
            // CDSect  ::= CDStart CData CDEnd
            // CDStart ::= '<![CDATA['
            // CData   ::= (Char* - (Char* ']]>' Char*))
            // CDEnd   ::= ']]>'
            case '!':
            {
                popFrontAndIncCol(_text);
                if(_text.stripStartsWith("--"))
                {
                    _parseComment();
                    static if(config.skipComments == SkipComments.yes)
                        _parseAtContentCharData();
                    else
                        _grammarPos = GrammarPos.contentCharData2;
                }
                else if(_text.stripStartsWith("[CDATA["))
                    _parseCDATA();
                else
                    throw new XMLParsingException("Not valid Comment or CData section", _text.pos);
                break;
            }
            // PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
            case '?':
            {
                _parsePI();
                _grammarPos = GrammarPos.contentCharData2;
                static if(config.skipPI == SkipPI.yes)
                    popFront();
                break;
            }
            // element ::= EmptyElemTag | STag content ETag
            default:
            {
                _parseElementStart();
                break;
            }
        }
    }


    // This parses the Misc* that come after the root element.
    void _parseAtEndMisc()
    {
        // Misc ::= Comment | PI | S

        stripWS(_text);

        if(_text.input.empty)
        {
            _grammarPos = GrammarPos.documentEnd;
            return;
        }

        if(_text.input.front != '<')
            throw new XMLParsingException("Expected <", _text.pos);
        popFrontAndIncCol(_text);
        checkNotEmpty(_text);

        switch(_text.input.front)
        {
            // Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
            case '!':
            {
                popFrontAndIncCol(_text);
                if(_text.stripStartsWith("--"))
                {
                    _parseComment();
                    break;
                }
                throw new XMLParsingException("Expected --", _text.pos);
            }
            // PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
            case '?':
            {
                _parsePI();
                static if(config.skipPI == SkipPI.yes)
                    popFront();
                break;
            }
            default: throw new XMLParsingException("Must be a comment or PI", _text.pos);
        }
    }

    // Used for keeping track of the names of start tags so that end tags can be
    // verified. We keep track of the total number of start tags and end tags
    // which have been parsed thus far so that only whichever EntityRange is
    // farthest along in parsing actually adds or removes tags from the
    // TagStack. That way, the end tags get verified, but we only have one
    // stack. If the stack were duplicated with every call to save, then there
    // would be a lot more allocations, which we don't want. But because we only
    // need to verify the end tags once, we can get away with having a shared
    // tag stack. The cost is that we have to keep track of how many tags we've
    // parsed so that we know if an EntityRange should actually be pushing or
    // popping tags from the stack, but that's a lot cheaper than duplicating
    // the stack, and it's a lot less annoying then making EntityRange an input
    // range and not a forward range or making it a cursor rather than a range.
    struct TagStack
    {
        void push(Taken tagName)
        {
            if(steps++ == state.maxSteps)
            {
                ++state.maxSteps;
                state.tags ~= tagName;
            }
            ++depth;
        }

        void pop(Taken tagName, SourcePos pos)
        {
            import std.algorithm : equal;
            import std.format : format;
            if(steps++ == state.maxSteps)
            {
                assert(!state.tags.empty);
                if(!equal(state.tags.back.save, tagName.save))
                {
                    enum fmt = "Name of end tag </%s> does not match corresponding start tag <%s>";
                    throw new XMLParsingException(format!fmt(tagName, state.tags.back), pos);
                }
                ++state.maxSteps;
                --state.tags.length;
                state.tags.assumeSafeAppend();
            }
            --depth;
        }

        struct SharedState
        {
            Taken[] tags;
            size_t maxSteps;
        }

        static create()
        {
            TagStack tagStack;
            tagStack.state = new SharedState;
            tagStack.state.tags.reserve(10);
            return tagStack;
        }

        SharedState* state;
        size_t steps;
        int depth;
    }


    struct Text(R)
    {
        alias config = cfg;
        alias Input = R;

        Input input;

        static if(config.posType == PositionType.lineAndCol)
            auto pos = SourcePos(1, 1);
        else static if(config.posType == PositionType.line)
            auto pos = SourcePos(1, -1);
        else
            SourcePos pos;

        @property save() { return typeof(this)(input.save, pos); }
    }


    alias Taken = typeof(takeExactly(byCodeUnit(R.init), 42));


    EntityType _type;
    auto _grammarPos = GrammarPos.documentStart;

    Taken _name;
    TagStack _tagStack;

    Text!(typeof(byCodeUnit(R.init))) _text;
    Text!Taken _savedText;


    this(R xmlText)
    {
        _tagStack = TagStack.create();
        _text.input = byCodeUnit(xmlText);

        // None of these initializations should be required. https://issues.dlang.org/show_bug.cgi?id=13945
        _savedText = typeof(_savedText).init;
        _name = typeof(_name).init;

        popFront();
    }
}

/// Ditto
EntityRange!(config, R) parseXML(Config config = Config.init, R)(R xmlText)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    return EntityRange!(config, R)(xmlText);
}

///
unittest
{
    auto xml = "<?xml version='1.0'?>\n" ~
               "<?instruction start?>\n" ~
               "<foo attr='42'>\n" ~
               "    <bar/>\n" ~
               "    <!-- no comment -->\n" ~
               "    <baz hello='world'>\n" ~
               "    nothing to say.\n" ~
               "    nothing at all...\n" ~
               "    </baz>\n" ~
               "</foo>\n" ~
               "<?some foo?>";

    {
        auto range = parseXML(xml);
        assert(range.front.type == EntityType.pi);
        assert(range.front.name == "instruction");
        assert(range.front.text == "start");

        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "foo");

        {
            auto attrs = range.front.attributes;
            assert(walkLength(attrs.save) == 1);
            assert(attrs.front.name == "attr");
            assert(attrs.front.value == "42");
        }

        range.popFront();
        assert(range.front.type == EntityType.elementEmpty);
        assert(range.front.name == "bar");

        range.popFront();
        assert(range.front.type == EntityType.comment);
        assert(range.front.text == " no comment ");

        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "baz");

        {
            auto attrs = range.front.attributes;
            assert(walkLength(attrs.save) == 1);
            assert(attrs.front.name == "hello");
            assert(attrs.front.value == "world");
        }

        range.popFront();
        assert(range.front.type == EntityType.text);
        assert(range.front.text ==
               "\n    nothing to say.\n    nothing at all...\n    ");

        range.popFront();
        assert(range.front.type == EntityType.elementEnd); // </baz>
        range.popFront();
        assert(range.front.type == EntityType.elementEnd); // </foo>

        range.popFront();
        assert(range.front.type == EntityType.pi);
        assert(range.front.name == "some");
        assert(range.front.text == "foo");

        range.popFront();
        assert(range.empty);
    }
    {
        auto range = parseXML!simpleXML(xml);

        // simpleXML is set to skip processing instructions.

        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "foo");

        {
            auto attrs = range.front.attributes;
            assert(walkLength(attrs.save) == 1);
            assert(attrs.front.name == "attr");
            assert(attrs.front.value == "42");
        }

        // simpleXML is set to split empty tags so that <bar/> is treated
        // as the same as <bar></bar> so that code does not have to
        // explicitly handle empty tags.
        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "bar");
        range.popFront();
        assert(range.front.type == EntityType.elementEnd);
        assert(range.front.name == "bar");

        // simpleXML is set to skip comments.

        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "baz");

        {
            auto attrs = range.front.attributes;
            assert(walkLength(attrs.save) == 1);
            assert(attrs.front.name == "hello");
            assert(attrs.front.value == "world");
        }

        range.popFront();
        assert(range.front.type == EntityType.text);
        assert(range.front.text ==
               "\n    nothing to say.\n    nothing at all...\n    ");

        range.popFront();
        assert(range.front.type == EntityType.elementEnd); // </baz>
        range.popFront();
        assert(range.front.type == EntityType.elementEnd); // </foo>
        range.popFront();
        assert(range.empty);
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

version(unittest)
    EntityRange!(Config.init, EntityCompileTests) _entityRangeTests;


/++
    Takes an $(LREF EntityRange) which is at a start tag and iterates it until
    it is at its corresponding end tag. It is an error to call skipContents when
    the current entity is not $(LREF EntityType.elementStart).

    $(TABLE
        $(TR $(TH Supported $(LREF EntityType)s:))
        $(TR $(TD $(LREF2 elementStart, EntityType)))
    )

    Returns: The range with front now at the end tag corresponding to the start
             tag that was front when the function was called.

    Throws: $(LREF XMLParsingException) on invalid XML.
  +/
R skipContents(R)(R entityRange)
    if(isInstanceOf!(EntityRange, R))
{
    assert(entityRange._type == EntityType.elementStart);

    // FIXME Rather than parsing exactly the same as normal, this should
    // skip as much parsing as possible.

    // We don't bother calling empty, because the only way for the entityRange to be
    // empty would be for it to reach the end of the document, and an
    // XMLParsingException would be thrown if the end of the document were
    // reached before we reached the corresponding end tag.
    for(int tagDepth = 1; tagDepth != 0;)
    {
        entityRange.popFront();
        immutable type = entityRange._type;
        if(type == EntityType.elementStart)
            ++tagDepth;
        else if(type == EntityType.elementEnd)
            --tagDepth;
    }

    return entityRange;
}

///
unittest
{
    auto xml = "<root>\n" ~
               "    <foo>\n" ~
               "        <bar>\n" ~
               "        Some text\n" ~
               "        </bar>\n" ~
               "    </foo>\n" ~
               "    <!-- no comment -->\n" ~
               "</root>";

    auto range = parseXML(xml);
    assert(range.front.type == EntityType.elementStart);
    assert(range.front.name == "root");

    range.popFront();
    assert(range.front.type == EntityType.elementStart);
    assert(range.front.name == "foo");

    range = range.skipContents();
    assert(range.front.type == EntityType.elementEnd);
    assert(range.front.name == "foo");

    range.popFront();
    assert(range.front.type == EntityType.comment);
    assert(range.front.text == " no comment ");

    range.popFront();
    assert(range.front.type == EntityType.elementEnd);
    assert(range.front.name == "root");

    range.popFront();
    assert(range.empty);
}


/++
    Skips entities until the given $(LREF EntityType) is reached.

    If multiple $(LREF EntityType)s are given, then any one of them counts as
    a match.

    The current entity is skipped regardless of whether it is the given
    $(LREF EntityType).

    This is essentially a slightly optimized equivalent to
    $(D range.find!((a, b) => a.type == b.type)(entityTypes)).

    Returns: The given range with front now at the first entity which matched
             one of the given $(LREF EntityType)s or an empty range if none were
             found.
  +/
R skipToEntityType(R)(R entityRange, EntityType[] entityTypes...)
    if(isInstanceOf!(EntityRange, R))
{
    if(entityRange.empty)
        return entityRange;
    entityRange.popFront();
    for(; !entityRange.empty; entityRange.popFront())
    {
        immutable type = entityRange._type;
        foreach(entityType; entityTypes)
        {
            if(type == entityType)
                return entityRange;
        }
    }
    return entityRange;
}

///
unittest
{
    auto xml = "<root>\n" ~
               "    <!-- blah blah blah -->\n" ~
               "    <foo>nothing to say</foo>\n" ~
               "</root>";

    auto range = parseXML(xml);
    assert(range.front.type == EntityType.elementStart);
    assert(range.front.name == "root");

    range = range.skipToEntityType(EntityType.elementStart,
                                   EntityType.elementEmpty);
    assert(range.front.type == EntityType.elementStart);
    assert(range.front.name == "foo");

    assert(range.skipToEntityType(EntityType.comment).empty);
}


/++
    Treats the given string like a file path except that each directory
    corresponds to the name of a start tag. Note that this does $(I not) try to
    implement XPath as that would be quite complicated, but it does try to be
    compatible with it for the small subset of the syntax that it supports.

    All paths should be relative. $(LREF EntityRange) can only move forward
    through the document, so using an absolute path would only make sense at
    the beginning of the document.

    Returns: The given range with front now at the requested entity if the path
             is valid; otherwise, an empty range is returned.
  +/
R skipToPath(R)(R entityRange, string path)
    if(isInstanceOf!(EntityRange, R))
{
    import std.algorithm.comparison : equal;
    import std.path : pathSplitter;

    with(EntityType)
    {
        static if(R.config.splitEmpty == SplitEmpty.yes)
            EntityType[2] startOrEnd = [elementStart, elementEnd];
        else
            EntityType[3] startOrEnd = [elementStart, elementEnd, elementEmpty];

        R findOnCurrLevel(string name)
        {
            if(entityRange._type == elementStart)
                entityRange = entityRange.skipContents();
            while(true)
            {
                entityRange = entityRange.skipToEntityType(startOrEnd[]);
                if(entityRange.empty)
                    return entityRange;
                if(entityRange._type == elementEnd)
                    return entityRange.takeNone();

                if(equal(name, entityRange._name.save))
                    return entityRange;

                static if(R.config.splitEmpty == SplitEmpty.no)
                {
                    if(entityRange._type == elementEmpty)
                        continue;
                }
                entityRange = entityRange.skipContents();
            }
        }

        bool checkCurrLevel = false;

        foreach(name; path.pathSplitter())
        {
            if(name.empty || name == ".")
                continue;
            if(name == "..")
            {
                entityRange = entityRange.skipToParentDepth();
                if(entityRange.empty)
                    return entityRange;
                checkCurrLevel = true;
                continue;
            }

            static if(R.config.splitEmpty == SplitEmpty.yes)
                immutable atStart = entityRange._type == elementStart;
            else
                immutable atStart = entityRange._type == elementStart || entityRange._type == elementEmpty;

            if(!atStart)
            {
                entityRange = findOnCurrLevel(name);
                if(entityRange.empty)
                    return entityRange;
                checkCurrLevel = false;
                continue;
            }

            if(checkCurrLevel)
            {
                checkCurrLevel = false;
                if(!equal(name, entityRange._name.save))
                {
                    entityRange = findOnCurrLevel(name);
                    if(entityRange.empty)
                        return entityRange;
                }
                continue;
            }

            static if(R.config.splitEmpty == SplitEmpty.no)
            {
                // elementEmpty has no children to check
                if(entityRange._type == elementEmpty)
                    return entityRange.takeNone();
            }

            entityRange = entityRange.skipToEntityType(startOrEnd[]);
            assert(!entityRange.empty);
            if(entityRange._type == elementEnd)
                return entityRange.takeNone();

            if(!equal(name, entityRange._name.save))
            {
                entityRange = findOnCurrLevel(name);
                if(entityRange.empty)
                    return entityRange;
            }
        }

        return entityRange;
    }
}

///
unittest
{
    auto xml = "<carrot>\n" ~
               "    <foo>\n" ~
               "        <bar a='42'>\n" ~
               "            <baz/>\n" ~
               "        </bar>\n" ~
               "        <foo></foo>\n" ~
               "        <bar b='24'>\n" ~
               "            <fuzz>\n" ~
               "                <buzz>blah</buzz>\n" ~
               "            </fuzz>\n" ~
               "        </bar>\n" ~
               "    </foo>\n" ~
               "</carrot>";
    {
        auto range = parseXML(xml);
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "carrot");

        range = range.skipToPath("foo/bar");
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "bar");
        assert(range.front.attributes.front.name == "a");

        range = range.skipToPath("baz");
        assert(range.front.type == EntityType.elementEmpty);
        assert(range.front.name == "baz");

        range = range.skipToPath("../bar/fuzz/buzz");
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "buzz");

        assert(range.skipToPath("bar").empty);
    }
    {
        auto range = parseXML(xml);
        assert(range.front.type == EntityType.elementStart);
        range = range.skipToPath("foo/bar");
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "bar");
        assert(range.front.attributes.front.name == "a");

        range = range.skipToPath("./baz");
        assert(range.front.type == EntityType.elementEmpty);
        assert(range.front.name == "baz");

        range = range.skipToPath("../bar");
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "bar");
        assert(range.front.attributes.front.name == "b");

        assert(range.skipToPath("buzz").empty);
    }
    // If the current entity is not a start tag, then the first "directory"
    // in the path is matched against a start tag at the same level rather
    // than against a child tag.
    {
        auto range = parseXML(xml);
        range = range.skipToEntityType(EntityType.elementEmpty);
        assert(range.front.type == EntityType.elementEmpty);
        assert(range.front.name == "baz");
        range.popFront();
        assert(range.front.type == EntityType.elementEnd);
        assert(range.front.name == "bar");
        range = range.skipToPath("bar");
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.attributes.front.name == "b");
    }
    // The first matching start tag is always taken, so even though there is
    // a bar/fuzz under foo, because fuzz is under the second bar, not the
    // first, it's not found.
    {
        auto range = parseXML(xml);
        assert(range.skipToPath("carrot/foo/bar/fuzz").empty);
    }
}

unittest
{
    auto xml = "<!-- and so it begins -->\n" ~
               "<potato>\n" ~
               "    <foo a='whatever'/>\n" ~
               "    <foo b='whatever'>\n" ~
               "        <bar>text<i>more</i>text</bar>\n" ~
               "        <!-- no comment -->\n" ~
               "        <bar>\n" ~
               "            <bar/>\n" ~
               "            <baz/>\n" ~
               "        </bar>\n" ~
               "    </foo>\n" ~
               "    <!-- comment -->\n" ~
               "</potato>";
    static foreach(i, config; [Config.init, simpleXML])
    {{
        static if(i == 0)
            enum empty = EntityType.elementEmpty;
        else
            enum empty = EntityType.elementStart;
        foreach(str; ["potato/foo", "./potato/foo", "./potato/./foo", "./potato///foo", "./potato/foo/"])
        {
            auto range = parseXML!config(xml).skipToPath("potato/foo");
            assert(range.front.type == empty);
            assert(range.front.name == "foo");
            assert(range.front.attributes.front.name == "a");
            assert(range.skipToPath("bar").empty);
        }
        {
            auto range = parseXML!config(xml).skipToPath("./potato/foo");
            assert(range.front.type == empty);
            range = range.skipToEntityType(EntityType.elementStart);
            assert(range.front.type == EntityType.elementStart);
            assert(range.front.name == "foo");
            assert(range.front.attributes.front.name == "b");
            assert(range.skipToPath("bar/baz").empty);
            //FIXME more testing needed
        }
    }}
}


/++
    Skips entities until an entity is reached that is at the same depth as the
    parent of the current entity.

    Returns: The given range with front now at the first entity found which is
             at the same depth as the entity which was front when
             skipToParentDepth was called. If the requested depth is not found
             (which means that the depth was 0 when skipToParentDepth was
             called), then an empty range is returned.
  +/
R skipToParentDepth(R)(R entityRange)
    if(isInstanceOf!(EntityRange, R))
{
    with(EntityType) final switch(entityRange._type)
    {
        case cdata:
        case comment:
        {
            entityRange = entityRange.skipToEntityType(elementStart, elementEnd);
            if(entityRange.empty || entityRange._type == elementEnd)
                return entityRange;
            goto case elementStart;
        }
        case elementStart:
        {
            while(true)
            {
                entityRange = entityRange.skipContents();
                entityRange.popFront();
                if(entityRange.empty || entityRange._type == elementEnd)
                    return entityRange;
                if(entityRange._type == elementStart)
                    continue;
                goto case comment;
            }
            assert(0); // the compiler isn't smart enough to see that this is unreachable.
        }
        case elementEnd:
        case elementEmpty:
        case pi:
        case text: goto case comment;
    }
}

///
unittest
{
    auto xml = "<root>\n" ~
               "    <foo>\n" ~
               "        <!-- comment -->\n" ~
               "        <bar>exam</bar>\n" ~
               "    </foo>\n" ~
               "    <!-- another comment -->\n" ~
               "</root>";
    {
        auto range = parseXML(xml);
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "root");

        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "foo");

        range.popFront();
        assert(range.front.type == EntityType.comment);
        assert(range.front.text == " comment ");

        range = range.skipToParentDepth();
        assert(range.front.type == EntityType.elementEnd);
        assert(range.front.name == "foo");

        range = range.skipToParentDepth();
        assert(range.front.type == EntityType.elementEnd);
        assert(range.front.name == "root");

        range = range.skipToParentDepth();
        assert(range.empty);
    }
    {
        auto range = parseXML(xml);
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "root");

        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "foo");

        range.popFront();
        assert(range.front.type == EntityType.comment);
        assert(range.front.text == " comment ");

        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "bar");

        range.popFront();
        assert(range.front.type == EntityType.text);
        assert(range.front.text == "exam");

        range = range.skipToParentDepth();
        assert(range.front.type == EntityType.elementEnd);
        assert(range.front.name == "bar");

        range = range.skipToParentDepth();
        assert(range.front.type == EntityType.elementEnd);
        assert(range.front.name == "foo");

        range.popFront();
        assert(range.front.type == EntityType.comment);
        assert(range.front.text == " another comment ");

        range = range.skipToParentDepth();
        assert(range.front.type == EntityType.elementEnd);
        assert(range.front.name == "root");

        assert(range.skipToParentDepth().empty);
    }
    {
        auto range = parseXML("<root><foo>bar</foo></root>");
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "root");
        assert(range.skipToParentDepth().empty);
    }
}

unittest
{
    import std.exception : assertNotThrown;
    // cdata
    {
        auto xml = "<root>\n" ~
                   "    <![CDATA[ cdata run ]]>\n" ~
                   "    <nothing/>\n" ~
                   "    <![CDATA[ cdata have its bits flipped ]]>\n" ~
                   "    <foo></foo>\n" ~
                   "    <![CDATA[ cdata play violin ]]>\n" ~
                   "</root>";
        auto range = parseXML(xml);
        assert(range.front.type == EntityType.elementStart);
        range.popFront();
        assert(range.front.type == EntityType.cdata);
        {
            auto temp = range.save;
            assert(temp.front.text == " cdata run ");
            temp = temp.skipToParentDepth();
            assert(temp._type == EntityType.elementEnd);
            assert(temp.front.name == "root");
        }
        range.popFront();
        assert(range.front.type == EntityType.elementEmpty);
        range.popFront();
        assert(range.front.type == EntityType.cdata);
        {
            auto temp = range.save;
            assert(temp.front.text == " cdata have its bits flipped ");
            temp = temp.skipToParentDepth();
            assert(temp._type == EntityType.elementEnd);
            assert(temp.front.name == "root");
        }
        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        range = range.skipContents();
        range.popFront();
        assert(range.front.type == EntityType.cdata);
        assert(range.front.text == " cdata play violin ");
        range = range.skipToParentDepth();
        assert(range._type == EntityType.elementEnd);
        assert(range.front.name == "root");
    }
    // comment
    {
        auto xml = "<!-- before -->\n" ~
                   "<root>\n" ~
                   "    <!-- comment 1 -->\n" ~
                   "    <nothing/>\n" ~
                   "    <!-- comment 2 -->\n" ~
                   "    <foo></foo>\n" ~
                   "    <!-- comment 3 -->\n" ~
                   "</root>\n" ~
                   "<!-- after -->" ~
                   "<!-- end -->";

        assert(parseXML(xml).skipToParentDepth().empty);

        {
            auto range = parseXML(xml);
            assert(range.front.type == EntityType.comment);
            range.popFront();
            assert(range.front.type == EntityType.elementStart);
            range.popFront();
            assert(range.front.type == EntityType.comment);
            {
                auto temp = range.save;
                assert(temp.front.text == " comment 1 ");
                temp = temp.skipToParentDepth();
                assert(temp._type == EntityType.elementEnd);
                assert(temp.front.name == "root");
            }
            range.popFront();
            assert(range.front.type == EntityType.elementEmpty);
            range.popFront();
            assert(range.front.type == EntityType.comment);
            {
                auto temp = range.save;
                assert(temp.front.text == " comment 2 ");
                temp = temp.skipToParentDepth();
                assert(temp._type == EntityType.elementEnd);
                assert(temp.front.name == "root");
            }
            range.popFront();
            assert(range.front.type == EntityType.elementStart);
            range = range.skipContents();
            range.popFront();
            assert(range.front.type == EntityType.comment);
            assert(range.front.text == " comment 3 ");
            range = range.skipToParentDepth();
            assert(range._type == EntityType.elementEnd);
            assert(range.front.name == "root");
        }
        {
            auto range = parseXML(xml);
            assert(range.front.type == EntityType.comment);
            range.popFront();
            assert(range.front.type == EntityType.elementStart);
            range = range.skipContents();
            range.popFront();
            assert(range.front.type == EntityType.comment);
            assert(range.front.text == " after ");
            assert(range.save.skipToParentDepth().empty);
            range.popFront();
            assert(range.front.type == EntityType.comment);
            assert(range.front.text == " end ");
            assert(range.skipToParentDepth().empty);
        }
    }
    // elementStart
    {
        auto xml = "<root>\n" ~
                   "    <a><b>foo</b></a>\n" ~
                   "    <nothing/>\n" ~
                   "    <c></c>\n" ~
                   "</root>";

        auto range = parseXML(xml);
        assert(range.front.type == EntityType.elementStart);
        {
            auto temp = range.save;
            assert(temp.front.name == "root");
            assert(temp.skipToParentDepth().empty);
        }
        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        {
            auto temp = range.save;
            assert(temp.front.name == "a");
            temp = temp.skipToParentDepth();
            assert(temp._type == EntityType.elementEnd);
            assert(temp.front.name == "root");
        }
        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        {
            auto temp = range.save;
            assert(temp.front.name == "b");
            temp = temp.skipToParentDepth();
            assert(temp._type == EntityType.elementEnd);
            assert(temp.front.name == "a");
        }
        range.popFront();
        assert(range.front.type == EntityType.text);
        range.popFront();
        assert(range.front.type == EntityType.elementEnd);
        range.popFront();
        assert(range.front.type == EntityType.elementEnd);
        range.popFront();
        assert(range.front.type == EntityType.elementEmpty);
        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "c");
        range = range.skipToParentDepth();
        assert(range._type == EntityType.elementEnd);
        assert(range.front.name == "root");
    }
    // elementEnd
    {
        auto xml = "<root>\n" ~
                   "    <a><b>foo</b></a>\n" ~
                   "    <nothing/>\n" ~
                   "    <c></c>\n" ~
                   "</root>";

        auto range = parseXML(xml);
        assert(range.front.type == EntityType.elementStart);
        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        range.popFront();
        assert(range.front.type == EntityType.text);
        range.popFront();
        assert(range.front.type == EntityType.elementEnd);
        {
            auto temp = range.save;
            assert(temp.front.name == "b");
            temp = temp.skipToParentDepth();
            assert(temp._type == EntityType.elementEnd);
            assert(temp.front.name == "a");
        }
        range.popFront();
        assert(range.front.type == EntityType.elementEnd);
        {
            auto temp = range.save;
            assert(temp.front.name == "a");
            temp = temp.skipToParentDepth();
            assert(temp._type == EntityType.elementEnd);
            assert(temp.front.name == "root");
        }
        range.popFront();
        assert(range.front.type == EntityType.elementEmpty);
        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        range.popFront();
        assert(range.front.type == EntityType.elementEnd);
        {
            auto temp = range.save;
            assert(temp.front.name == "c");
            temp = temp.skipToParentDepth();
            assert(temp._type == EntityType.elementEnd);
            assert(temp.front.name == "root");
        }
        range.popFront();
        assert(range.front.type == EntityType.elementEnd);
        assert(range.skipToParentDepth().empty);
    }
    // elementEmpty
    {
        auto range = parseXML("<root/>");
        assert(range.front.type == EntityType.elementEmpty);
        assert(range.skipToParentDepth().empty);
    }
    {
        auto xml = "<root>\n" ~
                   "    <a><b>foo</b></a>\n" ~
                   "    <nothing/>\n" ~
                   "    <c></c>\n" ~
                   "    <whatever/>\n" ~
                   "</root>";

        auto range = parseXML(xml);
        assert(range.front.type == EntityType.elementStart);
        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        range = range.skipContents();
        range.popFront();
        assert(range.front.type == EntityType.elementEmpty);
        assert(range.front.name == "nothing");
        {
            auto temp = range.save;
            temp.popFront();
            assert(temp.front.type == EntityType.elementStart);
            temp.popFront();
            assert(temp.front.type == EntityType.elementEnd);
            temp.popFront();
            assert(temp.front.type == EntityType.elementEmpty);
            assert(temp.front.name == "whatever");
        }
        range = range.skipToParentDepth();
        assert(range._type == EntityType.elementEnd);
        assert(range.front.name == "root");
    }
    // pi
    {
        auto xml = "<?Sherlock?>\n" ~
                   "<root>\n" ~
                   "    <?Foo?>\n" ~
                   "    <nothing/>\n" ~
                   "    <?Bar?>\n" ~
                   "    <foo></foo>\n" ~
                   "    <?Baz?>\n" ~
                   "</root>\n" ~
                   "<?Poirot?>\n" ~
                   "<?Conan?>";

        {
            auto range = parseXML(xml);
            assert(range.front.type == EntityType.pi);
            {
                auto temp = range.save;
                assert(temp.front.name == "Sherlock");
                assert(temp.skipToParentDepth().empty);
            }
            range.popFront();
            assert(range.front.type == EntityType.elementStart);
            range.popFront();
            assert(range.front.type == EntityType.pi);
            {
                auto temp = range.save;
                assert(temp.front.name == "Foo");
                temp = temp.skipToParentDepth();
                assert(temp._type == EntityType.elementEnd);
                assert(temp.front.name == "root");
            }
            range.popFront();
            assert(range.front.type == EntityType.elementEmpty);
            range.popFront();
            assert(range.front.type == EntityType.pi);
            {
                auto temp = range.save;
                assert(temp.front.name == "Bar");
                temp = temp.skipToParentDepth();
                assert(temp._type == EntityType.elementEnd);
                assert(temp.front.name == "root");
            }
            range.popFront();
            assert(range.front.type == EntityType.elementStart);
            range.popFront();
            assert(range.front.type == EntityType.elementEnd);
            range.popFront();
            assert(range.front.type == EntityType.pi);
            assert(range.front.name == "Baz");
            range = range.skipToParentDepth();
            assert(range._type == EntityType.elementEnd);
            assert(range.front.name == "root");
            range.popFront();
            assert(range.front.type == EntityType.pi);
            {
                auto temp = range.save;
                assert(temp.front.name == "Poirot");
                assert(temp.skipToParentDepth().empty);
            }
            range.popFront();
            assert(range.front.type == EntityType.pi);
            assert(range.front.name == "Conan");
            assert(range.skipToParentDepth().empty);
        }
    }
    // text
    {
        auto xml = "<root>\n" ~
                   "    nothing to say\n" ~
                   "    <nothing/>\n" ~
                   "    nothing whatsoever\n" ~
                   "    <foo></foo>\n" ~
                   "    but he keeps talking\n" ~
                   "</root>";

        {
            auto range = parseXML(xml);
            assert(range.front.type == EntityType.elementStart);
            range.popFront();
            assert(range.front.type == EntityType.text);
            {
                auto temp = range.save;
                assert(temp.front.text == "\n    nothing to say\n    ");
                temp = temp.skipToParentDepth();
                assert(temp._type == EntityType.elementEnd);
                assert(temp.front.name == "root");
            }
            range.popFront();
            assert(range.front.type == EntityType.elementEmpty);
            range.popFront();
            assert(range.front.type == EntityType.text);
            {
                auto temp = range.save;
                assert(temp.front.text == "\n    nothing whatsoever\n    ");
                temp = temp.skipToParentDepth();
                assert(temp._type == EntityType.elementEnd);
                assert(temp.front.name == "root");
            }
            range.popFront();
            assert(range.front.type == EntityType.elementStart);
            range = range.skipContents();
            range.popFront();
            assert(range.front.type == EntityType.text);
            assert(range.front.text == "\n    but he keeps talking\n");
            range = range.skipToParentDepth();
            assert(range._type == EntityType.elementEnd);
            assert(range.front.name == "root");
        }
    }
}


//------------------------------------------------------------------------------
// Private Section
//------------------------------------------------------------------------------
private:


version(unittest) auto testParser(Config config, R)(R xmlText) @trusted pure nothrow @nogc
{
    import std.utf : byCodeUnit;
    typeof(EntityRange!(config, R)._text) text;
    text.input = byCodeUnit(xmlText);
    return text;
}


// Used to indicate where in the grammar we're currently parsing.
enum GrammarPos
{
    // Nothing has been parsed yet.
    documentStart,

    // document ::= prolog element Misc*
    // prolog   ::= XMLDecl? Misc* (doctypedecl Misc*)?
    // This is that first Misc*. The next entity to parse is either a Misc, the
    // doctypedecl, or the root element which follows the prolog.
    prologMisc1,

    // document ::= prolog element Misc*
    // prolog   ::= XMLDecl? Misc* (doctypedecl Misc*)
    // This is that second Misc*. The next entity to parse is either a Misc or
    // the root element which follows the prolog.
    prologMisc2,

    // Used with SplitEmpty.yes to tell the parser that we're currently at an
    // empty element tag that we're treating as a start tag, so the next entity
    // will be an end tag even though we didn't actually parse one.
    splittingEmpty,

    // element  ::= EmptyElemTag | STag content ETag
    // content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
    // This is at the beginning of content at the first CharData?. The next
    // thing to parse will be a CharData, element, CDSect, PI, Comment, or ETag.
    // References are treated as part of the CharData and not parsed out by the
    // EntityRange (see EntityRange.Entity.text).
    contentCharData1,

    // element  ::= EmptyElemTag | STag content ETag
    // content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
    // This is after the first CharData?. The next thing to parse will be a
    // element, CDSect, PI, Comment, or ETag.
    // References are treated as part of the CharData and not parsed out by the
    // EntityRange (see EntityRange.Entity.text).
    contentMid,

    // element  ::= EmptyElemTag | STag content ETag
    // content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
    // This is at the second CharData?. The next thing to parse will be a
    // CharData, element, CDSect, PI, Comment, or ETag.
    // References are treated as part of the CharData and not parsed out by the
    // EntityRange (see EntityRange.Entity.text).
    contentCharData2,

    // element  ::= EmptyElemTag | STag content ETag
    // content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
    // This is after the second CharData?. The next thing to parse is an ETag.
    endTag,

    // document ::= prolog element Misc*
    // This is the Misc* at the end of the document. The next thing to parse is
    // either another Misc, or we will hit the end of the document.
    endMisc,

    // The end of the document (and the grammar) has been reached.
    documentEnd
}


// Similar to startsWith except that it consumes the part of the range that
// matches. It also deals with incrementing text.pos.col.
//
// It is assumed that there are no newlines.
bool stripStartsWith(Text)(ref Text text, string needle)
{
    static if(hasLength!(Text.Input))
    {
        if(text.input.length < needle.length)
            return false;

        // This branch is separate so that we can take advantage of whatever
        // speed boost comes from comparing strings directly rather than
        // comparing individual characters.
        static if(isDynamicArray!(Text.Input) && is(Unqual!(ElementEncodingType!(Text.Input)) == char))
        {
            if(text.input.source[0 .. needle.length] != needle)
                return false;
            text.input.popFrontN(needle.length);
        }
        else
        {
            auto orig = text.save;
            foreach(c; needle)
            {
                if(text.input.front != c)
                {
                    text = orig;
                    return false;
                }
                text.input.popFront();
            }
        }
    }
    else
    {
        auto orig = text.save;
        foreach(c; needle)
        {
            if(text.input.empty || text.input.front != c)
            {
                text = orig;
                return false;
            }
            text.input.popFront();
        }
    }

    static if(Text.config.posType == PositionType.lineAndCol)
        text.pos.col += needle.length;

    return true;
}

unittest
{
    import core.exception : AssertError;
    import std.exception : enforce;

    static void test(alias func)(string origHaystack, string needle, string remainder, bool startsWith,
                                 int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto text = testParser!config(haystack.save);
                enforce!AssertError(text.stripStartsWith(needle) == startsWith, "unittest failure 1", __FILE__, line);
                enforce!AssertError(equalCU(text.input, remainder), "unittest failure 2", __FILE__, line);
                enforce!AssertError(text.pos == pos, "unittest failure 3", __FILE__, line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto text = testParser!config(haystack.save);
                text.pos.line += 3;
                static if(i == 0)
                    text.pos.col += 7;
                enforce!AssertError(text.stripStartsWith(needle) == startsWith, "unittest failure 4", __FILE__, line);
                enforce!AssertError(equalCU(text.input, remainder), "unittest failure 5", __FILE__, line);
                enforce!AssertError(text.pos == pos, "unittest failure 6", __FILE__, line);
            }
        }}
    }

    static foreach(func; testRangeFuncs)
    {
        test!func("hello world", "hello", " world", true, 1, "hello".length + 1);
        test!func("hello world", "hello world", "", true, 1, "hello world".length + 1);
        test!func("hello world", "foo", "hello world", false, 1, 1);
        test!func("hello world", "hello sally", "hello world", false, 1, 1);
        test!func("hello world", "hello world ", "hello world", false, 1, 1);
    }
}


// Strips whitespace while dealing with text.pos accordingly. Newlines are not
// ignored.
// Returns whether any whitespace was stripped.
bool stripWS(Text)(ref Text text)
{
    enum hasLengthAndCol = hasLength!(Text.Input) && Text.config.posType == PositionType.lineAndCol;

    bool strippedSpace = false;

    static if(hasLengthAndCol)
        size_t lineStart = text.input.length;

    loop: while(!text.input.empty)
    {
        switch(text.input.front)
        {
            case ' ':
            case '\t':
            case '\r':
            {
                strippedSpace = true;
                text.input.popFront();
                static if(!hasLength!(Text.Input))
                    nextCol!(Text.config)(text.pos);
                break;
            }
            case '\n':
            {
                strippedSpace = true;
                text.input.popFront();
                static if(hasLengthAndCol)
                    lineStart = text.input.length;
                nextLine!(Text.config)(text.pos);
                break;
            }
            default: break loop;
        }
    }

    static if(hasLengthAndCol)
        text.pos.col += lineStart - text.input.length;

    return strippedSpace;
}

unittest
{
    import core.exception : AssertError;
    import std.exception : enforce;

    static void test(alias func)(string origHaystack, string remainder, bool stripped,
                                 int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto text = testParser!config(haystack.save);
                enforce!AssertError(text.stripWS() == stripped, "unittest failure 1", __FILE__, line);
                enforce!AssertError(equalCU(text.input, remainder), "unittest failure 2", __FILE__, line);
                enforce!AssertError(text.pos == pos, "unittest failure 3", __FILE__, line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto text = testParser!config(haystack.save);
                text.pos.line += 3;
                static if(i == 0)
                    text.pos.col += 7;
                enforce!AssertError(text.stripWS() == stripped, "unittest failure 4", __FILE__, line);
                enforce!AssertError(equalCU(text.input, remainder), "unittest failure 5", __FILE__, line);
                enforce!AssertError(text.pos == pos, "unittest failure 6", __FILE__, line);
            }
        }}
    }

    static foreach(func; testRangeFuncs)
    {
        test!func("  \t\rhello world", "hello world", true, 1, 5);
        test!func("  \n \n \n  \nhello world", "hello world", true, 5, 1);
        test!func("  \n \n \n  \n  hello world", "hello world", true, 5, 3);
        test!func("hello world", "hello world", false, 1, 1);
    }
}


// Returns a slice (or takeExactly) of text.input up to but not including the
// given needle, removing both that slice and the given needle from text.input
// in the process. If the needle is not found, then an XMLParsingException is
// thrown.
auto takeUntilAndDrop(string needle, Text)(ref Text text)
{
    return _takeUntil!(true, needle, Text)(text);
}

unittest
{
    import core.exception : AssertError;
    import std.algorithm.comparison : equal;
    import std.exception : assertThrown, enforce;

    static void test(alias func, string needle)(string origHaystack, string expected, string remainder,
                                                int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto text = testParser!config(haystack.save);
                enforce!AssertError(equal(text.takeUntilAndDrop!needle(), expected),
                                    "unittest failure 1", __FILE__, line);
                enforce!AssertError(equal(text.input, remainder), "unittest failure 2", __FILE__, line);
                enforce!AssertError(text.pos == pos, "unittest failure 3", __FILE__, line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto text = testParser!config(haystack.save);
                text.pos.line += 3;
                static if(i == 0)
                    text.pos.col += 7;
                enforce!AssertError(equal(text.takeUntilAndDrop!needle(), expected),
                                    "unittest failure 4", __FILE__, line);
                enforce!AssertError(equal(text.input, remainder), "unittest failure 5", __FILE__, line);
                enforce!AssertError(text.pos == pos, "unittest failure 6", __FILE__, line);
            }
        }}
    }

    static void testFail(alias func, string needle)(string origHaystack, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            auto text = testParser!config(haystack.save);
            assertThrown!XMLParsingException(text.takeUntilAndDrop!needle(), "unittest failure", __FILE__, line);
        }}
    }

    static foreach(func; testRangeFuncs)
    {
        {
            auto haystack = "hello world";
            enum needle = "world";

            static foreach(i; 1 .. needle.length)
                test!(func, needle[0 .. i])(haystack, "hello ", needle[i .. $], 1, 7 + i);
        }
        test!(func, "l")("lello world", "", "ello world", 1, 2);
        test!(func, "ll")("lello world", "le", "o world", 1, 5);
        test!(func, "le")("llello world", "l", "llo world", 1, 4);
        {
            import std.utf : codeLength;
            auto haystack = "プログラミング in D is great indeed";
            enum len = cast(int)codeLength!(ElementEncodingType!(typeof(func(haystack))))("プログラミング in D is ");
            enum needle = "great";
            enum remainder = "great indeed";

            static foreach(i; 1 .. needle.length)
                test!(func, needle[0 .. i])(haystack, "プログラミング in D is ", remainder[i .. $], 1, len + i + 1);
        }
        static foreach(haystack; ["", "a", "hello"])
            testFail!(func, "x")(haystack);
        static foreach(haystack; ["", "l", "lte", "world", "nomatch"])
            testFail!(func, "le")(haystack);
        static foreach(haystack; ["", "w", "we", "wew", "bwe", "we b", "hello we go", "nomatch"])
            testFail!(func, "web")(haystack);
    }
}

// Variant of takeUntilAndDrop which does not return a slice. It's intended for
// when the config indicates that something should be skipped.
void skipUntilAndDrop(string needle, Text)(ref Text text)
{
    _takeUntil!(false, needle, Text)(text);
}

unittest
{
    import core.exception : AssertError;
    import std.algorithm.comparison : equal;
    import std.exception : assertNotThrown, assertThrown, enforce;

    static void test(alias func, string needle)(string origHaystack, string remainder,
                                                int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto text = testParser!config(haystack.save);
                assertNotThrown!XMLParsingException(text.skipUntilAndDrop!needle(), "unittest failure 1",
                                                    __FILE__, line);
                enforce!AssertError(equal(text.input, remainder), "unittest failure 2", __FILE__, line);
                enforce!AssertError(text.pos == pos, "unittest failure 3", __FILE__, line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto text = testParser!config(haystack.save);
                text.pos.line += 3;
                static if(i == 0)
                    text.pos.col += 7;
                assertNotThrown!XMLParsingException(text.skipUntilAndDrop!needle(), "unittest failure 4",
                                                    __FILE__, line);
                enforce!AssertError(equal(text.input, remainder), "unittest failure 5", __FILE__, line);
                enforce!AssertError(text.pos == pos, "unittest failure 6", __FILE__, line);
            }
        }}
    }

    static void testFail(alias func, string needle)(string origHaystack, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            auto text = testParser!config(haystack.save);
            assertThrown!XMLParsingException(text.skipUntilAndDrop!needle(), "unittest failure", __FILE__, line);
        }}
    }

    static foreach(func; testRangeFuncs)
    {
        {
            enum needle = "world";
            static foreach(i; 1 .. needle.length)
                test!(func, needle[0 .. i])("hello world", needle[i .. $], 1, 7 + i);
        }

        test!(func, "l")("lello world", "ello world", 1, 2);
        test!(func, "ll")("lello world", "o world", 1, 5);
        test!(func, "le")("llello world", "llo world", 1, 4);

        {
            import std.utf : codeLength;
            auto haystack = "プログラミング in D is great indeed";
            enum len = cast(int)codeLength!(ElementEncodingType!(typeof(func(haystack))))("プログラミング in D is ");
            enum needle = "great";
            enum remainder = "great indeed";

            static foreach(i; 1 .. needle.length)
                test!(func, needle[0 .. i])(haystack, remainder[i .. $], 1, len + i + 1);
        }

        static foreach(haystack; ["", "a", "hello"])
            testFail!(func, "x")(haystack);
        static foreach(haystack; ["", "l", "lte", "world", "nomatch"])
            testFail!(func, "le")(haystack);
        static foreach(haystack; ["", "w", "we", "wew", "bwe", "we b", "hello we go", "nomatch"])
            testFail!(func, "web")(haystack);
    }
}

auto _takeUntil(bool retSlice, string needle, Text)(ref Text text)
{
    import std.algorithm : find;
    import std.ascii : isWhite;
    import std.range : takeExactly;

    static assert(needle.find!isWhite().empty);

    enum trackTakeLen = retSlice || Text.config.posType == PositionType.lineAndCol;

    auto orig = text.input.save;
    bool found = false;

    static if(trackTakeLen)
        size_t takeLen = 0;

    static if(Text.config.posType == PositionType.lineAndCol)
        size_t lineStart = 0;

    loop: while(!text.input.empty)
    {
        switch(text.input.front)
        {
            case cast(ElementType!(Text.Input))needle[0]:
            {
                static if(needle.length == 1)
                {
                    found = true;
                    text.input.popFront();
                    break loop;
                }
                else static if(needle.length == 2)
                {
                    text.input.popFront();
                    if(!text.input.empty && text.input.front == needle[1])
                    {
                        found = true;
                        text.input.popFront();
                        break loop;
                    }
                    static if(trackTakeLen)
                        ++takeLen;
                    continue;
                }
                else
                {
                    text.input.popFront();
                    auto saved = text.input.save;
                    foreach(i, c; needle[1 .. $])
                    {
                        if(text.input.empty)
                        {
                            static if(trackTakeLen)
                                takeLen += i + 1;
                            break loop;
                        }
                        if(text.input.front != c)
                        {
                            text.input = saved;
                            static if(trackTakeLen)
                                ++takeLen;
                            continue loop;
                        }
                        text.input.popFront();
                    }
                    found = true;
                    break loop;
                }
            }
            static if(Text.config.posType != PositionType.none)
            {
                case '\n':
                {
                    static if(trackTakeLen)
                        ++takeLen;
                    nextLine!(Text.config)(text.pos);
                    static if(Text.config.posType == PositionType.lineAndCol)
                        lineStart = takeLen;
                    break;
                }
            }
            default:
            {
                static if(trackTakeLen)
                    ++takeLen;
                break;
            }
        }

        text.input.popFront();
    }

    static if(Text.config.posType == PositionType.lineAndCol)
        text.pos.col += takeLen - lineStart + needle.length;
    if(!found)
        throw new XMLParsingException("Failed to find: " ~ needle, text.pos);

    static if(retSlice)
        return takeExactly(orig, takeLen);
}


// Okay, this name kind of sucks, because it's too close to skipUntilAndDrop,
// but I'd rather do this than be passing template arguments to choose between
// behaviors - especially when the logic is so different. It skips until it
// reaches one of the delimiter characters. If it finds one of them, then the
// first character in the input is the delimiter that was found, and if it
// doesn't find either, then it throws.
template skipToOneOf(delims...)
{
    static foreach(delim; delims)
    {
        static assert(is(typeof(delim) == char));
        static assert(!isSpace(delim));
    }

    void skipToOneOf(Text)(ref Text text)
    {
        while(!text.input.empty)
        {
            switch(text.input.front)
            {
                foreach(delim; delims)
                    case delim: return;
                static if(Text.config.posType != PositionType.none)
                {
                    case '\n':
                    {
                        nextLine!(Text.config)(text.pos);
                        text.input.popFront();
                        break;
                    }
                }
                default:
                {
                    popFrontAndIncCol(text);
                    break;
                }
            }
        }
        throw new XMLParsingException("Prematurely reached end of document", text.pos);
    }
}

unittest
{
    import core.exception : AssertError;
    import std.algorithm.comparison : equal;
    import std.exception : assertNotThrown, assertThrown, enforce;

    static void test(alias func, delims...)(string origHaystack, string remainder,
                                            int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto text = testParser!config(haystack.save);
                assertNotThrown!XMLParsingException(text.skipToOneOf!delims(), "unittest 1", __FILE__, line);
                enforce!AssertError(equal(text.input, remainder), "unittest failure 2", __FILE__, line);
                enforce!AssertError(text.pos == pos, "unittest failure 3", __FILE__, line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto text = testParser!config(haystack.save);
                text.pos.line += 3;
                static if(i == 0)
                    text.pos.col += 7;
                assertNotThrown!XMLParsingException(text.skipToOneOf!delims(), "unittest 4", __FILE__, line);
                enforce!AssertError(equal(text.input, remainder), "unittest failure 5", __FILE__, line);
                enforce!AssertError(text.pos == pos, "unittest failure 6", __FILE__, line);
            }
        }}
    }

    static void testFail(alias func, delims...)(string origHaystack, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            auto text = testParser!config(haystack.save);
            assertThrown!XMLParsingException(text.skipToOneOf!delims(), "unittest failure", __FILE__, line);
        }}
    }

    static foreach(func; testRangeFuncs)
    {
        test!(func, 'o', 'w')("hello world", "o world", 1, 5);
        test!(func, 'r', 'w', '1', '+', '*')("hello world", "world", 1, 7);
        testFail!(func, 'a', 'b')("hello world");
        test!(func, 'z', 'y')("abc\n\n\n  \n\n   wxyzzy \nf\ng", "yzzy \nf\ng", 6, 6);
        test!(func, 'o', 'g')("abc\n\n\n  \n\n   wxyzzy \nf\ng", "g", 8, 1);
        {
            import std.utf : codeLength;
            auto haystack = "プログラミング in D is great indeed";
            enum len = cast(int)codeLength!(ElementEncodingType!(typeof(func(haystack))))("プログラミング in D is ");
            test!(func, 'g', 'x')(haystack, "great indeed", 1, len + 1);
        }
    }
}


// The front of the input should be text surrounded by single or double quotes.
// This returns a slice of the input containing that text, and the input is
// advanced to one code unit beyond the quote.
auto takeEnquotedText(Text)(ref Text text)
{
    checkNotEmpty(text);
    immutable quote = text.input.front;
    static foreach(quoteChar; [`"`, `'`])
    {
        // This would be a bit simpler if takeUntilAndDrop took a runtime
        // argument, but in all other cases, a compile-time argument makes more
        // sense, so this seemed like a reasonable way to handle this one case.
        if(quote == quoteChar[0])
        {
            popFrontAndIncCol(text);
            return takeUntilAndDrop!quoteChar(text);
        }
    }
    throw new XMLParsingException("Expected quoted text", text.pos);
}

unittest
{
    import core.exception : AssertError;
    import std.algorithm.comparison : equal;
    import std.exception : assertThrown, enforce;
    import std.range : only;

    static void test(alias func)(string origHaystack, string expected, string remainder,
                                 int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto text = testParser!config(haystack.save);
                enforce!AssertError(equal(takeEnquotedText(text), expected), "unittest failure 1", __FILE__, line);
                enforce!AssertError(equal(text.input, remainder), "unittest failure 2", __FILE__, line);
                enforce!AssertError(text.pos == pos, "unittest failure 3", __FILE__, line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto text = testParser!config(haystack.save);
                text.pos.line += 3;
                static if(i == 0)
                    text.pos.col += 7;
                enforce!AssertError(equal(takeEnquotedText(text), expected), "unittest failure 3", __FILE__, line);
                enforce!AssertError(equal(text.input, remainder), "unittest failure 4", __FILE__, line);
                enforce!AssertError(text.pos == pos, "unittest failure 3", __FILE__, line);
            }
        }}
    }

    static void testFail(alias func)(string origHaystack, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            auto text = testParser!config(haystack.save);
            assertThrown!XMLParsingException(text.takeEnquotedText(), "unittest failure", __FILE__, line);
        }}
    }

    static foreach(func; testRangeFuncs)
    {
        foreach(quote; only("\"", "'"))
        {
            test!func(quote ~ quote, "", "", 1, 3);
            test!func(quote ~ "hello world" ~ quote, "hello world", "", 1, 14);
            test!func(quote ~ "hello world" ~ quote ~ " foo", "hello world", " foo", 1, 14);
            {
                import std.utf : codeLength;
                auto haystack = quote ~ "プログラミング " ~ quote ~ "in D";
                enum len = cast(int)codeLength!(ElementEncodingType!(typeof(func(haystack))))("プログラミング ");
                test!func(haystack, "プログラミング ", "in D", 1, len + 3);
            }
        }

        foreach(str; only(`hello`, `"hello'`, `"hello`, `'hello"`, `'hello`, ``, `"'`, `"`, `'"`, `'`))
            testFail!func(str);
    }
}


// This removes a name per the Name grammar rule from the front of the input and
// returns it.
// The parsing continues until either one of the given delimiters or an XML
// whitespace character is encountered. The delimiter/whitespace is not returned
// as part of the name and is left at the front of the input.
template takeName(delims...)
{
    static foreach(delim; delims)
    {
        static assert(is(typeof(delim) == char), delim);
        static assert(!isSpace(delim));
    }

    auto takeName(Text)(ref Text text)
    {
        import std.format : format;
        import std.range : takeExactly;
        import std.utf : decodeFront, UseReplacementDchar;

        assert(!text.input.empty);

        auto orig = text.input.save;
        size_t takeLen;
        {
            immutable decodedC = text.input.decodeFront!(UseReplacementDchar.yes)(takeLen);
            if(!isNameStartChar(decodedC))
                throw new XMLParsingException(format!"Name contains invalid character: '%s'"(decodedC), text.pos);
        }

        if(text.input.empty)
        {
            static if(Text.config.posType == PositionType.lineAndCol)
                text.pos.col += takeLen;
            return takeExactly(orig, takeLen);
        }

        loop: while(true)
        {
            immutable c = text.input.front;
            if(isSpace(c))
                break;
            static foreach(delim; delims)
            {
                if(c == delim)
                    break loop;
            }

            size_t numCodeUnits;
            immutable decodedC = text.input.decodeFront!(UseReplacementDchar.yes)(numCodeUnits);
            if(!isNameChar(decodedC))
                throw new XMLParsingException(format!"Name contains invalid character: '%s'"(decodedC), text.pos);
            takeLen += numCodeUnits;

            if(text.input.empty)
                break;
        }

        if(takeLen == 0)
            throw new XMLParsingException("Name cannot be empty", text.pos);

        static if(Text.config.posType == PositionType.lineAndCol)
            text.pos.col += takeLen;

        return takeExactly(orig, takeLen);
    }
}

unittest
{
    import core.exception : AssertError;
    import std.algorithm.comparison : equal;
    import std.exception : assertThrown, enforce;

    static void test(alias func, delim...)(string origHaystack, string expected, string remainder,
                                           int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto text = testParser!config(haystack.save);
                enforce!AssertError(equal(text.takeName!delim(), expected),
                                    "unittest failure 1", __FILE__, line);
                enforce!AssertError(equal(text.input, remainder), "unittest failure 2", __FILE__, line);
                enforce!AssertError(text.pos == pos, "unittest failure 3", __FILE__, line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto text = testParser!config(haystack.save);
                text.pos.line += 3;
                static if(i == 0)
                    text.pos.col += 7;
                enforce!AssertError(equal(text.takeName!delim(), expected),
                                    "unittest failure 4", __FILE__, line);
                enforce!AssertError(equal(text.input, remainder), "unittest failure 5", __FILE__, line);
                enforce!AssertError(text.pos == pos, "unittest failure 6", __FILE__, line);
            }
        }}
    }

    static void testFail(alias func, delim...)(string origHaystack, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            auto text = testParser!config(haystack.save);
            assertThrown!XMLParsingException(text.takeName!delim(), "unittest failure", __FILE__, line);
        }}
    }

    static foreach(func; testRangeFuncs)
    {
        static foreach(str; ["hello", "プログラミング", "h_:llo-.42", "_.", "_-", "_42"])
        {{
            import std.utf : codeLength;
            enum len = cast(int)codeLength!(ElementEncodingType!(typeof(func("hello"))))(str);

            static foreach(remainder; ["", " ", "\t", "\r", "\n", " foo", "\tfoo", "\rfoo", "\nfoo",  "  foo \n \r "])
            {{
                enum strRem = str ~ remainder;
                enum delimRem = '>' ~ remainder;
                enum hay = str ~ delimRem;
                test!func(strRem, str, remainder, 1, len + 1);
                test!(func, '=')(strRem, str, remainder, 1, len + 1);
                test!(func, '>', '|')(hay, str, delimRem, 1, len + 1);
                test!(func, '|', '>')(hay, str, delimRem, 1, len + 1);
            }}
        }}

        static foreach(haystack; [" ", "<", "foo!", "foo!<"])
        {
            testFail!func(haystack);
            testFail!func(haystack ~ '>');
            testFail!(func, '?')(haystack);
            testFail!(func, '=')(haystack ~ '=');
        }

        testFail!(func, '>')(">");
        testFail!(func, '?')("?");

        static foreach(haystack; ["42", ".", ".a"])
        {
            testFail!func(haystack);
            testFail!(func, '>')(haystack);
        }
    }
}


// S := (#x20 | #x9 | #xD | #XA)+
bool isSpace(C)(C c)
    if(isSomeChar!C)
{
    switch(c)
    {
        case ' ':
        case '\t':
        case '\r':
        case '\n': return true;
        default : return false;
    }
}

unittest
{
    foreach(char c; char.min .. char.max)
    {
        if(c == ' ' || c == '\t' || c == '\r' || c == '\n')
            assert(isSpace(c));
        else
            assert(!isSpace(c));
    }
    foreach(wchar c; wchar.min .. wchar.max / 100)
    {
        if(c == ' ' || c == '\t' || c == '\r' || c == '\n')
            assert(isSpace(c));
        else
            assert(!isSpace(c));
    }
    foreach(dchar c; dchar.min .. dchar.max / 1000)
    {
        if(c == ' ' || c == '\t' || c == '\r' || c == '\n')
            assert(isSpace(c));
        else
            assert(!isSpace(c));
    }
}


// NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] |
//                   [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] |
//                   [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
bool isNameStartChar(dchar c)
{
    import std.ascii : isAlpha;

    if(isAlpha(c))
        return true;
    if(c == ':' || c == '_')
        return true;
    if(c >= 0xC0 && c <= 0xD6)
        return true;
    if(c >= 0xD8 && c <= 0xF6)
        return true;
    if(c >= 0xF8 && c <= 0x2FF)
        return true;
    if(c >= 0x370 && c <= 0x37D)
        return true;
    if(c >= 0x37F && c <= 0x1FFF)
        return true;
    if(c >= 0x200C && c <= 0x200D)
        return true;
    if(c >= 0x2070 && c <= 0x218F)
        return true;
    if(c >= 0x2C00 && c <= 0x2FEF)
        return true;
    if(c >= 0x3001 && c <= 0xD7FF)
        return true;
    if(c >= 0xF900 && c <= 0xFDCF)
        return true;
    if(c >= 0xFDF0 && c <= 0xFFFD)
        return true;
    if(c >= 0x10000 && c <= 0xEFFFF)
        return true;
    return false;
}

unittest
{
    import std.range : only;
    import std.typecons : tuple;

    foreach(c; char.min .. cast(char)128)
    {
        if(c == ':' || c == '_' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z')
            assert(isNameStartChar(c));
        else
            assert(!isNameStartChar(c));
    }

    foreach(t; only(tuple(0xC0, 0xD6),
                    tuple(0xD8, 0xF6),
                    tuple(0xF8, 0x2FF),
                    tuple(0x370, 0x37D),
                    tuple(0x37F, 0x1FFF),
                    tuple(0x200C, 0x200D),
                    tuple(0x2070, 0x218F),
                    tuple(0x2C00, 0x2FEF),
                    tuple(0x3001, 0xD7FF),
                    tuple(0xF900, 0xFDCF),
                    tuple(0xFDF0, 0xFFFD),
                    tuple(0x10000, 0xEFFFF)))
    {
        assert(!isNameStartChar(t[0] - 1));
        assert(isNameStartChar(t[0]));
        assert(isNameStartChar(t[0] + 1));
        assert(isNameStartChar(t[0] + (t[1] - t[0])));
        assert(isNameStartChar(t[1] - 1));
        assert(isNameStartChar(t[1]));
        assert(!isNameStartChar(t[1] + 1));
    }
}


// NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] |
//                   [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] |
//                   [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
// NameChar      ::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
bool isNameChar(dchar c)
{
    import std.ascii : isDigit;
    return isNameStartChar(c) || isDigit(c) || c == '-' || c == '.' || c == 0xB7 ||
           c >= 0x0300 && c <= 0x036F || c >= 0x203F && c <= 0x2040;
}

unittest
{
    import std.ascii : isAlphaNum;
    import std.range : only;
    import std.typecons : tuple;

    foreach(c; char.min .. cast(char)129)
    {
        if(isAlphaNum(c) || c == ':' || c == '_' || c == '-' || c == '.')
            assert(isNameChar(c));
        else
            assert(!isNameChar(c));
    }

    assert(!isNameChar(0xB7 - 1));
    assert(isNameChar(0xB7));
    assert(!isNameChar(0xB7 + 1));

    foreach(t; only(tuple(0xC0, 0xD6),
                    tuple(0xD8, 0xF6),
                    tuple(0xF8, 0x037D), // 0xF8 - 0x2FF, 0x0300 - 0x036F, 0x37 - 0x37D
                    tuple(0x37F, 0x1FFF),
                    tuple(0x200C, 0x200D),
                    tuple(0x2070, 0x218F),
                    tuple(0x2C00, 0x2FEF),
                    tuple(0x3001, 0xD7FF),
                    tuple(0xF900, 0xFDCF),
                    tuple(0xFDF0, 0xFFFD),
                    tuple(0x10000, 0xEFFFF),
                    tuple(0x203F, 0x2040)))
    {
        assert(!isNameChar(t[0] - 1));
        assert(isNameChar(t[0]));
        assert(isNameChar(t[0] + 1));
        assert(isNameChar(t[0] + (t[1] - t[0])));
        assert(isNameChar(t[1] - 1));
        assert(isNameChar(t[1]));
        assert(!isNameChar(t[1] + 1));
    }
}


pragma(inline, true) void popFrontAndIncCol(Text)(ref Text text)
{
    text.input.popFront();
    nextCol!(Text.config)(text.pos);
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

pragma(inline, true) void checkNotEmpty(Text)(ref Text text, size_t line = __LINE__)
{
    if(text.input.empty)
        throw new XMLParsingException("Prematurely reached end of document", text.pos, __FILE__, line);
}


version(unittest)
{
    // Wrapping it like this rather than assigning testRangeFuncs directly
    // allows us to avoid having the imports be at module-level, which is
    // generally not desirable with version(unittest).
    alias testRangeFuncs = _testRangeFuncs!();
    template _testRangeFuncs()
    {
        import std.conv : to;
        import std.algorithm : filter;
        import std.meta : AliasSeq;
        import std.utf : byCodeUnit;
        alias _testRangeFuncs = AliasSeq!(a => to!string(a), a => to!wstring(a), a => to!dstring(a),
                                          a => filter!"true"(a), a => fwdCharRange(a), a => fwdRefCharRange(a),
                                          a => raCharRange(a), a => rasCharRange(a), a => rasRefCharRange(a),
                                          a => byCodeUnit(a));
    }

    enum testConfigs = [ Config.init, makeConfig(PositionType.line), makeConfig(PositionType.none) ];
}
