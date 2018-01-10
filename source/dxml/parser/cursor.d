// Written in the D programming language

/++
    This implements a STaX parser for XML 1.0 (which will work with XML 1.1
    documents assuming that they don't use any 1.1-specific features). For the
    sake of simplicity, sanity, and efficiency, the DTD section is not
    supported beyond what is required to parse past it.

    Copyright: Copyright 2017 - 2018
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   Jonathan M Davis
    Source:    $(LINK_TO_SRC dxml/parser/_cursor.d)
  +/
module dxml.parser.cursor;

import std.range.primitives;
import std.range : takeExactly;
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
    in the text the invalid XML was found). $(LREF _PositionType.none) is the
    most efficient but least informative, whereas
    $(LREF _PositionType.lineAndCol) is the most informative but least
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
        Whether $(LREF EntityType.text) entities that contain only whitespace
        will be skipped.

        With $(LREF SkipContentWS.no), XML that is formatted with indenting and
        newlines and the like will contain lots of $(LREF EntityType.text)
        entities which are just whitespace, but with $(LREF SkipContentWS.yes),
        all of the formatting is lost when parsing, and if the program wants to
        treat that whitespace as significant, then it has a problem. Which is
        better of course depends on what the application is doing when parsing
        the XML.

        However, note that $(I some) formatting will be lost regardless,
        because any whitespace which is not part of an $(LREF EntityType.text)
        entity will be parsed without being communicated to the program
        (e.g. the program using $(LREF parseXML) won't know what whitespace
        was between $(LREF EntityType.pi) entities and won't know what kind of
        whitespace or how much there was between the name of a start tag and
        its attributes).

        Note that XML only considers $(D ' '), $(D '\t'), $(D '\n'), and
        $(D '\r') to be whitespace. So, those are the only character skipped
        with $(LREF StripContentsWS.yes), and those are the characters that
        $(LREF EntityCursor) expects at any point in the XML that is supposed
        to contain whitespace.

        Defaults to $(D SkipContentWS.yes).

        See_Also: $(LREF EntityCursor.text)
      +/
    auto skipContentWS = SkipContentWS.yes;

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
        and end tag with nothing in between. But since some code will care about
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
            auto cursor = parseXML("<root></root>");
            assert(cursor.next() == EntityType.elementStart);
            assert(cursor.name == "root");
            assert(cursor.next() == EntityType.elementEnd);
            assert(cursor.name == "root");
            assert(cursor.next() == EntityType.documentEnd);
        }
        {
            // No difference if the tags are already split.
            auto cursor = parseXML!configSplitYes("<root></root>");
            assert(cursor.next() == EntityType.elementStart);
            assert(cursor.name == "root");
            assert(cursor.next() == EntityType.elementEnd);
            assert(cursor.name == "root");
            assert(cursor.next() == EntityType.documentEnd);
        }
        {
            // This treats <root></root> and <root/> as distinct.
            auto cursor = parseXML("<root/>");
            assert(cursor.next() == EntityType.elementEmpty);
            assert(cursor.name == "root");
            assert(cursor.next() == EntityType.documentEnd);
        }
        {
            // This is parsed as if it were <root></root> insead of <root/>.
            auto cursor = parseXML!configSplitYes("<root/>");
            assert(cursor.next() == EntityType.elementStart);
            assert(cursor.name == "root");
            assert(cursor.next() == EntityType.elementEnd);
            assert(cursor.name == "root");
            assert(cursor.next() == EntityType.documentEnd);
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

/// See_Also: $(LREF2 skipContentWS, Config)
alias SkipContentWS = Flag!"SkipContentWS";

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
        assert(config.skipContentWS == Config.init.skipContentWS);
        assert(config.splitEmpty == Config.init.splitEmpty);
        assert(config.posType == Config.init.posType);
    }
    {
        auto config = makeConfig(SkipComments.yes, PositionType.none);
        assert(config.skipComments == SkipComments.yes);
        assert(config.skipPI == Config.init.skipPI);
        assert(config.skipContentWS == Config.init.skipContentWS);
        assert(config.splitEmpty == Config.init.splitEmpty);
        assert(config.posType == PositionType.none);
    }
    {
        auto config = makeConfig(SplitEmpty.yes,
                                 SkipComments.yes,
                                 PositionType.line);
        assert(config.skipComments == SkipComments.yes);
        assert(config.skipContentWS == Config.init.skipContentWS);
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
    static assert(simpleXML.skipContentWS == SkipContentWS.yes);
    static assert(simpleXML.splitEmpty == SplitEmpty.yes);
    static assert(simpleXML.posType == PositionType.lineAndCol);
}


/++
    Represents the type of an XML entity. Used by EntityCursor.
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
        parseXML has been called to create the $(LREF EntityCursor), but no
        parsing has been done.
      +/
    documentStart,

    /++
        The end of the document has been reached. There are no more entities to
        parse. Calling any functions of $(LREF EntityCursor) after this has been
        reached is an error, and no node of $(REF EntityTree, dxml, parser, dom)
        will have $(LREF EntityType._documentEnd) as its
        $(type, EntityTree.type, dxml, parser, dom).
      +/
    documentEnd,

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

    EntityCursor is essentially a
    $(LINK2 https://en.wikipedia.org/wiki/StAX, StAX) parser, though it evolved
    into that rather than being based on what Java did, so its API is likely
    to differ from other implementations.

    The resulting EntityCursor is similar to an input range with how it's
    iterated until it's consumed, and its state cannot be saved (since
    unfortunately, saving would essentially require duplicating the entire
    parser, including all of the memory that it's allocated), but because there
    are essentially multiple ways to pop the front (e.g. choosing to skip all of
    the contents between a start tag and end tag instead of processing it), the
    input range API didn't seem to appropriate, even if the result functions
    similarly.

    Also, unlike a range the, "front" is integrated into the EntityCursor rather
    than being a value that can be extracted. So, while an entity can be queried
    while it is the "front", it can't be kept around after the EntityCursor has
    moved to the next entity. Only the information that has been queried for
    that entity can be retained (e.g. its $(LREF2 name, _EntityCursor),
    $(LREF2 attributes, _EntityCursor), or $(LREF2 text, _EntityCursor)ual value).
    While that does place some restrictions on how an algorithm operating on an
    EntityCursor can operate, it does allow for more efficient processing.

    If invalid XML is encountered at any point during the parsing process, an
    $(LREF XMLParsingException) will be thrown.

    However, note that EntityCursor does not generally care about XML validation
    beyond what is required to correctly parse what it has been told to parse.
    In particular, any portions that are skipped (either due to the values in
    the $(LREF Config) or because a function such as
    $(LREF skipContents) is called) will only be validated enough to correctly
    determine where those portions terminated. Similarly, if the functions to
    process the value of an entity are not called (e.g.
    $(LREF2 attributes, _EntityCursor) for $(LREF EntityType.elementStart) and
    $(LREF2 xmlSpec, _EntityCursor) for $(LREF EntityType.xmlSpec)), then those
    portions of the XML will not be validated beyond what is required to iterate
    to the next entity.

    A possible enhancement would be to add a $(D validateXML) function that
    corresponds to parseXML and fully validates the XML, but for now, no such
    function exists.

    EntityCursor is a reference type.

    Note that while EntityCursor is not $(D @nogc), it is designed to allocate
    very miminally. The parser state is allocated on the heap so that it is a
    reference type, and it has to allocate on the heap to retain a stack of the
    names of element tags so that it can validate the XML properly as well as
    provide the full path for a given entity, but it does no allocation
    whatsoever with the text that it's given. It operates entirely on slices
    (or the result of $(PHOBOS_REF takeExactly, std, range)), and that's what
    it returns from any function that returns a portion of the XML. Helper
    functions (such as $(LREF cleanText)) may allocate, but if so, their
    documentation makes that clear. The only other case where it may allocate
    is when throwing an $(LREF XMLException).

    See_Also: $(MREF dxml, parser, dom)
  +/
struct EntityCursor(Config cfg, R)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
public:

    import std.algorithm : canFind;
    import std.range : only;
    import std.typecons : Nullable;

    private enum compileInTests = is(R == EntityCompileTests);

    /// The Config used for when parsing the XML.
    alias config = cfg;

    /++
        The type used when any slice of the original text is used. If $(D R)
        is a string or supports slicing, then SliceOfR is the same as $(D R);
        otherwise, it's the result of calling
        $(PHOBOS_REF takeExactly, std, range) on the text.
      +/
    static if(isDynamicArray!R || hasSlicing!R)
        alias SliceOfR = R;
    else
        alias SliceOfR = typeof(takeExactly(R.init, 42));

    // TODO re-enable these. Whenever EntityCursor doesn't compile correctly due
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

        static assert(is(EntityCursor!(Config.init, string).SliceOfR == string));

        auto range = filter!(a => true)("some xml");

        static assert(is(EntityCursor!(Config.init, typeof(range)).SliceOfR ==
                         typeof(takeExactly(range, 4))));
    }
    +/


    /++
        Move to the next entity.

        The next entity is the next one that is linearly in the XML document.
        So, if the current entity has child entities, the next entity will be
        the child entity, whereas if it has no child entities, it will be the
        next entity at the same level.

        Returns:
            The $(LREF EntityType) of the entity that just became the
            current entity.

            The return value determines which member functions and properties
            are allowed to be called, since some are only appropriate for
            specific entity types (e.g. $(LREF2 attributes, EntityCursor) would
            not be appropriate for $(LREF EntityType.elementEnd)).

            If the return type is $(LREF EntityType.documentEnd), then the
            parser has reached the end of the document, and it is an error to
            call any functions of $(LREF EntityCursor).

        Throws: $(LREF XMLParsingException) on invalid XML.
      +/
    EntityType next()
    {
        final switch(_state.grammarPos) with(GrammarPos)
        {
            case documentStart: _parseDocumentStart(); break;
            case prologMisc1: _parseAtPrologMisc!1(); break;
            case prologMisc2: _parseAtPrologMisc!2(); break;
            case splittingEmpty:
            {
                _state.type = EntityType.elementEnd;
                _state.grammarPos = _state.tagStack.empty ? GrammarPos.endMisc : GrammarPos.contentCharData2;
                break;
            }
            case contentCharData1:
            {
                assert(_state.type == EntityType.elementStart);
                _state.tagStack.push(_state.name);
                _parseAtContentCharData();
                break;
            }
            case contentMid: _parseAtContentMid(); break;
            case contentCharData2: _parseAtContentCharData(); break;
            case endTag: _parseElementEnd(); break;
            case endMisc: _parseAtEndMisc(); break;
            case documentEnd:
                assert(0, "It's invalid to call next when the EntityCursor has reached EntityType.documentEnd");
        }
        return _state.type;
    }


    /++
        Returns the $(LREF EntityType) for the current entity. It's the exact
        same value that was last returned by $(LREF2 next, EntityCursor), but
        it allows for the value to be queried again if need be.
      +/
    @property EntityType type() @safe const pure nothrow @nogc
    {
        return _state.type;
    }


    /++
        Convenience function equivalent to
        $(D cursor.type == EntityType.documentEnd).
      +/
    @property bool empty() @safe const pure nothrow @nogc
    {
        return _state.type == EntityType.documentEnd;
    }


    /++
        Gives the name of the current entity.

        Note that this is the direct name in the XML for this entity and does
        not contain any of the names of any of the parent entities that this
        entity has.

        $(TABLE
            $(TR $(TH Supported $(LREF EntityType)s:))
            $(TR $(TD $(LREF2 elementStart, EntityType)))
            $(TR $(TD $(LREF2 elementEnd, EntityType)))
            $(TR $(TD $(LREF2 elementEmpty, EntityType)))
            $(TR $(TD $(LREF2 pi, EntityType)))
        )

        See_Also: $(LREF path, EntityCursor)$(BR)$(LREF parentPath, EntityCursor)
      +/
    @property SliceOfR name()
    {
        with(EntityType)
            assert(only(elementStart, elementEnd, elementEmpty, pi).canFind(_state.type));
        return stripBCU!R(_state.name.save);
    }


    /++
        Gives the path of the current entity as a lazy range.

        Unlike $(LREF2 name, EntityCursor), this includes the names of all of
        the parent entities. The path does not take into account the fact that
        there may be multiple entities with the same path (e.g. a start tag
        could have multiple start tags in its contents which have the same
        name). The path is simply the list of the names of the parent tags plus
        the name of the current tag.

        Note that the range returned by path is only valid until
        $(LREF next, EntityCursor) is called. Each element in the range will
        continue to be valid, but the range itself will not be (since the range
        refers to the internal stack of tag names, which can change when
        $(LREF next, EntityCursor) is called).

        $(TABLE
            $(TR $(TH Supported $(LREF EntityType)s:))
            $(TR $(TD $(LREF2 elementStart, EntityType)))
            $(TR $(TD $(LREF2 elementEnd, EntityType)))
            $(TR $(TD $(LREF2 elementEmpty, EntityType)))
            $(TR $(TD $(LREF2 pi, EntityType)))
         )

        See_Also: $(LREF name, EntityCursor)$(BR)$(LREF parentPath, EntityCursor)
      +/
    @property auto path()
    {
        with(EntityType)
            assert(only(elementStart, elementEmpty, elementEnd, pi).canFind(_state.type));

        assert(0);
    }


    /++
        Gives the path of the parent entity as a lazy range.

        Unlike $(LREF2 name, EntityCursor), this includes the names of all of
        the parent entities. However, unlike $(LREF2 name, EntityCursor) and
        $(LREF2 path, EntityCursor), parentPath works with all entity types
        (though in the case of root-level elements, it will be empty). Like
        (LREF2 path, EntityCursor), no effort is made to make the path unique.
        It is simply a list of the names of the parent tags.

        Note that the range returned by parentPath is only valid until
        $(LREF next, EntityCursor) is called. Each element in the range will
        continue to be valid, but the range itself will not be (since the range
        refers to the internal stack of tag names, which can change when
        $(LREF next, EntityCursor) is called).

        See_Also: $(LREF name, EntityCursor)$(BR)$(LREF path, EntityCursor)
      +/
    @property auto parentPath()
    {
        with(EntityType)
            assert(only(elementStart, elementEnd, elementEmpty, pi).canFind(_state.type));

        assert(0);
    }


    /++
        Returns a lazy range of attributes for a start tag where each attribute
        is represented as a
        $(D Tuple!($(LREF2 SliceOfR, EntityCursor), "name", $(LREF2 SliceOfR, EntityCursor), "value")).

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
            assert(_state.type == elementStart || _state.type == elementEmpty);

        // STag         ::= '<' Name (S Attribute)* S? '>'
        // Attribute    ::= Name Eq AttValue
        // EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'

        import std.typecons : Tuple;
        alias Attribute = Tuple!(SliceOfR, "name", SliceOfR, "value");

        static struct Range
        {
            @property Attribute front()
            {
                return _front;
            }

            void popFront()
            {
                if(_text.input.empty)
                {
                    empty = true;
                    return;
                }

                auto state = &_text;
                auto name = stripBCU!R(state.takeName!(true, '=')());
                stripWS(state);
                checkNotEmpty(state);
                if(state.input.front != '=')
                    throw new XMLParsingException("= missing", state.pos);
                popFrontAndIncCol(state);
                stripWS(state);

                auto value = stripBCU!R(takeEnquotedText(state));
                stripWS(state);

                _front = Attribute(name, value);
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
            typeof(_state.savedText) _text;
        }

        return Range(_state.savedText);
    }


    /++
        Returns the value of the current entity.

        In the case of $(LREF EntityType.pi), this is the
        text that follows the name, whereas in the other cases, the text is
        the entire contents of the entity (save for the delimeters on the ends
        if that entity has them).

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
            assert(only(cdata, comment, pi, text).canFind(_state.type));
        return stripBCU!R(_state.savedText.input.save);
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
        {
            auto cursor = parseXML(xml);

            // "<?instructionName?>\n" ~
            assert(cursor.next() == EntityType.pi);
            assert(cursor.name == "instructionName");
            assert(cursor.text.empty);

            // "<?foo here is something to say?>\n" ~
            assert(cursor.next() == EntityType.pi);
            assert(cursor.name == "foo");
            assert(cursor.text == "here is something to say");

            // "<root>\n" ~
            assert(cursor.next() == EntityType.elementStart);

            // "    <![CDATA[ Yay! random text >> << ]]>\n" ~
            assert(cursor.next() == EntityType.cdata);
            assert(cursor.text == " Yay! random text >> << ");

            // "    <!-- some random comment -->\n" ~
            assert(cursor.next() == EntityType.comment);
            assert(cursor.text == " some random comment ");

            // "    <p>something here</p>\n" ~
            assert(cursor.next() == EntityType.elementStart);
            assert(cursor.next() == EntityType.text);
            assert(cursor.text == "something here");
            assert(cursor.next() == EntityType.elementEnd);

            // "    <p>\n" ~
            // "       something else\n" ~
            // "       here</p>\n" ~
            assert(cursor.next() == EntityType.elementStart);
            assert(cursor.next() == EntityType.text);
            assert(cursor.text == "\n       something else\n       here");
            assert(cursor.next() == EntityType.elementEnd);

            // "</root>"
            assert(cursor.next() == EntityType.elementEnd);
            assert(cursor.next() == EntityType.documentEnd);
        }
        {
            auto cursor = parseXML!(makeConfig(SkipContentWS.no))(xml);

            // "<?instructionName?>\n" ~
            assert(cursor.next() == EntityType.pi);
            assert(cursor.name == "instructionName");
            assert(cursor.text.empty);

            // "<?foo here is something to say?>\n" ~
            assert(cursor.next() == EntityType.pi);
            assert(cursor.name == "foo");
            assert(cursor.text == "here is something to say");

            // "<root>\n" ~
            assert(cursor.next() == EntityType.elementStart);

            // With SkipContentWS.no, no EntityType.text entities are skipped,
            // even if they only contain whitespace, resulting in a number of
            // entities of type EntityType.text which just contain whitespace
            // if the XML has been formatted to be human readable.
            assert(cursor.next() == EntityType.text);
            assert(cursor.text == "\n    ");

            // "    <![CDATA[ Yay! random text >> << ]]>\n" ~
            assert(cursor.next() == EntityType.cdata);
            assert(cursor.text == " Yay! random text >> << ");
            assert(cursor.next() == EntityType.text);
            assert(cursor.text == "\n    ");

            // "    <!-- some random comment -->\n" ~
            assert(cursor.next() == EntityType.comment);
            assert(cursor.text == " some random comment ");
            assert(cursor.next() == EntityType.text);
            assert(cursor.text == "\n    ");

            // "    <p>something here</p>\n" ~
            assert(cursor.next() == EntityType.elementStart);
            assert(cursor.next() == EntityType.text);
            assert(cursor.text == "something here");
            assert(cursor.next() == EntityType.elementEnd);
            assert(cursor.next() == EntityType.text);
            assert(cursor.text == "\n    ");

            // "    <p>\n" ~
            // "       something else\n" ~
            // "       here</p>\n" ~
            assert(cursor.next() == EntityType.elementStart);
            assert(cursor.next() == EntityType.text);
            assert(cursor.text == "\n       something else\n       here");
            assert(cursor.next() == EntityType.elementEnd);
            assert(cursor.next() == EntityType.text);
            assert(cursor.text == "\n");

            // "</root>"
            assert(cursor.next() == EntityType.elementEnd);
            assert(cursor.next() == EntityType.documentEnd);
        }
    }


    /++
        When at a start tag, moves the cursor to the entity after the
        corresponding end tag tag and returns the contents between the two tags
        as text, leaving any markup in between as unprocessed text.

        $(TABLE
            $(TR $(TH Supported $(LREF EntityType)s:))
            $(TR $(TD $(LREF2 elementStart, EntityType)))
        )

        Throws: $(LREF XMLParsingException) on invalid XML.
      +/
    @property SliceOfR contentAsText()
    {
        assert(_state.type == EntityType.elementStart);

        assert(0);
    }


private:

    void _parseDocumentStart()
    {
        if(_state.stripStartsWith("<?xml"))
        {
            _state.skipUntilAndDrop!"?>"();
            _state.grammarPos = GrammarPos.prologMisc1;
            next();
        }
        else
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
                auto cursor = parseXML!config(func(xml));
                assertNotThrown!XMLParsingException(cursor.next(), "unittest failure 1", __FILE__, line);
                enforce!AssertError(cursor._state.pos == pos, "unittest failure 2", __FILE__, line);
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

        stripWS(_state);
        checkNotEmpty(_state);
        if(_state.input.front != '<')
            throw new XMLParsingException("Expected <", _state.pos);
        popFrontAndIncCol(_state);
        checkNotEmpty(_state);

        switch(_state.input.front)
        {
            // Comment     ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
            // doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
            case '!':
            {
                popFrontAndIncCol(_state);
                if(_state.stripStartsWith("--"))
                {
                    _parseComment();
                    break;
                }
                static if(miscNum == 1)
                {
                    if(_state.stripStartsWith("DOCTYPE"))
                    {
                        if(!_state.stripWS())
                            throw new XMLParsingException("Whitespace must follow <!DOCTYPE", _state.pos);
                        _parseDoctypeDecl();
                        break;
                    }
                    throw new XMLParsingException("Expected DOCTYPE or --", _state.pos);
                }
                else
                {
                    if(_state.stripStartsWith("DOCTYPE"))
                    {
                        throw new XMLParsingException("Only one <!DOCTYPE ...> declaration allowed per XML document",
                                                      _state.pos);
                    }
                    throw new XMLParsingException("--", _state.pos);
                }
            }
            // PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
            case '?':
            {
                _parsePI();
                static if(config.skipPI == SkipPI.yes)
                    next();
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
            _state.skipUntilAndDrop!"--"();
        else
        {
            _state.type = EntityType.comment;
            _state.savedText.pos = _state.pos;
            _state.savedText.input = _state.takeUntilAndDrop!"--"();
        }
        if(_state.input.empty || _state.input.front != '>')
            throw new XMLParsingException("Comments cannot contain -- and cannot be terminated by --->", _state.pos);
        popFrontAndIncCol(_state);
    }


    // PI       ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
    // PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
    // Parses a processing instruction. < was already removed from the input.
    void _parsePI()
    {
        assert(_state.input.front == '?');
        popFrontAndIncCol(_state);
        static if(config.skipPI == SkipPI.yes)
            _state.skipUntilAndDrop!"?>"();
        else
        {
            auto pos = _state.pos;
            _state.type = EntityType.pi;
            _state.name = takeName!(true, '?')(_state);
            checkNotEmpty(_state);
            if(_state.input.front != '?')
            {
                if(!stripWS(_state))
                {
                    throw new XMLParsingException("There must be whitespace after the name if there is text before " ~
                                                  "the terminating ?>", _state.pos);
                }
            }
            _state.savedText.pos = _state.pos;
            _state.savedText.input = _state.takeUntilAndDrop!"?>"();
            if(walkLength(_state.name.save) == 3)
            {
                // FIXME icmp doesn't compile right now due to an issue with
                // byUTF that needs to be looked into.
                /+
                import std.uni : icmp;
                if(icmp(_state.name.save, "xml") == 0)
                    throw new XMLParsingException("Processing instructions cannot be named xml", pos);
                +/
                if(_state.name.front == 'x' || _state.name.front == 'X')
                {
                    _state.name.popFront();
                    if(_state.name.front == 'm' || _state.name.front == 'M')
                    {
                        _state.name.popFront();
                        if(_state.name.front == 'l' || _state.name.front == 'L')
                            throw new XMLParsingException("Processing instructions cannot be named xml", pos);
                    }
                }
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
        _state.type = EntityType.cdata;
        _state.savedText.pos = _state.pos;
        _state.savedText.input = _state.takeUntilAndDrop!"]]>";
        _state.grammarPos = GrammarPos.contentCharData2;
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
        _state.skipToOneOf!('"', '\'', '[', '>')();
        switch(_state.input.front)
        {
            case '"':
            {
                _state.skipUntilAndDrop!`"`();
                checkNotEmpty(_state);
                _state.skipToOneOf!('[', '>')();
                if(_state.input.front == '[')
                    goto case '[';
                else
                    goto case '>';
            }
            case '\'':
            {
                _state.skipUntilAndDrop!`'`();
                checkNotEmpty(_state);
                _state.skipToOneOf!('[', '>')();
                if(_state.input.front == '[')
                    goto case '[';
                else
                    goto case '>';
            }
            case '[':
            {
                popFrontAndIncCol(_state);
                while(1)
                {
                    checkNotEmpty(_state);
                    _state.skipToOneOf!('"', '\'', ']')();
                    switch(_state.input.front)
                    {
                        case '"':
                        {
                            _state.skipUntilAndDrop!`"`();
                            continue;
                        }
                        case '\'':
                        {
                            _state.skipUntilAndDrop!`'`();
                            continue;
                        }
                        case ']':
                        {
                            _state.skipUntilAndDrop!`>`();
                            _parseAtPrologMisc!2();
                            return;
                        }
                        default: assert(0);
                    }
                }
            }
            case '>':
            {
                popFrontAndIncCol(_state);
                _parseAtPrologMisc!2();
                break;
            }
            default: assert(0);
        }
    }

    static if(compileInTests) unittest
    {
        import std.algorithm.comparison : equal;
        import std.exception : assertThrown;

        static void test(alias func)(string text, int row, int col, size_t line = __LINE__)
        {
            enum int tagLen = "<root/>".length;
            auto xml = func(text ~ "<root/>");

            static foreach(i, config; testConfigs)
            {{
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col + tagLen : -1);
                auto cursor = parseXML!config(xml.save);
                enforceTest(cursor.next() == EntityType.elementEmpty, "unittest failure 1", line);
                enforceTest(cursor._state.pos == pos, "unittest failure 2", line);
            }}
        }

        static void testFail(alias func)(string text, size_t line = __LINE__)
        {
            auto xml = func(text);

            static foreach(i, config; testConfigs)
            {{
                auto cursor = parseXML!config(xml.save);
                assertThrown!XMLParsingException(cursor.next(), "unittest failure", __FILE__, line);
            }}
        }

        static foreach(func; testRangeFuncs)
        {
            test!func("<!DOCTYPE name>", 1, 16);
            test!func("<!DOCTYPE \n\n\n name>", 4, 7);
            test!func("<!DOCTYPE name \n\n\n >", 4, 3);

            test!func("<!DOCTYPE name []>", 1, 19);
            test!func("<!DOCTYPE \n\n\n name []>", 4, 10);
            test!func("<!DOCTYPE name \n\n\n []>", 4, 5);

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
        _state.savedText.pos = _state.pos;
        _state.savedText.input = _state.takeUntilAndDrop!">"();
        auto temp = _state.savedText.input.save;
        temp.popFrontN(temp.length - 1);
        if(temp.front == '/')
        {
            _state.savedText.input = _state.savedText.input.takeExactly(_state.savedText.input.length - 1);

            static if(config.splitEmpty == SplitEmpty.no)
            {
                _state.type = EntityType.elementEmpty;
                _state.grammarPos = _state.tagStack.empty ? GrammarPos.endMisc : GrammarPos.contentCharData2;
            }
            else
            {
                _state.type = EntityType.elementStart;
                _state.grammarPos = GrammarPos.splittingEmpty;
            }
        }
        else
        {
            _state.type = EntityType.elementStart;
            _state.grammarPos = GrammarPos.contentCharData1;
        }

        if(_state.savedText.input.empty)
            throw new XMLParsingException("Tag missing name", _state.savedText.pos);
        if(_state.savedText.input.front == '/')
            throw new XMLParsingException("Invalid end tag", _state.savedText.pos);
        _state.name = takeName!true(&_state.savedText);
        stripWS(&_state.savedText);
        // The attributes should be all that's left in savedText.
    }


    // Parse an end tag. It could be the root element, or it could be a
    // sub-element.
    // </ was already removed from the front of the input.
    void _parseElementEnd()
    {
        import std.algorithm.comparison : equal;
        import std.format : format;
        _state.type = EntityType.elementEnd;
        _state.savedText.pos = _state.pos;
        _state.name = _state.takeUntilAndDrop!">"();
        assert(!_state.tagStack.empty);
        if(!equal(_state.name.save, _state.tagStack.back.save))
        {
            enum fmt = "Name of end tag </%s> does not match corresponding start tag <%s>";
            throw new XMLParsingException(format!fmt(_state.name, _state.tagStack.back), _state.savedText.pos);
        }
        _state.tagStack.pop();
        _state.grammarPos = _state.tagStack.empty ? GrammarPos.endMisc : GrammarPos.contentCharData2;
    }


    // GrammarPos.contentCharData1
    // content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
    // Parses at either CharData?. Nothing from the CharData? (or what's after it
    // if it's not there) has been consumed.
    void _parseAtContentCharData()
    {
        checkNotEmpty(_state);
        static if(config.skipContentWS == SkipContentWS.yes)
        {
            auto origInput = _state.input.save;
            auto origPos = _state.pos;
            stripWS(_state);
            checkNotEmpty(_state);
            if(_state.input.front != '<')
            {
                _state.input = origInput;
                _state.pos = origPos;
            }
        }
        if(_state.input.front != '<')
        {
            _state.type = EntityType.text;
            _state.savedText.input = _state.takeUntilAndDrop!"<"();
            checkNotEmpty(_state);
            if(_state.input.front == '/')
            {
                popFrontAndIncCol(_state);
                _state.grammarPos = GrammarPos.endTag;
            }
            else
                _state.grammarPos = GrammarPos.contentMid;
        }
        else
        {
            popFrontAndIncCol(_state);
            checkNotEmpty(_state);
            if(_state.input.front == '/')
            {
                popFrontAndIncCol(_state);
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
        // parsed out by the EntityCursor (see EntityCursor.text).

        switch(_state.input.front)
        {
            // Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
            // CDSect  ::= CDStart CData CDEnd
            // CDStart ::= '<![CDATA['
            // CData   ::= (Char* - (Char* ']]>' Char*))
            // CDEnd   ::= ']]>'
            case '!':
            {
                popFrontAndIncCol(_state);
                if(_state.stripStartsWith("--"))
                {
                    _parseComment();
                    static if(config.skipComments == SkipComments.yes)
                        _parseAtContentCharData();
                    else
                        _state.grammarPos = GrammarPos.contentCharData2;
                }
                else if(_state.stripStartsWith("[CDATA["))
                    _parseCDATA();
                else
                    throw new XMLParsingException("Not valid Comment or CData section", _state.pos);
                break;
            }
            // PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
            case '?':
            {
                _parsePI();
                _state.grammarPos = GrammarPos.contentCharData2;
                static if(config.skipPI == SkipPI.yes)
                    next();
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

        stripWS(_state);

        if(_state.input.empty)
        {
            _state.grammarPos = GrammarPos.documentEnd;
            _state.type = EntityType.documentEnd;
            return;
        }

        if(_state.input.front != '<')
            throw new XMLParsingException("Expected <", _state.pos);
        popFrontAndIncCol(_state);
        checkNotEmpty(_state);

        switch(_state.input.front)
        {
            // Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
            case '!':
            {
                popFrontAndIncCol(_state);
                if(_state.stripStartsWith("--"))
                {
                    _parseComment();
                    break;
                }
                throw new XMLParsingException("Expected --", _state.pos);
            }
            // PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
            case '?':
            {
                _parsePI();
                static if(config.skipPI == SkipPI.yes)
                    next();
                break;
            }
            default: throw new XMLParsingException("Must be a comment or PI", _state.pos);
        }
    }


    struct ParserState
    {
        import std.utf : byCodeUnit;

        alias config = cfg;
        alias Text = R;
        alias Taken = typeof(takeExactly(byCodeUnit(R.init), 42));
        alias SliceOfR = EntityCursor.SliceOfR;

        auto type = EntityType.documentStart;
        auto grammarPos = GrammarPos.documentStart;

        static if(config.posType == PositionType.lineAndCol)
            auto pos = SourcePos(1, 1);
        else static if(config.posType == PositionType.line)
            auto pos = SourcePos(1, -1);
        else
            SourcePos pos;

        typeof(byCodeUnit(R.init)) input;

        // This mirrors the ParserState's fields so that the same parsing functions
        // can be used to parse main text contained directly in the ParserState
        // and the currently saved text (e.g. for the attributes of a start tag).
        struct SavedText
        {
            alias config = cfg;
            alias Text = R;
            Taken input;
            SourcePos pos;

            @property save() { return SavedText(input.save, pos); }
        }

        SavedText savedText;

        Taken name;
        TagStack!Taken tagStack;

        this(R xmlText)
        {
            input = byCodeUnit(xmlText);

            // None of these initializations should be required. https://issues.dlang.org/show_bug.cgi?id=13945
            savedText = typeof(savedText).init;
            name = typeof(name).init;
        }
    }


    this(R xmlText)
    {
        _state = new ParserState(xmlText);
    }


    ParserState* _state;
}

/// Ditto
EntityCursor!(config, R) parseXML(Config config = Config.init, R)(R xmlText)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    return EntityCursor!(config, R)(xmlText);
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
        auto cursor = parseXML(xml);
        assert(cursor.next() == EntityType.pi);
        assert(cursor.name == "instruction");
        assert(cursor.text == "start");

        assert(cursor.next() == EntityType.elementStart);
        assert(cursor.name == "foo");

        {
            auto attrs = cursor.attributes;
            assert(walkLength(attrs.save) == 1);
            assert(attrs.front.name == "attr");
            assert(attrs.front.value == "42");
        }

        assert(cursor.next() == EntityType.elementEmpty);
        assert(cursor.name == "bar");

        assert(cursor.next() == EntityType.comment);
        assert(cursor.text == " no comment ");

        assert(cursor.next() == EntityType.elementStart);
        assert(cursor.name == "baz");

        {
            auto attrs = cursor.attributes;
            assert(walkLength(attrs.save) == 1);
            assert(attrs.front.name == "hello");
            assert(attrs.front.value == "world");
        }

        assert(cursor.next() == EntityType.text);
        assert(cursor.text ==
               "\n    nothing to say.\n    nothing at all...\n    ");

        assert(cursor.next() == EntityType.elementEnd); // </baz>
        assert(cursor.next() == EntityType.elementEnd); // </foo>

        assert(cursor.next() == EntityType.pi);
        assert(cursor.name == "some");
        assert(cursor.text == "foo");

        assert(cursor.next() == EntityType.documentEnd);
    }
    {
        auto cursor = parseXML!simpleXML(xml);

        // simpleXML is set to skip processing instructions.

        assert(cursor.next() == EntityType.elementStart);
        assert(cursor.name == "foo");

        {
            auto attrs = cursor.attributes;
            assert(walkLength(attrs.save) == 1);
            assert(attrs.front.name == "attr");
            assert(attrs.front.value == "42");
        }

        // simpleXML is set to split empty tags so that <bar/> is treated
        // as the same as <bar></bar> so that code does not have to
        // explicitly handle empty tags.
        assert(cursor.next() == EntityType.elementStart);
        assert(cursor.name == "bar");
        assert(cursor.next() == EntityType.elementEnd);

        // simpleXML is set to skip comments.

        assert(cursor.next() == EntityType.elementStart);
        assert(cursor.name == "baz");

        {
            auto attrs = cursor.attributes;
            assert(walkLength(attrs.save) == 1);
            assert(attrs.front.name == "hello");
            assert(attrs.front.value == "world");
        }

        assert(cursor.next() == EntityType.text);
        assert(cursor.text ==
               "\n    nothing to say.\n    nothing at all...\n    ");

        assert(cursor.next() == EntityType.elementEnd); // </baz>
        assert(cursor.next() == EntityType.elementEnd); // </foo>
        assert(cursor.next() == EntityType.documentEnd);
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
    EntityCursor!(Config.init, EntityCompileTests) _entityCursorTests;


/++
    When at a start tag, moves the cursor to the corresponding end tag. It is
    an error to call skipContents when the current entity is not
    $(LREF EntityType.elementStart).

    $(TABLE
        $(TR $(TH Supported $(LREF EntityType)s:))
        $(TR $(TD $(LREF2 elementStart, EntityType)))
    )

    Throws: $(LREF XMLParsingException) on invalid XML.
  +/
void skipContents(EC)(EC cursor)
    if(isInstanceOf!(EntityCursor, EC))
{
    assert(cursor.type == EntityType.elementStart);

    // FIXME Rather than parsing exactly the same as normal, this should
    // skip as much parsing as possible.

    for(int tagDepth = 1; tagDepth != 0;)
    {
        immutable type = cursor.next();
        if(type == EntityType.elementStart)
            ++tagDepth;
        else if(type == EntityType.elementEnd)
            --tagDepth;
    }
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

    auto cursor = parseXML(xml);
    assert(cursor.next() == EntityType.elementStart);
    assert(cursor.name == "root");

    assert(cursor.next() == EntityType.elementStart);
    assert(cursor.name == "foo");

    cursor.skipContents();
    assert(cursor.type == EntityType.elementEnd);
    assert(cursor.name == "foo");

    assert(cursor.next() == EntityType.comment);
    assert(cursor.text == " no comment ");

    assert(cursor.next() == EntityType.elementEnd);
    assert(cursor.name == "root");

    assert(cursor.next() == EntityType.documentEnd);
}


/++
    Skips entities until the given $(LREF EntityType) is reached.

    If multiple $(LREF EntityType)s are given, then any one of them counts as
    a match.

    The current entity is skipped regardless of whether it is the given
    $(LREF EntityType).

    Returns: The $(LREF EntityType) of the now-current entity just like
             $(LREF2 next, EntityCursor) would. If the requested
             $(LREF EntityType) is not found, then
             $(LREF EntityType.documentEnd) is returned.
  +/
auto skipToEntityType(EC)(EC cursor, EntityType[] entityTypes...)
    if(isInstanceOf!(EntityCursor, EC))
{
    while(true)
    {
        auto type = cursor.next();
        foreach(entityType; entityTypes)
        {
            if(type == entityType)
                return type;
        }
        if(type == EntityType.documentEnd)
            return EntityType.documentEnd;
    }
}

unittest
{
    auto xml = "<root>\n" ~
               "    <!-- blah blah blah -->\n" ~
               "    <foo>nothing to say</foo>\n" ~
               "</root>";

    auto cursor = parseXML(xml);
    assert(cursor.next() == EntityType.elementStart);
    assert(cursor.name == "root");

    assert(cursor.skipToEntityType(EntityType.elementStart, EntityType.elementEmpty) ==
           EntityType.elementStart);
    assert(cursor.name == "foo");

    assert(cursor.skipToEntityType(EntityType.comment) ==
           EntityType.documentEnd);
}


/+
/++
    Treats the given string like a file path except that each directory
    corresponds to the name of a start tag. Note that this does $(I not) try to
    implement XPath as that would be quite complicated, but it does try to be
    compatible with it for the small subset of the syntax that it supports.

    All paths should be relative. $(LREF EntityCursor) can only move forward
    through the document, so using an absolute path would only make sense at
    the beginning of the document.

    Returns: The $(LREF EntityType) of the now-current entity just like
             $(LREF2 next, EntityCursor) would. If the requested path is not
             found, then $(LREF EntityType.documentEnd) is returned.
  +/
EntityType skipToPath(EC)(EC cursor, string path)
    if(isInstanceOf!(EntityCursor, EC))
{
    with(EntityType):

    import std.algorithm.comparison : equal;
    import std.path : pathSplitter;

    EntityType upLevel()
    {
        if(cursor.skipToParentDepth() == documentEnd)
            return documentEnd;
        static if(EC.config.splitEmpty == SplitEmpty.yes)
            immutable type = cursor.skipToEntityType(elementStart, elementEnd);
        else
            immutable type = cursor.skipToEntityType(elementStart, elementEnd, elementEmpty);
        return type == elementEnd ? documentEnd : type;
    }

    auto pieces = path.pathSplitter();

    static if(EC.config.splitEmpty == SplitEmpty.yes)
        immutable atStart = cursor.type == elementStart;
    else
        immutable atStart = cursor.type == elementStart || cursor.type == elementEmpty;

    if(!atStart)
    {
        immutable name = pieces.front;
        pieces.popFront();

        while(true)
        {
            static if(EC.config.splitEmpty == SplitEmpty.yes)
                immutable type =  cursor.skipToEntityType(elementStart, elementEnd);
            else
                immutable type = cursor.skipToEntityType(elementStart, e, elementEnd, lementEmpty);

            if(type == elementEnd || type == documentEnd)
                return documentEnd;

            if(name == ".")
                break;

            if(name == "..")
            {
                immutable type = upLevel();
                if(type == documentEnd)
                    return documentEnd;
                break;
            }

            if(equal(name, cursor.name))
            {
                pieces.popFront();
                break;
            }

            static if(EC.config.splitEmpty == SplitEmpty.no)
            {
                if(type == elementEmpty)
                    continue;
            }
            cursor.skipContents();
        }
    }

    for(; !pieces.empty; pieces.popFront())
    {
        immutable name = pieces.front;

        if(name == ".")
            continue;
        if(name == "..")
        {
            if(cursor.skipToParentDepth() == documentEnd)
                return documentEnd;
            static if(EC.config.splitEmpty == SplitEmpty.yes)
                immutable type = cursor.skipToEntityType(elementStart, elementEnd);
            else
                immutable type = cursor.skipToEntityType(elementStart, elementEnd, elementEmpty);
            if(type == elementEnd || type == documentEnd)
                return documentEnd;
            continue;
        }
        if(equal(name, cursor.name))
        {
            static if(EC.config.splitEmpty == SplitEmpty.yes)
                immutable type = cursor.skipToEntityType(elementStart, elementEnd);
            else
                immutable type = cursor.skipToEntityType(elementStart, elementEnd, elementEmpty);
        }
        cursor.skipToEntityType(elementStart, elementEnd);
    }

    assert(0);
}

unittest
{
}
+/


/++
    Skips entities until an entity is reached that is at the same depth as the
    parent of the current entity.

    Returns: The $(LREF EntityType) of the now-current entity just like
             $(LREF2 next, EntityCursor) would. If the requested depth is not
             found (which mean that the depth was 0 when skipToParentDepth was
             called), then $(LREF EntityType.documentEnd) is returned.
  +/
auto skipToParentDepth(EC)(EC cursor)
    if(isInstanceOf!(EntityCursor, EC))
{
    with(EntityType) final switch(cursor.type)
    {
        case cdata:
        case comment:
        {
            immutable type = cursor.skipToEntityType(elementStart, elementEnd);
            if(type == documentEnd || type == elementEnd)
                return type;
            goto case elementStart;
        }
        case documentEnd: return cursor.next(); // will assert(0), but we don't have to duplicate the message this way
        case documentStart: return documentEnd;
        case elementStart:
        {
            while(true)
            {
                {
                    cursor.skipContents();
                    immutable type = cursor.next();
                    if(type == elementEnd || type == documentEnd)
                        return type;
                    if(type == elementStart)
                        continue;
                }
                immutable type = cursor.skipToEntityType(elementStart, elementEnd);
                if(type == elementEnd)
                    return elementEnd;
                if(type == documentEnd)
                    return type;
            }
        }
        case elementEnd: goto case comment;
        case elementEmpty: goto case comment;
        case pi:
        {
            immutable type = cursor.skipToEntityType(elementStart, elementEnd);
            if(type == documentEnd || type == elementEnd)
                return type;
            goto case elementStart;
        }
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
        auto cursor = parseXML(xml);
        assert(cursor.next() == EntityType.elementStart);
        assert(cursor.name == "root");

        assert(cursor.next() == EntityType.elementStart);
        assert(cursor.name == "foo");

        assert(cursor.next() == EntityType.comment);
        assert(cursor.text == " comment ");

        assert(cursor.skipToParentDepth() == EntityType.elementEnd);
        assert(cursor.name == "foo");

        assert(cursor.skipToParentDepth() == EntityType.elementEnd);
        assert(cursor.name == "root");

        assert(cursor.skipToParentDepth() == EntityType.documentEnd);
    }
    {
        auto cursor = parseXML(xml);
        assert(cursor.next() == EntityType.elementStart);
        assert(cursor.name == "root");

        assert(cursor.next() == EntityType.elementStart);
        assert(cursor.name == "foo");

        assert(cursor.next() == EntityType.comment);
        assert(cursor.text == " comment ");

        assert(cursor.next() == EntityType.elementStart);
        assert(cursor.name == "bar");

        assert(cursor.next() == EntityType.text);
        assert(cursor.text == "exam");

        assert(cursor.skipToParentDepth() == EntityType.elementEnd);
        assert(cursor.name == "bar");

        assert(cursor.skipToParentDepth() == EntityType.elementEnd);
        assert(cursor.name == "foo");

        assert(cursor.next() == EntityType.comment);
        assert(cursor.text == " another comment ");

        assert(cursor.skipToParentDepth() == EntityType.elementEnd);
        assert(cursor.name == "root");

        assert(cursor.skipToParentDepth() == EntityType.documentEnd);
    }
    {
        auto cursor = parseXML("<root><foo>bar</foo></root>");
        assert(cursor.next() == EntityType.elementStart);
        assert(cursor.name == "root");
        assert(cursor.skipToParentDepth() == EntityType.documentEnd);
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
        for(int i = 0; true; ++i)
        {
            auto cursor = parseXML(xml);
            enforceTest(cursor.next() == EntityType.elementStart);
            enforceTest(cursor.next() == EntityType.cdata);
            if(i == 0)
            {
                enforceTest(cursor.text == " cdata run ");
                enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
                enforceTest(cursor.name == "root");
                continue;
            }
            enforceTest(cursor.next() == EntityType.elementEmpty);
            enforceTest(cursor.next() == EntityType.cdata);
            if(i == 1)
            {
                enforceTest(cursor.text == " cdata have its bits flipped ");
                enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
                enforceTest(cursor.name == "root");
                continue;
            }
            enforceTest(cursor.next() == EntityType.elementStart);
            assertNotThrown!XMLParsingException(cursor.skipContents());
            enforceTest(cursor.next() == EntityType.cdata);
            enforceTest(cursor.text == " cdata play violin ");
            enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
            enforceTest(cursor.name == "root");
            break;
        }
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
        {
            auto cursor = parseXML(xml);
            enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
        }
        for(int i = 0; true; ++i)
        {
            auto cursor = parseXML(xml);
            enforceTest(cursor.next() == EntityType.comment);
            enforceTest(cursor.next() == EntityType.elementStart);
            enforceTest(cursor.next() == EntityType.comment);
            if(i == 0)
            {
                enforceTest(cursor.text == " comment 1 ");
                enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
                enforceTest(cursor.name == "root");
                continue;
            }
            enforceTest(cursor.next() == EntityType.elementEmpty);
            enforceTest(cursor.next() == EntityType.comment);
            if(i == 1)
            {
                enforceTest(cursor.text == " comment 2 ");
                enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
                enforceTest(cursor.name == "root");
                continue;
            }
            enforceTest(cursor.next() == EntityType.elementStart);
            assertNotThrown!XMLParsingException(cursor.skipContents());
            enforceTest(cursor.next() == EntityType.comment);
            enforceTest(cursor.text == " comment 3 ");
            enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
            enforceTest(cursor.name == "root");
            break;
        }
        for(int i = 0; true; ++i)
        {
            auto cursor = parseXML(xml);
            enforceTest(cursor.next() == EntityType.comment);
            enforceTest(cursor.next() == EntityType.elementStart);
            assertNotThrown!XMLParsingException(cursor.skipContents());
            enforceTest(cursor.next() == EntityType.comment);
            enforceTest(cursor.text == " after ");
            if(i == 0)
            {
                enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
                continue;
            }
            enforceTest(cursor.next() == EntityType.comment);
            enforceTest(cursor.text == " end ");
            enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
            break;
        }
    }
    // documentStart
    {
        auto xml = "<?xml version='1.0'?>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
    }
    {
        auto xml = "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
    }
    // elementStart
    for(int i = 0; true; ++i)
    {
        auto xml = "<root>\n" ~
                   "    <a><b>foo</b></a>\n" ~
                   "    <nothing/>\n" ~
                   "    <c></c>\n" ~
                   "</root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.elementStart);
        if(i == 0)
        {
            enforceTest(cursor.name == "root");
            enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
            continue;
        }
        enforceTest(cursor.next() == EntityType.elementStart);
        if(i == 1)
        {
            enforceTest(cursor.name == "a");
            enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
            enforceTest(cursor.name == "root");
            continue;
        }
        enforceTest(cursor.next() == EntityType.elementStart);
        if(i == 2)
        {
            enforceTest(cursor.name == "b");
            enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
            enforceTest(cursor.name == "a");
            continue;
        }
        enforceTest(cursor.next() == EntityType.text);
        enforceTest(cursor.next() == EntityType.elementEnd);
        enforceTest(cursor.next() == EntityType.elementEnd);
        enforceTest(cursor.next() == EntityType.elementEmpty);
        enforceTest(cursor.next() == EntityType.elementStart);
        enforceTest(cursor.name == "c");
        enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
        enforceTest(cursor.name == "root");
        break;
    }
    // elementEnd
    for(int i = 0; true; ++i)
    {
        auto xml = "<root>\n" ~
                   "    <a><b>foo</b></a>\n" ~
                   "    <nothing/>\n" ~
                   "    <c></c>\n" ~
                   "</root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.elementStart);
        enforceTest(cursor.next() == EntityType.elementStart);
        enforceTest(cursor.next() == EntityType.elementStart);
        enforceTest(cursor.next() == EntityType.text);
        enforceTest(cursor.next() == EntityType.elementEnd);
        if(i == 0)
        {
            enforceTest(cursor.name == "b");
            enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
            enforceTest(cursor.name == "a");
            continue;
        }
        enforceTest(cursor.next() == EntityType.elementEnd);
        if(i == 1)
        {
            enforceTest(cursor.name == "a");
            enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
            enforceTest(cursor.name == "root");
            continue;
        }
        enforceTest(cursor.next() == EntityType.elementEmpty);
        enforceTest(cursor.next() == EntityType.elementStart);
        enforceTest(cursor.next() == EntityType.elementEnd);
        if(i == 2)
        {
            enforceTest(cursor.name == "c");
            enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
            enforceTest(cursor.name == "root");
            continue;
        }
        enforceTest(cursor.next() == EntityType.elementEnd);
        enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
        break;
    }
    // elementEmpty
    {
        auto cursor = parseXML("<root/>");
        enforceTest(cursor.next() == EntityType.elementEmpty);
        enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
    }
    foreach(i; 0 .. 2)
    {
        auto xml = "<root>\n" ~
                   "    <a><b>foo</b></a>\n" ~
                   "    <nothing/>\n" ~
                   "    <c></c>\n" ~
                   "    <whatever/>\n" ~
                   "</root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.elementStart);
        enforceTest(cursor.next() == EntityType.elementStart);
        assertNotThrown!XMLParsingException(cursor.skipContents());
        enforceTest(cursor.next() == EntityType.elementEmpty);
        if(i == 0)
            enforceTest(cursor.name == "nothing");
        else
        {
            enforceTest(cursor.next() == EntityType.elementStart);
            enforceTest(cursor.next() == EntityType.elementEnd);
            enforceTest(cursor.next() == EntityType.elementEmpty);
            enforceTest(cursor.name == "whatever");
        }
        enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
        enforceTest(cursor.name == "root");
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
        for(int i = 0; true; ++i)
        {
            auto cursor = parseXML(xml);
            enforceTest(cursor.next() == EntityType.pi);
            if(i == 0)
            {
                enforceTest(cursor.name == "Sherlock");
                enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
                continue;
            }
            enforceTest(cursor.next() == EntityType.elementStart);
            enforceTest(cursor.next() == EntityType.pi);
            if(i == 1)
            {
                enforceTest(cursor.name == "Foo");
                enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
                enforceTest(cursor.name == "root");
                continue;
            }
            enforceTest(cursor.next() == EntityType.elementEmpty);
            enforceTest(cursor.next() == EntityType.pi);
            if(i == 2)
            {
                enforceTest(cursor.name == "Bar");
                enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
                enforceTest(cursor.name == "root");
                continue;
            }
            enforceTest(cursor.next() == EntityType.elementStart);
            enforceTest(cursor.next() == EntityType.elementEnd);
            enforceTest(cursor.next() == EntityType.pi);
            enforceTest(cursor.name == "Baz");
            enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
            enforceTest(cursor.name == "root");
            enforceTest(cursor.next() == EntityType.pi);
            if(i == 3)
            {
                enforceTest(cursor.name == "Poirot");
                enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
                continue;
            }
            enforceTest(cursor.next() == EntityType.pi);
            enforceTest(cursor.name == "Conan");
            enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
            break;
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
        for(int i = 0; true; ++i)
        {
            auto cursor = parseXML(xml);
            enforceTest(cursor.next() == EntityType.elementStart);
            enforceTest(cursor.next() == EntityType.text);
            if(i == 0)
            {
                enforceTest(cursor.text == "\n    nothing to say\n    ");
                enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
                enforceTest(cursor.name == "root");
                continue;
            }
            enforceTest(cursor.next() == EntityType.elementEmpty);
            enforceTest(cursor.next() == EntityType.text);
            if(i == 1)
            {
                enforceTest(cursor.text == "\n    nothing whatsoever\n    ");
                enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
                enforceTest(cursor.name == "root");
                continue;
            }
            enforceTest(cursor.next() == EntityType.elementStart);
            assertNotThrown!XMLParsingException(cursor.skipContents());
            enforceTest(cursor.next() == EntityType.text);
            enforceTest(cursor.text == "\n    but he keeps talking\n");
            enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
            enforceTest(cursor.name == "root");
            break;
        }
    }
}


//------------------------------------------------------------------------------
// Private Section
//------------------------------------------------------------------------------
private:


version(unittest) auto testParser(Config config, R)(R xmlText) @trusted pure nothrow @nogc
{
    static getPS(R xmlText) @trusted nothrow @nogc
    {
        static EntityCursor!(config, R).ParserState ps;
        ps = typeof(ps)(xmlText);
        return &ps;
    }
    alias FuncType = @trusted pure nothrow @nogc EntityCursor!(config, R).ParserState* function(R);
    return (cast(FuncType)&getPS)(xmlText);
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
    // EntityCursor (see EntityCursor.text).
    contentCharData1,

    // element  ::= EmptyElemTag | STag content ETag
    // content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
    // This is after the first CharData?. The next thing to parse will be a
    // element, CDSect, PI, Comment, or ETag.
    // References are treated as part of the CharData and not parsed out by the
    // EntityCursor (see EntityCursor.text).
    contentMid,

    // element  ::= EmptyElemTag | STag content ETag
    // content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
    // This is at the second CharData?. The next thing to parse will be a
    // CharData, element, CDSect, PI, Comment, or ETag.
    // References are treated as part of the CharData and not parsed out by the
    // EntityCursor (see EntityCursor.text).
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
// matches. It also deals with incrementing state.pos.col.
//
// It is assumed that there are no newlines.
bool stripStartsWith(PS)(PS state, string text)
{
    static assert(isPointer!PS, "_state.savedText was probably passed rather than &_state.savedText");

    alias R = typeof(PS.input);

    static if(hasLength!R)
    {
        if(state.input.length < text.length)
            return false;

        // This branch is separate so that we can take advantage of whatever
        // speed boost comes from comparing strings directly rather than
        // comparing individual characters.
        static if(isDynamicArray!(PS.Text) && is(Unqual!(ElementEncodingType!(PS.Text)) == char))
        {
            if(state.input.source[0 .. text.length] != text)
                return false;
            state.input.popFrontN(text.length);
        }
        else
        {
            auto origInput = state.input.save;
            auto origPos = state.pos;

            foreach(c; text)
            {
                if(state.input.front != c)
                {
                    state.input = origInput;
                    state.pos = origPos;
                    return false;
                }
                state.input.popFront();
            }
        }
    }
    else
    {
        auto origInput = state.input.save;
        auto origPos = state.pos;

        foreach(c; text)
        {
            if(state.input.empty)
            {
                state.input = origInput;
                state.pos = origPos;
                return false;
            }
            if(state.input.front != c)
            {
                state.input = origInput;
                state.pos = origPos;
                return false;
            }
            state.input.popFront();
        }
    }

    static if(PS.config.posType == PositionType.lineAndCol)
        state.pos.col += text.length;

    return true;
}

unittest
{
    static void test(alias func)(string origHaystack, string needle, string remainder, bool startsWith,
                                 int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);

        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto state = testParser!config(haystack.save);
                enforceTest(state.stripStartsWith(needle) == startsWith, "unittest failure 1", line);
                enforceTest(equalCU(state.input, remainder), "unittest failure 2", line);
                enforceTest(state.pos == pos, "unittest failure 3", line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto state = testParser!config(haystack.save);
                state.pos.line += 3;
                static if(i == 0)
                    state.pos.col += 7;
                enforceTest(state.stripStartsWith(needle) == startsWith, "unittest failure 4", line);
                enforceTest(equalCU(state.input, remainder), "unittest failure 5", line);
                enforceTest(state.pos == pos, "unittest failure 6", line);
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


// Strips whitespace while dealing with state.pos accordingly. Newlines are not
// ignored.
// Returns whether any whitespace was stripped.
bool stripWS(PS)(PS state)
{
    static assert(isPointer!PS, "_state.savedText was probably passed rather than &_state.savedText");

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
    static void test(alias func)(string origHaystack, string remainder, bool stripped,
                                 int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);

        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto state = testParser!config(haystack.save);
                enforceTest(state.stripWS() == stripped, "unittest failure 1", line);
                enforceTest(equalCU(state.input, remainder), "unittest failure 2", line);
                enforceTest(state.pos == pos, "unittest failure 3", line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto state = testParser!config(haystack.save);
                state.pos.line += 3;
                static if(i == 0)
                    state.pos.col += 7;
                enforceTest(state.stripWS() == stripped, "unittest failure 4", line);
                enforceTest(equalCU(state.input, remainder), "unittest failure 5", line);
                enforceTest(state.pos == pos, "unittest failure 6", line);
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


enum TakeUntil
{
    keepAndReturn,
    dropAndReturn,
    drop
}

// Returns a slice (or takeExactly) of state.input up to _and_ including the
// given text, removing both that slice from state.input in the process. If the
// text is not found, then an XMLParsingException is thrown.
auto takeUntilAndKeep(string text, PS)(PS state)
{
    return _takeUntil!(TakeUntil.keepAndReturn, text, PS)(state);
}

unittest
{
    import std.algorithm.comparison : equal;
    import std.exception : assertThrown;

    static void test(alias func, string needle)(string origHaystack, string expected, string remainder,
                                                int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);

        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto state = testParser!config(haystack.save);
                enforceTest(equal(state.takeUntilAndKeep!needle(), expected), "unittest failure 1", line);
                enforceTest(equal(state.input, remainder), "unittest failure 2", line);
                enforceTest(state.pos == pos, "unittest failure 3", line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto state = testParser!config(haystack.save);
                state.pos.line += 3;
                static if(i == 0)
                    state.pos.col += 7;
                enforceTest(equal(state.takeUntilAndKeep!needle(), expected), "unittest failure 4", line);
                enforceTest(equal(state.input, remainder), "unittest failure 5", line);
                enforceTest(state.pos == pos, "unittest failure 6", line);
            }
        }}
    }

    static void testFail(alias func, string needle)(string origHaystack, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            auto state = testParser!config(haystack.save);
            assertThrown!XMLParsingException(state.takeUntilAndKeep!needle(), "unittest failure", __FILE__, line);
        }}
    }

    static foreach(func; testRangeFuncs)
    {
        {
            auto haystack = "hello world";
            enum needle = "world";

            static foreach(i; 1 .. needle.length)
                test!(func, needle[0 .. i])(haystack, haystack[0 .. $ - (needle.length - i)], needle[i .. $], 1, 7 + i);
        }
        test!(func, "l")("lello world", "l", "ello world", 1, 2);
        test!(func, "ll")("lello world", "lell", "o world", 1, 5);
        test!(func, "le")("llello world", "lle", "llo world", 1, 4);
        {
            import std.utf : codeLength;
            auto haystack = "プログラミング in D is great indeed";
            auto found = "プログラミング in D is great";
            enum len = cast(int)codeLength!(ElementEncodingType!(typeof(func(haystack))))("プログラミング in D is ");
            enum needle = "great";
            enum remainder = "great indeed";

            static foreach(i; 1 .. needle.length)
            {
                test!(func, needle[0 .. i])(haystack, found[0 .. $ - (needle.length - i)],
                                            remainder[i .. $], 1, len + i + 1);
            }
        }
        static foreach(haystack; ["", "a", "hello"])
            testFail!(func, "x")(haystack);
        static foreach(haystack; ["", "l", "lte", "world", "nomatch"])
            testFail!(func, "le")(haystack);
        static foreach(haystack; ["", "w", "we", "wew", "bwe", "we b", "hello we go", "nomatch"])
            testFail!(func, "web")(haystack);
    }
}


// Returns a slice (or takeExactly) of state.input up to but not including the
// given text, removing both that slice and the given text from state.input in
// the process. If the text is not found, then an XMLParsingException is thrown.
auto takeUntilAndDrop(string text, PS)(PS state)
{
    return _takeUntil!(TakeUntil.dropAndReturn, text, PS)(state);
}

unittest
{
    import std.algorithm.comparison : equal;
    import std.exception : assertThrown;

    static void test(alias func, string needle)(string origHaystack, string expected, string remainder,
                                                int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);

        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto state = testParser!config(haystack.save);
                enforceTest(equal(state.takeUntilAndDrop!needle(), expected), "unittest failure 1", line);
                enforceTest(equal(state.input, remainder), "unittest failure 2", line);
                enforceTest(state.pos == pos, "unittest failure 3", line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto state = testParser!config(haystack.save);
                state.pos.line += 3;
                static if(i == 0)
                    state.pos.col += 7;
                enforceTest(equal(state.takeUntilAndDrop!needle(), expected), "unittest failure 4", line);
                enforceTest(equal(state.input, remainder), "unittest failure 5", line);
                enforceTest(state.pos == pos, "unittest failure 6", line);
            }
        }}
    }

    static void testFail(alias func, string needle)(string origHaystack, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            auto state = testParser!config(haystack.save);
            assertThrown!XMLParsingException(state.takeUntilAndDrop!needle(), "unittest failure", __FILE__, line);
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
void skipUntilAndDrop(string text, PS)(PS state)
{
    return _takeUntil!(TakeUntil.drop, text, PS)(state);
}

unittest
{
    import std.algorithm.comparison : equal;
    import std.exception : assertNotThrown, assertThrown;

    static void test(alias func, string needle)(string origHaystack, string remainder,
                                                int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);

        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto state = testParser!config(haystack.save);
                assertNotThrown!XMLParsingException(state.skipUntilAndDrop!needle(), "unittest failure 1",
                                                    __FILE__, line);
                enforceTest(equal(state.input, remainder), "unittest failure 2", line);
                enforceTest(state.pos == pos, "unittest failure 3", line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto state = testParser!config(haystack.save);
                state.pos.line += 3;
                static if(i == 0)
                    state.pos.col += 7;
                assertNotThrown!XMLParsingException(state.skipUntilAndDrop!needle(), "unittest failure 4",
                                                    __FILE__, line);
                enforceTest(equal(state.input, remainder), "unittest failure 5", line);
                enforceTest(state.pos == pos, "unittest failure 6", line);
            }
        }}
    }

    static void testFail(alias func, string needle)(string origHaystack, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            auto state = testParser!config(haystack.save);
            assertThrown!XMLParsingException(state.skipUntilAndDrop!needle(), "unittest failure", __FILE__, line);
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

auto _takeUntil(TakeUntil tu, string text, PS)(PS state)
{
    import std.algorithm : find;
    import std.ascii : isWhite;

    static assert(isPointer!PS, "_state.savedText was probably passed rather than &_state.savedText");
    static assert(text.find!isWhite().empty);

    enum trackTakeLen = tu != TakeUntil.drop || PS.config.posType == PositionType.lineAndCol;

    alias R = typeof(PS.input);
    auto orig = state.input.save;
    bool found = false;

    static if(trackTakeLen)
        size_t takeLen = 0;

    static if(PS.config.posType == PositionType.lineAndCol)
        size_t lineStart = 0;

    loop: while(!state.input.empty)
    {
        switch(state.input.front)
        {
            case cast(ElementType!R)text[0]:
            {
                static if(text.length == 1)
                {
                    found = true;
                    state.input.popFront();
                    break loop;
                }
                else static if(text.length == 2)
                {
                    state.input.popFront();
                    if(!state.input.empty && state.input.front == text[1])
                    {
                        found = true;
                        state.input.popFront();
                        break loop;
                    }
                    static if(trackTakeLen)
                        ++takeLen;
                    continue;
                }
                else
                {
                    state.input.popFront();
                    auto saved = state.input.save;
                    foreach(i, c; text[1 .. $])
                    {
                        if(state.input.empty)
                        {
                            static if(trackTakeLen)
                                takeLen += i + 1;
                            break loop;
                        }
                        if(state.input.front != c)
                        {
                            state.input = saved;
                            static if(trackTakeLen)
                                ++takeLen;
                            continue loop;
                        }
                        state.input.popFront();
                    }
                    found = true;
                    break loop;
                }
            }
            static if(PS.config.posType != PositionType.none)
            {
                case '\n':
                {
                    static if(trackTakeLen)
                        ++takeLen;
                    nextLine!(PS.config)(state.pos);
                    static if(PS.config.posType == PositionType.lineAndCol)
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

        state.input.popFront();
    }

    static if(PS.config.posType == PositionType.lineAndCol)
        state.pos.col += takeLen - lineStart + text.length;
    if(!found)
        throw new XMLParsingException("Failed to find: " ~ text, state.pos);

    static if(tu == TakeUntil.keepAndReturn)
        return takeExactly(orig, takeLen + text.length);
    else static if(tu == TakeUntil.dropAndReturn)
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

    void skipToOneOf(PS)(PS state)
    {
        static assert(isPointer!PS, "_state.savedText was probably passed rather than &_state.savedText");

        while(!state.input.empty)
        {
            switch(state.input.front)
            {
                foreach(delim; delims)
                    case delim: return;
                static if(PS.config.posType != PositionType.none)
                {
                    case '\n':
                    {
                        nextLine!(PS.config)(state.pos);
                        state.input.popFront();
                        break;
                    }
                }
                default:
                {
                    popFrontAndIncCol(state);
                    break;
                }
            }
        }
        throw new XMLParsingException("Prematurely reached end of document", state.pos);
    }
}

unittest
{
    import std.algorithm.comparison : equal;
    import std.exception : assertNotThrown, assertThrown;

    static void test(alias func, delims...)(string origHaystack, string remainder,
                                            int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);

        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto state = testParser!config(haystack.save);
                assertNotThrown!XMLParsingException(state.skipToOneOf!delims(), "unittest 1", __FILE__, line);
                enforceTest(equal(state.input, remainder), "unittest failure 2", line);
                enforceTest(state.pos == pos, "unittest failure 3", line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto state = testParser!config(haystack.save);
                state.pos.line += 3;
                static if(i == 0)
                    state.pos.col += 7;
                assertNotThrown!XMLParsingException(state.skipToOneOf!delims(), "unittest 4", __FILE__, line);
                enforceTest(equal(state.input, remainder), "unittest failure 5", line);
                enforceTest(state.pos == pos, "unittest failure 6", line);
            }
        }}
    }

    static void testFail(alias func, delims...)(string origHaystack, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            auto state = testParser!config(haystack.save);
            assertThrown!XMLParsingException(state.skipToOneOf!delims(), "unittest failure", __FILE__, line);
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
auto takeEnquotedText(PS)(PS state)
{
    static assert(isPointer!PS, "_state.savedText was probably passed rather than &_state.savedText");
    checkNotEmpty(state);
    immutable quote = state.input.front;
    static foreach(quoteChar; [`"`, `'`])
    {
        // This would be a bit simpler if takeUntilAndDrop took a runtime
        // argument, but in all other cases, a compile-time argument makes more
        // sense, so this seemed like a reasonable way to handle this one case.
        if(quote == quoteChar[0])
        {
            popFrontAndIncCol(state);
            return takeUntilAndDrop!quoteChar(state);
        }
    }
    throw new XMLParsingException("Expected quoted text", state.pos);
}

unittest
{
    import std.algorithm.comparison : equal;
    import std.exception : assertThrown;
    import std.range : only;

    static void test(alias func)(string origHaystack, string expected, string remainder,
                                 int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);

        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto state = testParser!config(haystack.save);
                enforceTest(equal(takeEnquotedText(state), expected), "unittest failure 1", line);
                enforceTest(equal(state.input, remainder), "unittest failure 2", line);
                enforceTest(state.pos == pos, "unittest failure 3", line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto state = testParser!config(haystack.save);
                state.pos.line += 3;
                static if(i == 0)
                    state.pos.col += 7;
                enforceTest(equal(takeEnquotedText(state), expected), "unittest failure 3", line);
                enforceTest(equal(state.input, remainder), "unittest failure 4", line);
                enforceTest(state.pos == pos, "unittest failure 3", line);
            }
        }}
    }

    static void testFail(alias func)(string origHaystack, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            auto state = testParser!config(haystack.save);
            assertThrown!XMLParsingException(state.takeEnquotedText(), "unittest failure", __FILE__, line);
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


// Parses
// Eq ::= S? '=' S?
void stripEq(PS)(PS state)
{
    static assert(isPointer!PS, "_state.savedText was probably passed rather than &_state.savedText");
    stripWS(state);
    if(!stripStartsWith(state, "="))
        throw new XMLParsingException("Expec= missing", state.pos);
    stripWS(state);
}

unittest
{
    import std.algorithm.comparison : equal;
    import std.exception : assertNotThrown, assertThrown;

    static void test(alias func)(string origHaystack, string remainder, int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);

        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto state = testParser!config(haystack.save);
                assertNotThrown!XMLParsingException(stripEq(state), "unittest failure 1", __FILE__, line);
                enforceTest(equal(state.input, remainder), "unittest failure 2", line);
                enforceTest(state.pos == pos, "unittest failure 3", line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto state = testParser!config(haystack.save);
                state.pos.line += 3;
                static if(i == 0)
                    state.pos.col += 7;
                assertNotThrown!XMLParsingException(stripEq(state), "unittest failure 4", __FILE__, line);
                enforceTest(equal(state.input, remainder), "unittest failure 5", line);
                enforceTest(state.pos == pos, "unittest failure 6", line);
            }
        }}
    }

    static void testFail(alias func)(string origHaystack, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            auto state = testParser!config(haystack.save);
            assertThrown!XMLParsingException(state.stripEq(), "unittest failure", __FILE__, line);
        }}
    }

    static foreach(func; testRangeFuncs)
    {
        test!func("=", "", 1, 2);
        test!func("=hello", "hello", 1, 2);
        test!func(" \n\n =hello", "hello", 3, 3);
        test!func("=\n\n\nhello", "hello", 4, 1);
        testFail!func("hello");
        testFail!func("hello=");
    }
}


// If requireNameStart is true, then this removes a name per the Name grammar
// rule from the front of the input and returns it. If requireNameStart is
// false, then this removes a name per the Nmtoken grammar rule and returns it
// (the difference being whether the first character must be a NameStartChar
// rather than a NameChar like the other characters).
// The parsing continues until either one of the given delimiters or an XML
// whitespace character is encountered. The delimiter/whitespace is not returned
// as part of the name and is left at the front of the input.
template takeName(bool requireNameStart, delims...)
{
    static foreach(delim; delims)
    {
        static assert(is(typeof(delim) == char), delim);
        static assert(!isSpace(delim));
    }

    auto takeName(PS)(PS state)
    {
        static assert(isPointer!PS, "_state.savedText was probably passed rather than &_state.savedText");

        import std.format : format;
        import std.utf : decodeFront, UseReplacementDchar;

        assert(!state.input.empty);

        auto orig = state.input.save;
        size_t takeLen;
        static if(requireNameStart)
        {{
            auto decodedC = state.input.decodeFront!(UseReplacementDchar.yes)(takeLen);
            if(!isNameStartChar(decodedC))
                throw new XMLParsingException(format!"Name contains invalid character: '%s'"(decodedC), state.pos);

            if(state.input.empty)
            {
                static if(PS.config.posType == PositionType.lineAndCol)
                    state.pos.col += takeLen;
                return takeExactly(orig, takeLen);
            }
        }}

        loop: while(true)
        {
            immutable c = state.input.front;
            if(isSpace(c))
                break;
            static foreach(delim; delims)
            {
                if(c == delim)
                    break loop;
            }

            size_t numCodeUnits;
            auto decodedC = state.input.decodeFront!(UseReplacementDchar.yes)(numCodeUnits);
            if(!isNameChar(decodedC))
                throw new XMLParsingException(format!"Name contains invalid character: '%s'"(decodedC), state.pos);
            takeLen += numCodeUnits;

            if(state.input.empty)
                break;
        }

        if(takeLen == 0)
            throw new XMLParsingException("Name cannot be empty", state.pos);

        static if(PS.config.posType == PositionType.lineAndCol)
            state.pos.col += takeLen;

        return takeExactly(orig, takeLen);
    }
}

unittest
{
    import std.algorithm.comparison : equal;
    import std.exception : assertThrown;

    static void test(alias func, bool rns, delim...)(string origHaystack, string expected, string remainder,
                                                     int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);

        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto state = testParser!config(haystack.save);
                enforceTest(equal(state.takeName!(rns, delim)(), expected), "unittest failure 1", line);
                enforceTest(equal(state.input, remainder), "unittest failure 2", line);
                enforceTest(state.pos == pos, "unittest failure 3", line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto state = testParser!config(haystack.save);
                state.pos.line += 3;
                static if(i == 0)
                    state.pos.col += 7;
                enforceTest(equal(state.takeName!(rns, delim)(), expected), "unittest failure 4", line);
                enforceTest(equal(state.input, remainder), "unittest failure 5", line);
                enforceTest(state.pos == pos, "unittest failure 6", line);
            }
        }}
    }

    static void testFail(alias func, bool rns, delim...)(string origHaystack, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            auto state = testParser!config(haystack.save);
            assertThrown!XMLParsingException(state.takeName!(rns, delim)(), "unittest failure", __FILE__, line);
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
                static foreach(bool rns; [true, false])
                {
                    test!(func, rns)(strRem, str, remainder, 1, len + 1);
                    test!(func, rns, '=')(strRem, str, remainder, 1, len + 1);
                    test!(func, rns, '>', '|')(hay, str, delimRem, 1, len + 1);
                    test!(func, rns, '|', '>')(hay, str, delimRem, 1, len + 1);
                }
            }}
        }}

        static foreach(bool rns; [true, false])
        {
            static foreach(haystack; [" ", "<", "foo!", "foo!<"])
            {
                testFail!(func, rns)(haystack);
                testFail!(func, rns)(haystack ~ '>');
                testFail!(func, rns, '?')(haystack);
                testFail!(func, rns, '=')(haystack ~ '=');
            }

            testFail!(func, rns, '>')(">");
            testFail!(func, rns, '?')("?");
        }

        static foreach(haystack; ["42", ".", ".a"])
        {
            testFail!(func, true)(haystack);
            test!(func, false)(haystack, haystack, "", 1, haystack.length + 1);
            testFail!(func, true, '>')(haystack);
            test!(func, false, '?')(haystack, haystack, "", 1, haystack.length + 1);
            test!(func, false, '=')(haystack ~ '=', haystack, "=", 1, haystack.length + 1);
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


pragma(inline, true) void popFrontAndIncCol(PS)(PS state)
{
    static assert(isPointer!PS, "_state.savedText was probably passed rather than &_state.savedText");
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

pragma(inline, true) void checkNotEmpty(PS)(PS state, size_t line = __LINE__)
{
    static assert(isPointer!PS, "_state.savedText was probably passed rather than &_state.savedText");
    if(state.input.empty)
        throw new XMLParsingException("Prematurely reached end of document", state.pos, __FILE__, line);
}


version(unittest)
{
    // Wrapping it like this rather than assigning testRangeFuncs directly
    // allows us to avoid having the imports be at module-level, which is
    // generally not esirable with version(unittest).
    alias testRangeFuncs = _testRangeFuncs!();
    template _testRangeFuncs()
    {
        import std.conv : to;
        import std.algorithm : filter;
        import std.meta : AliasSeq;
        import std.utf : byCodeUnit;
        alias _testRangeFuncs = AliasSeq!(a => to!string(a), a => to!wstring(a), a => to!dstring(a),
                                          a => filter!"true"(a), a => fwdCharRange(a), a => rasRefCharRange(a),
                                          a => byCodeUnit(a));
    }

    enum testConfigs = [ Config.init, makeConfig(PositionType.line), makeConfig(PositionType.none) ];

    // The main reason for using this over assert is because of how frequently a
    // mistake in the code results in an XMLParsingException being thrown, and
    // it's more of a pain to track down than an assertion failure, because you
    // have to dig throught the stack trace to figure out which line failed.
    // This way, it tells you like it would with an assertion failure.
    void enforceTest(T)(lazy T value, lazy const(char)[] msg = "unittest failed", size_t line = __LINE__)
    {
        import core.exception : AssertError;
        import std.exception : enforce;
        import std.format : format;
        import std.stdio : writeln;

        try
            enforce!AssertError(value, msg, __FILE__, line);
        catch(XMLParsingException e)
        {
            writeln(e);
            throw new AssertError(format("XMLParsingException thrown: %s", msg), __FILE__, line);
        }
    }
}
