// Written in the D programming language

/++
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
        Whether the `<!DOCTYPE ... >` entities should be skipped while parsing.

        If $(D true), any entities of type $(LREF EntityType.dtdStartTag) and
        $(LREF EntityType.dtdEndTag) and any entities in between will be omitted
        from the parsing results.

        Defaults to $(D SkipDTD.no).
      +/
    auto skipDTD = SkipDTD.no;

    /++
        Whether the prolog should be skipped while parsing.

        If $(D true), any entities prior to the root element will omitted
        from the parsing results.

        Defaults to $(D SkipProlog.no).
      +/
    auto skipProlog = SkipProlog.no;

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

/// See_Also: $(LREF2 skipDTD, Config)
alias SkipDTD = Flag!"SkipDTD";

/// See_Also: $(LREF2 skipProlog, Config)
alias SkipProlog = Flag!"SkipProlog";

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
        assert(config.skipDTD == Config.init.skipDTD);
        assert(config.skipProlog == Config.init.skipProlog);
        assert(config.skipPI == Config.init.skipPI);
        assert(config.skipContentWS == Config.init.skipContentWS);
        assert(config.splitEmpty == Config.init.splitEmpty);
        assert(config.posType == Config.init.posType);
    }
    {
        auto config = makeConfig(SkipComments.yes, PositionType.none);
        assert(config.skipComments == SkipComments.yes);
        assert(config.skipDTD == Config.init.skipDTD);
        assert(config.skipProlog == Config.init.skipProlog);
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
        assert(config.skipDTD == Config.init.skipDTD);
        assert(config.skipProlog == Config.init.skipProlog);
        assert(config.skipPI == Config.init.skipPI);
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
enum simpleXML = makeConfig(SkipComments.yes, SkipDTD.yes, SkipProlog.yes, SkipPI.yes,
                            SplitEmpty.yes, PositionType.lineAndCol);

///
@safe pure nothrow @nogc unittest
{
    static assert(simpleXML.skipComments == SkipComments.yes);
    static assert(simpleXML.skipDTD == SkipDTD.yes);
    static assert(simpleXML.skipProlog == SkipProlog.yes);
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
        The beginning of an `<!ATTLIST ... >` tag.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#attdecls)
      +/
    attlistDeclStart,

    /++
        One of the
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-AttDef, $(I AttDef)s) in
        `'<!ATTLIST' S Name AttDef* S? '>'`.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#attdecls)
      +/
    attDef,

    /++
        The end of an `<!ATTLIST ... >` tag.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#attdecls)
      +/
    attlistDeclEnd,

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
        The beginning of a `<!DOCTYPE ... >` tag where
        $(I $(LINK2 http://www.w3.org/TR/REC-xml/#NT-doctypedecl, intSubset)) is
        not empty.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#sec-prolog-dtd)
      +/
    docTypeStart,

    /++
        A `<!DOCTYPE ... >` tag with no
        $(I $(LINK2 http://www.w3.org/TR/REC-xml/#NT-doctypedecl, intSubset)).

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#sec-prolog-dtd)
      +/
    docTypeEmpty,

    /++
        The `>` indicating the end of a `<!DOCTYPE` tag where
        $(I $(LINK2 http://www.w3.org/TR/REC-xml/#NT-doctypedecl, intSubset))
        was not empty.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#sec-prolog-dtd)
      +/
    docTypeEnd,

    /++
        The end of the document has been reached. There are no more entities to
        parse. Calling any functions of $(LREF EntityCursor) after this has been
        reached is an error, and no node of $(REF EntityTree, dxml, parser, dom)
        will have $(LREF EntityType._documentEnd) as its
        $(type, EntityTree.type, dxml, parser, dom).
      +/
    documentEnd,

    /++
        The `<!ELEMENT ... >` tag.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#elemdecls)
      +/
    elementDecl,

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
        The `<!ENTITY ... >` tag.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#sec-entity-decl)
      +/
    entityDecl,

    /++
        The `<!NOTATION ... >` tag.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#Notations)
      +/
    notationDecl,

    /++
        A $(I PEReference) inside the
        $(I $(LINK2 http://www.w3.org/TR/REC-xml/#NT-doctypedecl, intSubset))
        of a `<!DOCTYPE ...>` declaration.

        e.g. In `<!DOCTYPE Name [ %someref; ]>`, it would be the `%someref;`,
        and $(LREF EntityCursor.name) would return $(D "someref").

        $(I $(LINK2 http://www.w3.org/TR/REC-xml/#NT-PEReference, PEReference)
        does appear elsewhere in the grammar for XML, but in the other cases,
        it's within text and is treated as part of the text just like
        $(I $(LINK2 http://www.w3.org/TR/REC-xml/#NT-CharRef, CharRef))s such
        as `&#20` are. However, within a
        $(I $(LINK2 http://www.w3.org/TR/REC-xml/#NT-doctypedecl, doctypedecl)),
        that doesn't really work, so in that case, it's treated as its own
        entity.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#NT-doctypedecl)($BR)
                  $(LINK http://www.w3.org/TR/REC-xml/#NT-PEReference)
      +/
    peReference,

    /++
        A processing instruction such as `<?foo?>`. Note that
        `<?xml ... ?>` is an $(LREF EntityType.xmlDecl) and not an
        $(LREF EntityType.pi).

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

    /++
        The `<?xml ... ?>` entity that can start an XML 1.0 document and must
        start an XML 1.1 document.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#sec-prolog-dtd)
      +/
    xmlDecl
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
        while(1)
        {
            final switch(_state.grammarPos) with(GrammarPos)
            {
                case documentStart: _parseDocumentStart(); break;
                case prologMisc1:
                {
                    _parseAtPrologMisc!1();
                    static if(config.skipProlog == SkipProlog.yes)
                    {
                        if(_state.grammarPos <= intSubset)
                            continue;
                    }
                    break;
                }
                case prologMisc2:
                {
                    _parseAtPrologMisc!2();
                    static if(config.skipProlog == SkipProlog.yes)
                    {
                        if(_state.grammarPos <= intSubset)
                            continue;
                    }
                    break;
                }
                case intSubset:
                {
                    _parseAtIntSubset();
                    break;
                }
                case intSubsetAttDef:
                {
                    _parseAtAttDef();
                    break;
                }
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
            $(TR $(TD $(LREF2 acttlistDeclStart, EntityType)))
            $(TR $(TD $(LREF2 attDef, EntityType)))
            $(TR $(TD $(LREF2 docTypeStart, EntityType)))
            $(TR $(TD $(LREF2 docTypeEmpty, EntityType)))
            $(TR $(TD $(LREF2 elementDecl, EntityType)))
            $(TR $(TD $(LREF2 elementStart, EntityType)))
            $(TR $(TD $(LREF2 elementEnd, EntityType)))
            $(TR $(TD $(LREF2 elementEmpty, EntityType)))
            $(TR $(TD $(LREF2 entityDecl, EntityType)))
            $(TR $(TD $(LREF2 notationDecl, EntityType)))
            $(TR $(TD $(LREF2 pi, EntityType)))
        )

        See_Also: $(LREF path, EntityCursor)$(BR)$(LREF parentPath, EntityCursor)
      +/
    @property SliceOfR name()
    {
        with(EntityType)
        {
            assert(only(attlistDeclStart, attDef, docTypeStart, docTypeEmpty, elementDecl, elementStart, elementEnd,
                        elementEmpty, entityDecl, notationDecl, pi).canFind(_state.type));
        }
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
            $(TR $(TD $(LREF2 docTypeStart, EntityType)))
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
            assert(only(docTypeStart, elementStart, elementEmpty, elementEnd, pi).canFind(_state.type));

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
            assert(only(docTypeStart, elementStart, elementEnd, elementEmpty, pi).canFind(_state.type));

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
            assert(only(elementStart, elementEmpty).canFind(_state.type));

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

            // "<?xml version='1.0'?>\n" ~
            assert(cursor.next() == EntityType.xmlDecl);

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

            // "<?xml version='1.0'?>\n" ~
            assert(cursor.next() == EntityType.xmlDecl);

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


    /++
        Information parsed from an `<?xml ... ?>` declaration.

        Note that while XML 1.1 requires this declaration, it's optional in XML
        1.0.

        $(TABLE
            $(TR $(TH Supported $(LREF EntityType)s:)),
            $(TR $(TD $(LREF2 _xmlDecl, EntityType)))
        )

        Returns: The XMLDecl corresponding to the current entity.

        Throws: $(LREF XMLParsingException) on invalid XML.

        See_Also: $(LINK http://www.w3.org/TR/xml/#NT-XMLDecl)
      +/
    struct XMLDecl
    {
        /++
            The version of XML that the document contains.
          +/
        SliceOfR xmlVersion;

        /++
            The encoding of the text in the XML document.

            Note that dxml only supports UTF-8, UTF-16, and UTF-32, and it is
            assumed that the encoding matches the character type. The parser
            ignores this field aside from providing its value as part of an
            $(LREF XMLDecl).

            Anyone looking to make their program determine the encoding based on
            the $(D "encoding") field, should read
            $(LINK http://www.w3.org/TR/REC-xml/#sec-guessing).
          +/
        Nullable!SliceOfR encoding;

        /++
            $(D true) if the XML document does $(I not) contain any external
            references. $(D false) if it does or may contain external references.
            It's null if the $(D "standalone") declaration was not included in
            the `<?xml ..?>` declaration.
          +/
        Nullable!bool standalone;
    }

    /// Ditto
    @property XMLDecl xmlDecl()
    {
        assert(_state.type == EntityType.xmlDecl);

        import std.ascii : isDigit;
        import std.typecons : nullable;

        void throwXPE(string msg, size_t line = __LINE__)
        {
            throw new XMLParsingException(msg, _state.savedText.pos, __FILE__, line);
        }

        // We shouldn't need to use .init like this, but when compiling the
        // examples with filter, we get an error about accessing the frame
        // pointer if we don't, which seems like a bug in dmd which should
        // probably be reduced and reported. It may be https://issues.dlang.org/show_bug.cgi?id=13945
        XMLDecl retval = XMLDecl.init;

        // XMLDecl      ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
        // VersionInfo  ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
        // Eq           ::= S? '=' S?
        // VersionNum   ::= '1.' [0-9]+
        // EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
        // EncName      ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
        // SDDecl       ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))

        // The '<?xml' and '?>' were already stripped off before _state.savedText
        // was set.

        if(!stripWS(&_state.savedText))
            throwXPE("There must be whitespace after <?xml");

        if(!stripStartsWith(&_state.savedText, "version"))
            throwXPE("version missing after <?xml");
        stripEq(&_state.savedText);

        {
            auto temp = takeEnquotedText(&_state.savedText);
            retval.xmlVersion = stripBCU!R(temp.save);
            if(temp.length != 3)
                throwXPE("Invalid or unsupported XML version number");
            if(temp.front != '1')
                throwXPE("Invalid or unsupported XML version number");
            temp.popFront();
            if(temp.front != '.')
                throwXPE("Invalid or unsupported XML version number");
            temp.popFront();
            if(!isDigit(temp.front))
                throwXPE("Invalid or unsupported XML version number");
        }

        auto wasSpace = stripWS(&_state.savedText);
        if(_state.savedText.input.empty)
            return retval;

        if(_state.savedText.input.front == 'e')
        {
            if(!wasSpace)
                throwXPE("There must be whitespace before the encoding declaration");
            popFrontAndIncCol(&_state.savedText);
            if(!stripStartsWith(&_state.savedText, "ncoding"))
                throwXPE("Invalid <?xml ... ?> declaration in prolog");
            stripEq(&_state.savedText);
            retval.encoding = nullable(stripBCU!R(takeEnquotedText(&_state.savedText)));

            wasSpace = stripWS(&_state.savedText);
            if(_state.savedText.input.empty)
                return retval;
        }

        if(_state.savedText.input.front == 's')
        {
            if(!wasSpace)
                throwXPE("There must be whitespace before the standalone declaration");
            popFrontAndIncCol(&_state.savedText);
            if(!stripStartsWith(&_state.savedText, "tandalone"))
                throwXPE("Invalid <?xml ... ?> declaration in prolog");
            stripEq(&_state.savedText);

            auto pos = _state.savedText.pos;
            auto standalone = takeEnquotedText(&_state.savedText);
            if(standalone.save.equalCU("yes"))
                retval.standalone = nullable(true);
            else if(standalone.equalCU("no"))
                retval.standalone = nullable(false);
            else
                throw new XMLParsingException("If standalone is present, its value must be yes or no", pos);
            stripWS(&_state.savedText);
        }

        if(!_state.savedText.input.empty)
            throwXPE("Invalid <?xml ... ?> declaration in prolog");

        return retval;
    }

    static if(compileInTests) unittest
    {
        import std.array : replace;
        import std.exception : assertThrown;
        import std.range : only;

        static foreach(func; testRangeFuncs)
        {
            {
                foreach(i; 0 .. 4)
                {
                    auto xml = "<?xml version='1.0'?><root/>";
                    if(i > 1)
                        xml = xml.replace(`'`, `"`);
                    if(i % 2 == 1)
                        xml = xml.replace("1.0", "1.1");
                    auto cursor = parseXML(func(xml));
                    cursor.next();
                    auto xmlDecl = cursor.xmlDecl;
                    assert(equalCU(xmlDecl.xmlVersion, i % 2 == 0 ? "1.0" : "1.1"));
                    assert(xmlDecl.encoding.isNull);
                    assert(xmlDecl.standalone.isNull);
                }
            }
            {
                foreach(i; 0 .. 4)
                {
                    auto xml = "<?xml version='1.0' encoding='UTF-8'?><root/>";
                    if(i > 1)
                        xml = xml.replace(`'`, `"`);
                    if(i % 2 == 1)
                        xml = xml.replace("1.0", "1.1");
                    auto cursor = parseXML(func(xml));
                    cursor.next();
                    auto xmlDecl = cursor.xmlDecl;
                    assert(equalCU(xmlDecl.xmlVersion, i % 2 == 0 ? "1.0" : "1.1"));
                    assert(equalCU(xmlDecl.encoding.get, "UTF-8"));
                    assert(xmlDecl.standalone.isNull);
                }
            }
            {
                foreach(i; 0 .. 8)
                {
                    auto xml = "<?xml version='1.0' encoding='UTF-8' standalone='yes'?><root/>";
                    if(i > 3)
                        xml = xml.replace(`'`, `"`).replace("yes", "no");
                    if(i % 2 == 1)
                        xml = xml.replace("1.0", "1.1");
                    auto cursor = parseXML(func(xml));
                    cursor.next();
                    auto xmlDecl = cursor.xmlDecl;
                    assert(equalCU(xmlDecl.xmlVersion, i % 2 == 0 ? "1.0" : "1.1"));
                    assert(equalCU(xmlDecl.encoding.get, "UTF-8"));
                    assert(xmlDecl.standalone == (i < 4));
                }
            }
            {
                foreach(i; 0 .. 8)
                {
                    auto xml = "<?xml version='1.0' standalone='yes'?><root/>";
                    if(i > 3)
                        xml = xml.replace(`'`, `"`).replace("yes", "no");
                    if(i % 2 == 1)
                        xml = xml.replace("1.0", "1.1");
                    auto cursor = parseXML(func(xml));
                    cursor.next();
                    auto xmlDecl = cursor.xmlDecl;
                    assert(equalCU(xmlDecl.xmlVersion, i % 2 == 0 ? "1.0" : "1.1"));
                    assert(xmlDecl.encoding.isNull);
                    assert(xmlDecl.standalone == (i < 4));
                }
            }

            foreach(xml; only("<?xml version='1.0'><root/>",
                              "<?xml version='1.0'><?root/>",
                              "<?xml version='1.0'? ><root/>",
                              "< ?xml version='1.0'?><root/>"))
            {
                auto cursor = parseXML(func(xml));
                assertThrown!XMLParsingException(cursor.next());
            }

            foreach(xml; only("<?xml version='1.a'?><root/>",
                              "<?xml version='0.0'?><root/>",
                              "<?xml version='2.0'?><root/>",
                              "<?xml version='10'?><root/>",
                              "<?xml version='100'?><root/>",
                              "<?xml version='1,0'?><root/>",
                              "<?xml versio='1.0'?><root/>",
                              "<?xml version='1.0 '?><root/>",
                              "<?xml?><root/>",
                              "<?xml version='1.0'encoding='UTF-8'?><root/>",
                              "<?xml version='1.0' encoding='UTF-8'standalone='yes'?><root/>",
                              "<?xml version='1.0'standalone='yes'?><root/>",
                              "<?xml version='1.0'\vencoding='UTF-8'?><root/>",
                              "<?xml version='1.0' encoding='UTF-8'\vstandalone='yes'?><root/>",
                              "<?xml version='1.0'\vstandalone='yes'?><root/>",
                              "<?xml version='1.0' standalone='yes' encoding='UTF-8'?><root/>",
                              "<?xml version='1.0' standalone='YES'?><root/>",
                              "<?xml version='1.0' standalone='NO'?><root/>",
                              "<?xml version='1.0' standalone='y'?><root/>",
                              "<?xml version='1.0' standalone='n'?><root/>",
                              "<?xml version='1.0  standalone='yes'?><root/>",
                              "<?xml version='1.0  standalone='yes''?><root/>",
                              "<?xml version='1.0' ecoding='UTF-8'?><root/>",
                              "<?xml version='1.0' silly='yes'?><root/>"))
            {
                auto cursor = parseXML(func(xml));
                cursor.next();
                assertThrown!XMLParsingException(cursor.xmlDecl);
            }
        }
    }



    /++
        Information for an external or public ID as parsed from a `SYSTEM ...` or
        `PUBLIC ...` declaration inside a `<!DOCTYPE ...>` declaration.

        In the case of `<!DOCTYPE ...>` (where the external ID is optional),
        if there is no external ID present, then the return value will be null.

        $(TABLE
            $(TR $(TH Supported $(LREF EntityType)s:)),
            $(TR $(TD $(LREF2 docTypeStart, EntityType))),
            $(TR $(TD $(LREF2 docTypeEmpty, EntityType))),
            $(TR $(TD $(LREF2 notation, EntityType)))
        )

        Returns: The external ID or public ID portion of the current entity.

        See_Also: $(LINK http://www.w3.org/TR/xml/#NT-ExternalID)$(BR)
                  $(LINK http://www.w3.org/TR/xml/#NT-PublicID)
      +/
    struct ID
    {
        /++
            The PubidLiteral portion of a `PUBLIC ...` delaration. If the
            ID was a `SYSTEM ...` declaration, then this is null.
          +/
        Nullable!SliceOfR publicLiteral;

        /++
            The SystemLiteral portion of a `SYSTEM ...` or `PUBLIC ...` delaration.
            If the ID is a PublicID, then this is null.
          +/
        Nullable!SliceOfR systemLiteral;
    }

    /// Ditto
    @property Nullable!ID id()
    {
        with(EntityType)
            assert(only(docTypeStart, docTypeEmpty, notationDecl).canFind(_state.type));
        return _state.id;
    }

    ///
    static if(compileInTests) unittest
    {
        {
            auto xml = "<!DOCTYPE dentist PUBLIC 'foo' 'bar'\n" ~
                       "    [\n" ~
                       "        <!NOTATION good PUBLIC 'something'>\n" ~
                       "        <!NOTATION bad PUBLIC 'else' 'to'>\n" ~
                       "        <!NOTATION ugly SYSTEM 'parse'>\n" ~
                       "    ]>\n" ~
                       "<root/>";

            auto cursor = parseXML(xml);
            assert(cursor.next() == EntityType.docTypeStart);
            assert(cursor.name == "dentist");
            assert(cursor.id.publicLiteral == "foo");
            assert(cursor.id.systemLiteral == "bar");

            assert(cursor.next() == EntityType.notationDecl);
            assert(cursor.name == "good");
            assert(cursor.id.publicLiteral == "something");
            assert(cursor.id.systemLiteral.isNull);

            assert(cursor.next() == EntityType.notationDecl);
            assert(cursor.name == "bad");
            assert(cursor.id.publicLiteral == "else");
            assert(cursor.id.systemLiteral == "to");

            assert(cursor.next() == EntityType.notationDecl);
            assert(cursor.name == "ugly");
            assert(cursor.id.publicLiteral.isNull);
            assert(cursor.id.systemLiteral == "parse");

            assert(cursor.next() == EntityType.docTypeEnd);

            assert(cursor.next() == EntityType.elementEmpty);
            assert(cursor.name == "root");

            assert(cursor.next() == EntityType.documentEnd);
        }
        {
            auto xml = "<!DOCTYPE archaelogist SYSTEM 'digging'>\n" ~
                       "<root/>";

            auto cursor = parseXML(xml);
            assert(cursor.next() == EntityType.docTypeEmpty);
            assert(cursor.name == "archaelogist");
            assert(cursor.id.publicLiteral.isNull);
            assert(cursor.id.systemLiteral == "digging");

            assert(cursor.next() == EntityType.elementEmpty);
            assert(cursor.name == "root");

            assert(cursor.next() == EntityType.documentEnd);
        }
        {
            auto xml = "<!DOCTYPE optometrist>\n" ~
                       "<root/>";

            auto cursor = parseXML(xml);
            assert(cursor.next() == EntityType.docTypeEmpty);
            assert(cursor.name == "optometrist");
            assert(cursor.id.isNull);

            assert(cursor.next() == EntityType.elementEmpty);
            assert(cursor.name == "root");

            assert(cursor.next() == EntityType.documentEnd);
        }
    }


    /++
        Information for an `<!ENTIITY ...>` declaration.

        It either contains an entity value, an external ID, or an external ID
        and an `NDATA` declaration.

        $(TABLE
            $(TR $(TH Supported $(LREF EntityType)s:)),
            $(TR $(TD $(LREF2 entityDecl, EntityType))),
        )

        Returns: The EntityDef or PEDecl portion of an `<!ENTITY ...>`
        declaration (both represented by $(LREF EntityDef)).

        Throws: $(LREF XMLParsingException) on invalid XML.

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#NT-EntityDecl)$(BR)
                  $(LINK http://www.w3.org/TR/REC-xml/#NT-EntityValue)
      +/
    struct EntityDef
    {
        /++
            True when the `<!ENTITY ...>` declaration contains the '%' sign after
            `ENTITY`.
          +/
        bool parsedEntity;

        ///
        static if(compileInTests) unittest
        {
            import std.algorithm.comparison : equal;

            auto xml = "<!DOCTYPE surgeon\n" ~
                       "    [\n" ~
                       "        <!ENTITY one 'foo'>\n" ~
                       "        <!ENTITY % two 'bar'>\n" ~
                       "    ]>\n" ~
                       "<root/>";

            auto cursor = parseXML(xml);
            assert(cursor.next == EntityType.docTypeStart);
            assert(cursor.name == "surgeon");

            assert(cursor.next == EntityType.entityDecl);
            assert(cursor.name == "one");

            auto one = cursor.entityDef;
            assert(!one.parsedEntity);
            assert(!one.value.isNull && equal(one.value, "foo"));

            assert(cursor.next == EntityType.entityDecl);
            assert(cursor.name == "two");

            auto two = cursor.entityDef;
            assert(two.parsedEntity);
            assert(!two.value.isNull && equal(two.value, "bar"));

            assert(cursor.next() == EntityType.docTypeEnd);

            assert(cursor.next() == EntityType.elementEmpty);
            assert(cursor.name == "root");

            assert(cursor.next() == EntityType.documentEnd);
        }

        /++
          +/
        Nullable!SliceOfR value;

        /++
          +/
        Nullable!ID externalID;

        /++
          +/
        SliceOfR ndataName;
    }

    /// Ditto
    @property EntityDef entityDef() nothrow
    {
        assert(_state.type == EntityType.entityDecl);
        return _state.entityDef;
    }

    static if(compileInTests) unittest
    {
        auto xml = "<!DOCTYPE pediatrist\n" ~
                   "    [\n" ~
                   "        <!ENTITY Name 'a value'>\n" ~
                   "        <!ENTITY abcd 'another value'>\n" ~
                   "        <!ENTITY nom PUBLIC 'foo' 'bar' NDATA baz>\n" ~
                   "        <!ENTITY xyzzy SYSTEM 'hello' NDATA world>\n" ~
                   "        <!ENTITY % e01 'nothing to see'>\n" ~
                   "        <!ENTITY % e02 PUBLIC '1' '2'>\n" ~
                   "        <!ENTITY % e03 SYSTEM '3'>\n" ~
                   "    ]>\n" ~
                   "<root/>";

        auto cursor = parseXML(xml);
        assert(cursor.next() == EntityType.docTypeStart);
        assert(cursor.name == "pediatrist");

        import std.stdio;
        // "<!ENTITY Name 'a value'>"
        assert(cursor.next() == EntityType.entityDecl);
        assert(cursor.name == "Name");
        assert(!cursor.entityDef.parsedEntity);
        assert(cursor.entityDef.value == "a value");
        assert(cursor.entityDef.externalID.isNull);
        assert(cursor.entityDef.ndataName.empty);

        // "<!ENTITY abcd 'another value'>"
        assert(cursor.next() == EntityType.entityDecl);
        assert(cursor.name == "abcd");
        assert(!cursor.entityDef.parsedEntity);
        assert(cursor.entityDef.value == "another value");
        assert(cursor.entityDef.externalID.isNull);
        assert(cursor.entityDef.ndataName.empty);

        // "<!ENTITY nom PUBLIC 'foo' 'bar' NDATA baz>"
        assert(cursor.next() == EntityType.entityDecl);
        assert(cursor.name == "nom");
        assert(!cursor.entityDef.parsedEntity);
        assert(cursor.entityDef.value.isNull);
        assert(cursor.entityDef.externalID.publicLiteral == "foo");
        assert(cursor.entityDef.externalID.systemLiteral == "bar");
        assert(cursor.entityDef.ndataName == "baz");

        // "<!ENTITY xyzzy SYSTEM 'hello' NDATA world>"
        assert(cursor.next() == EntityType.entityDecl);
        assert(cursor.name == "xyzzy");
        assert(!cursor.entityDef.parsedEntity);
        assert(cursor.entityDef.value.isNull);
        assert(cursor.entityDef.externalID.publicLiteral.isNull);
        assert(cursor.entityDef.externalID.systemLiteral == "hello");
        assert(cursor.entityDef.ndataName == "world");

        // "<!ENTITY % e01 'nothing to see'>"
        assert(cursor.next() == EntityType.entityDecl);
        assert(cursor.name == "e01");
        assert(cursor.entityDef.parsedEntity);
        assert(cursor.entityDef.value == "nothing to see");
        assert(cursor.entityDef.externalID.isNull);
        assert(cursor.entityDef.ndataName.empty);

        // "<!ENTITY % e02 PUBLIC '1' '2'>"
        assert(cursor.next() == EntityType.entityDecl);
        assert(cursor.name == "e02");
        assert(cursor.entityDef.parsedEntity);
        assert(cursor.entityDef.value.isNull);
        assert(cursor.entityDef.externalID.publicLiteral == "1");
        assert(cursor.entityDef.externalID.systemLiteral == "2");
        assert(cursor.entityDef.ndataName.empty);

        // "<!ENTITY % e03 SYSTEM '3'>"
        assert(cursor.next() == EntityType.entityDecl);
        assert(cursor.name == "e03");
        assert(cursor.entityDef.parsedEntity);
        assert(cursor.entityDef.value.isNull);
        assert(cursor.entityDef.externalID.publicLiteral.isNull);
        assert(cursor.entityDef.externalID.systemLiteral == "3");
        assert(cursor.entityDef.ndataName.empty);

        // ">"
        assert(cursor.next() == EntityType.docTypeEnd);

        // "<root/>"
        assert(cursor.next() == EntityType.elementEmpty);
        assert(cursor.name == "root");

        assert(cursor.next() == EntityType.documentEnd);
    }


    /++
        Information for an
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-AttDef, $(I AttDef)) in
        `'<!ATTLIST' S Name AttDef* S? '>'`.

        The name of the $(LINK2 http://www.w3.org/TR/REC-xml/#NT-AttDef, $(I AttDef))
        itself is contained in $(LREF EntityCursor.name), whereas the
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-AttType, $(I AttType)) and
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-DefaultDecl, $(I DefaultDecl)) are
        contained in an AttDefInfo.

        $(TABLE
            $(TR $(TH Supported $(LREF EntityType)s:)),
            $(TR $(TD $(LREF2 attDef, EntityType))),
        )

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#attdecls)
      +/
    struct AttDefInfo
    {
    public:
        /++
            The $(LINK2 http://www.w3.org/TR/REC-xml/#NT-AttType, $(I AttType)) for
            the $(LINK2 http://www.w3.org/TR/REC-xml/#NT-AttDef, $(I AttDef)).

            See_also: $(LREF AttType)$(BR)
                      $(LINK http://www.w3.org/TR/REC-xml/#NT-AttType)
          +/
        AttType attType;

        /++
            If $(D attType == AttType.notationType), then this will return a
            range of the values in the notation. Otherwise, it's an error to
            call notationValues.

            $(TABLE
                $(TR $(TH Supported $(LREF AttType)s:)),
                $(TR $(TD $(LREF2 notationType, AttType))),
            )

            See_also: $(LINK http://www.w3.org/TR/REC-xml/#NT-NotationType)
          +/
        @property auto notationValues()
        {
            assert(attType == AttType.notationType);
            auto retval = PipeSeparatedRange!(PSRConfig.name)(_savedText.save);
            if(retval.empty)
                throw new XMLParsingException("NOTATION must have at least one value", _savedText.pos);
            return retval;
        }

        /++
            If $(D attType == AttType.enumeration), then this will return a
            range of the values in the enumeration. Otherwise, it's an error to
            call enumValues.

            $(TABLE
                $(TR $(TH Supported $(LREF AttType)s:)),
                $(TR $(TD $(LREF2 enumeration, AttType))),
            )

            See_also: $(LINK http://www.w3.org/TR/REC-xml/#NT-Enumeration)
          +/
        @property auto enumValues()
        {
            assert(attType == AttType.enumeration);
            auto retval = PipeSeparatedRange!(PSRConfig.nmtoken)(_savedText.save);
            if(retval.empty)
                throw new XMLParsingException("Enumeration must have at least one value", _savedText.pos);
            return retval;
        }

        /++
            The $(LINK2 http://www.w3.org/TR/REC-xml/#NT-DefaultDecl, $(I DefaultDecl))
            for the $(LINK2 http://www.w3.org/TR/REC-xml/#NT-AttDef, $(I AttDef)).

            See_also: $(LREF DefaultDecl)$(BR)
                      $(LINK http://www.w3.org/TR/REC-xml/#NT-DefaultDecl)
          +/
        DefaultDecl defaultDecl;

        /++
            The $(LINK2 http://www.w3.org/TR/REC-xml/#NT-AttValue, $(I AttValue))
            for the $(LINK2 http://www.w3.org/TR/REC-xml/#NT-DefaultDecl, $(I DefaultDecl)).

            If $(D defaultDecl == DefaultDecl.required) or
            $(D defaultDecl == DefaultDecl.implied), then it is an error to call
            attValue.

            If $(D defaultDecl == DefaultDecl.fixed) or
            $(D defaultDecl == DefaultDecl.unfixed), and this is empty, then
            that is because there was nothing between the quotes of the
            $(LINK2 http://www.w3.org/TR/REC-xml/#NT-AttValue, $(I AttValue)).

            $(TABLE
                $(TR $(TH Supported $(LREF DefaultDecl)s:)),
                $(TR $(TD $(LREF2 fixed, DefaultDecl))),
                $(TR $(TD $(LREF2 unfixed, DefaultDecl))),
            )

            See_also: $(LREF DefaultDecl)$(BR)
                      $(LINK http://www.w3.org/TR/REC-xml/#NT-DefaultDecl)
          +/
        @property SliceOfR attValue()
        {
            assert(defaultDecl == DefaultDecl.fixed || defaultDecl == DefaultDecl.unfixed);
            return _attValue.save;
        }

    private:

        ParserState.SavedText _savedText;
        SliceOfR _attValue;
    }

    /// Ditto
    @property AttDefInfo attDefInfo() nothrow
    {
        assert(_state.type == EntityType.attDef);
        return _state.attDefInfo;
    }


    /++
        Information for an `<!ELEMENT ...>`.
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-elementDecl, $(I elementDecl))

        The name of the $(LINK2 http://www.w3.org/TR/REC-xml/#NT-elementDecl,
        $(I elementDecl)) itself is contained in $(LREF EntityCursor.name),
        whereas the
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-contentspec, $(I contentspec))
        is contained in a ContentSpec.

        $(TABLE
            $(TR $(TH Supported $(LREF EntityType)s:)),
            $(TR $(TD $(LREF2 elementDecl, EntityType))),
        )

        See_Also: $(LINK http://www.w3.org/TR/REC-xml/#elemdecls)
      +/
    struct ContentSpec
    {
    public:

        /++
            The type of data that the $(I contentspec) contains.

            See_Also: $(LREF ContentSpecType)
          +/
        ContentSpecType type;

        /++
            If $(D type == ContentSpecType.mixedStar), then this will return a
            range of the names in the
            $(LINK2 http://www.w3.org/TR/REC-xml/#NT-Mixed, mixed-content declaration).
            Otherwise, it's an error to call mixedNames.

            $(TABLE
                $(TR $(TH Supported $(LREF ContentSpecType)s:)),
                $(TR $(TD $(LREF2 mixed, AttType))),
            )

            Throws: $(LREF XMLParsingException on invalid XML)

            See_also: $(LINK http://www.w3.org/TR/REC-xml/#NT-Mixed)
          +/
        auto mixedNames()
        {
            assert(type == ContentSpecType.mixedStar);
            return PipeSeparatedRange!(PSRConfig.pcdataAndName)(_savedText.save);
        }

        /++
            If $(D type == ContentSpecType.children), then this will return a
            slice/$(D takeExactly) of the `<!ELEMENT ...>` declaration, starting
            with the $(D '$(LPAREN)') that starts the element-content model
            and ending with with the $(D '$(RPAREN)'), $(D '?'), $(D '*'),
            or $(D '+') that terminates it.

            If $(D type != ContentSpecType.children), then it is an error to
            call children.

            At some point, a helper function should probably be added to parse
            this and turn into into something more easily used, but for now,
            it's just provided as text.

            $(TABLE
                $(TR $(TH Supported $(LREF ContentSpecType)s:)),
                $(TR $(TD $(LREF2 children, AttType))),
            )

            See_also: $(LINK http://www.w3.org/TR/REC-xml/#sec-element-content)
          +/
        @property SliceOfR children()
        {
            assert(type == ContentSpecType.children);
            return stripBCU!R(_savedText.input);
        }


    private:

        ParserState.SavedText _savedText;
    }

    /// Ditto
    @property contentSpec() nothrow
    {
        assert(_state.type == EntityType.elementDecl);
        return _state.contentSpec;
    }

    ///
    static if(compileInTests) unittest
    {
        import std.algorithm.comparison : equal;

        auto xml = "<!DOCTYPE medical [\n" ~
                   "    <!ELEMENT br EMPTY>\n" ~
                   "    <!ELEMENT container ANY>\n" ~
                   "    <!ELEMENT p (#PCDATA|emph|blah)* >\n" ~
                   "    <!ELEMENT foo (#PCDATA)* >\n" ~
                   "    <!ELEMENT bar (#PCDATA) >\n" ~
                   "    <!ELEMENT spec (front, body, back?)>\n" ~
                   "    <!ELEMENT div1 (head, (p | list | note)*, div2*)>\n" ~
                   "]> <potato/>";

        auto cursor = parseXML(xml);
        assert(cursor.next() == EntityType.docTypeStart);
        assert(cursor.name == "medical");

        // "<!ELEMENT br EMPTY>"
        assert(cursor.next() == EntityType.elementDecl);
        assert(cursor.name == "br");
        assert(cursor.contentSpec.type == ContentSpecType.empty);

        // "<!ELEMENT container ANY>"
        assert(cursor.next() == EntityType.elementDecl);
        assert(cursor.name == "container");
        assert(cursor.contentSpec.type == ContentSpecType.any);

        // "<!ELEMENT p (#PCDATA|emph|blah)* >"
        assert(cursor.next() == EntityType.elementDecl);
        assert(cursor.name == "p");
        assert(cursor.contentSpec.type == ContentSpecType.mixedStar);
        assert(equal(cursor.contentSpec.mixedNames, ["emph", "blah"]));

        // "<!ELEMENT foo (#PCDATA)* >";
        assert(cursor.next() == EntityType.elementDecl);
        assert(cursor.name == "foo");
        assert(cursor.contentSpec.type == ContentSpecType.mixedStar);
        assert(cursor.contentSpec.mixedNames.empty);

        // "<!ELEMENT bar (#PCDATA) >"
        assert(cursor.next() == EntityType.elementDecl);
        assert(cursor.name == "bar");
        assert(cursor.contentSpec.type == ContentSpecType.mixedSingle);

        // "<!ELEMENT spec (front, body, back?)>"
        assert(cursor.next() == EntityType.elementDecl);
        assert(cursor.name == "spec");
        assert(cursor.contentSpec.type == ContentSpecType.children);
        assert(cursor.contentSpec.children == "(front, body, back?)");

        // "<!ELEMENT div1 (head, (p | list | note)*, div2*)>"
        assert(cursor.next() == EntityType.elementDecl);
        assert(cursor.name == "div1");
        assert(cursor.contentSpec.type == ContentSpecType.children);
        assert(cursor.contentSpec.children == "(head, (p | list | note)*, div2*)");

        assert(cursor.next() == EntityType.docTypeEnd);

        assert(cursor.next() == EntityType.elementEmpty);
        assert(cursor.name == "potato");

        assert(cursor.next() == EntityType.documentEnd);
    }


private:

    enum PSRConfig
    {
        name,
        nmtoken,
        pcdataAndName
    }

    struct PipeSeparatedRange(PSRConfig psrc)
    {
        enum requireNameStart = psrc != PSRConfig.nmtoken;

    public:

        bool empty() { return _savedText.input.empty; }

        SliceOfR front() { return _front.save; }

        void popFront()
        {
            auto state = &_savedText;
            if(state.input.front == ')')
            {
                popFrontAndIncCol(state);
                assert(state.input.empty);
                return;
            }
            popFrontAndIncCol(state);
            stripWS(state);
            _front = stripBCU!R(state.takeName!(requireNameStart, '|', ')')());
            stripWS(state);
            checkNotEmpty(state);
            switch(state.input.front)
            {
                case '|':
                case ')': break;
                default: throw new XMLParsingException("Expected | or )", state.pos);
            }
        }

        @property save() { return typeof(this)(_front.save, _savedText.save); }

    private:

        this(SliceOfR front, ParserState.SavedText savedText)
        {
            _front = front;
            _savedText = savedText;
        }

        this(ParserState.SavedText savedText)
        {
            assert(!savedText.input.empty);
            assert(savedText.input.front == '(');
            _savedText = savedText;

            static if(psrc == PSRConfig.pcdataAndName)
            {
                auto state = &_savedText;
                popFrontAndIncCol(state);
                stripWS(state);
                immutable stripped = state.stripStartsWith("#PCDATA");
                assert(stripped);
                stripWS(state);
                checkNotEmpty(state);
                switch(state.input.front)
                {
                    case '|':
                    case ')': popFront(); break;
                    default: throw new XMLParsingException("Expected | or )", state.pos);
                }
            }
            else
                popFront();
        }

        SliceOfR _front;
        ParserState.SavedText _savedText;
    }


    void _parseDocumentStart()
    {
        if(_state.stripStartsWith("<?xml"))
        {
            static if(config.skipProlog == SkipProlog.yes)
            {
                _state.skipUntilAndDrop!"?>"();
                _state.grammarPos = GrammarPos.prologMisc1;
                next();
            }
            else
            {
                _state.savedText.pos = _state.pos;
                _state.savedText.input = _state.takeUntilAndDrop!"?>"();
                _state.type = EntityType.xmlDecl;
                _state.grammarPos = GrammarPos.prologMisc1;
            }
        }
        else
            _parseAtPrologMisc!1();
    }

    static if(compileInTests) unittest
    {
        import core.exception : AssertError;
        import std.exception : assertNotThrown, enforce;
        import std.range : iota, lockstep, only;

        static void test(alias func, SkipProlog skipProlog = SkipProlog.no)
                        (string xml, int row, int col, size_t line = __LINE__)
        {
            static foreach(i, config; [makeConfig(skipProlog),
                                       makeConfig(skipProlog, PositionType.line),
                                       makeConfig(skipProlog, PositionType.none)])
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
            test!func("<?xml\n\n\nversion='1.8'\n\n\n\nencoding='UTF-8'\n\n\nstandalone='yes'\n?><root/>", 12, 3);
            test!func("<?xml\n\n\n    \r\r\r\n\nversion='1.8'?><root/>", 6, 16);
            test!func("<?xml\n\n\n    \r\r\r\n\nversion='1.8'?>\n     <root/>", 6, 16);
            test!func("<root/>", 1, 8);
            test!func("\n\t\n <root/>   \n", 3, 9);
            test!(func, SkipProlog.yes)
                 ("<?xml\n\n\nversion='1.8'\n\n\n\nencoding='UTF-8'\n\n\nstandalone='yes'\n?><root/>", 12, 10);
            test!(func, SkipProlog.yes)("<?xml\n\n\n    \r\r\r\n\nversion='1.8'?><root/>", 6, 23);
            test!(func, SkipProlog.yes)("<?xml\n\n\n    \r\r\r\n\nversion='1.8'?>\n     <root/>", 7, 13);
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
                    _parseComment!(config.skipProlog == SkipProlog.yes || config.skipComments == SkipComments.yes)();
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
                _parsePI!(config.skipProlog == SkipProlog.yes || config.skipPI == SkipPI.yes);
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
    void _parseComment(bool skip = config.skipComments == SkipComments.yes)()
    {
        static if(skip)
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
    void _parsePI(bool skip = config.skipPI == SkipPI.yes)()
    {
        assert(_state.input.front == '?');
        popFrontAndIncCol(_state);
        static if(skip)
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
        static if(config.skipProlog == SkipProlog.yes)
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
        else
        {
            _state.name = _state.takeName!(true, '>')();
            immutable wasSpace = _state.stripWS();
            checkNotEmpty(_state);

            switch(_state.input.front)
            {
                case 'S':
                case 'P':
                {
                    if(!wasSpace)
                    {
                        throw new XMLParsingException("There must be whitespace between the name and the ID",
                                                      _state.pos);
                    }
                    _state.id = _state.takeID!(true, false)();
                    immutable c = _state.input.front;
                    if(c == '[')
                    {
                        _state.type = EntityType.docTypeStart;
                        _state.grammarPos = GrammarPos.intSubset;
                    }
                    else if(c == '>')
                    {
                        _state.type = EntityType.docTypeEmpty;
                        _state.grammarPos = GrammarPos.prologMisc2;
                    }
                    else
                        throw new XMLParsingException("Expected [ or >", _state.pos);
                    popFrontAndIncCol(_state);
                    break;
                }
                case '[':
                {
                    popFrontAndIncCol(_state);
                    _state.id.nullify(); // In case it has an ID from a previous entity.
                    _state.type = EntityType.docTypeStart;
                    _state.grammarPos = GrammarPos.intSubset;
                    break;
                }
                case '>':
                {
                    popFrontAndIncCol(_state);
                    _state.id.nullify(); // In case it has an ID from a previous entity.
                    _state.type = EntityType.docTypeEmpty;
                    _state.grammarPos = GrammarPos.prologMisc2;
                    break;
                }
                default: throw new XMLParsingException("Expected SYSTIME, PUBLIC, [, or >", _state.pos);
            }
        }
    }

    static if(compileInTests) unittest
    {
        import std.algorithm.comparison : equal;
        import std.exception : assertThrown;

        static void test(alias func, EntityType entityType)(string text, string name,
                                                            int row, int col, size_t line = __LINE__)
        {
            auto xml = func(text);

            static foreach(i, config; testConfigs)
            {{
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto cursor = parseXML!config(xml.save);
                enforceTest(cursor.next() == entityType, "unittest failure 1", line);
                enforceTest(equal(cursor.name, name), "unittest failure 2", line);
                enforceTest(cursor._state.pos == pos, "unittest failure 3", line);
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
            test!(func, EntityType.docTypeEmpty)("<!DOCTYPE name>", "name", 1, 16);
            test!(func, EntityType.docTypeEmpty)("<!DOCTYPE \n\n\n name>", "name", 4, 7);
            test!(func, EntityType.docTypeEmpty)("<!DOCTYPE name \n\n\n >", "name", 4, 3);

            test!(func, EntityType.docTypeStart)("<!DOCTYPE name []>", "name", 1, 17);
            test!(func, EntityType.docTypeStart)("<!DOCTYPE \n\n\n name []>", "name", 4, 8);
            test!(func, EntityType.docTypeStart)("<!DOCTYPE name \n\n\n []>", "name", 4, 3);

            testFail!func("<!DOCTYP name>");
            testFail!func("<!DOCTYPEname>");
            testFail!func("<!DOCTYPE >");
            testFail!func("<!DOCTYPE>");
        }
    }


    // doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
    // DeclSep     ::= PEReference | S
    // intSubset   ::= (markupdecl | DeclSep)*
    // markupdecl  ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
    // DeclSep     ::= PEReference | S
    // Parse doctypedecl at GrammarPos.intSubset.
    // The next thing to be parsed is always either a PEReference, one of the
    // items in markupdecl (all of which start with '<'), one of those two
    // things preceded by whitespace, or it's the ']' following the intSubset.
    void _parseAtIntSubset()
    {
        stripWS(_state);
        checkNotEmpty(_state);
        switch(_state.input.front)
        {
            case '<':
            {
                popFrontAndIncCol(_state);
                checkNotEmpty(_state);
                break;
            }
            case ']':
            {
                popFrontAndIncCol(_state);
                stripWS(_state);
                if(_state.input.front != '>')
                    throw new XMLParsingException("Expected >", _state.pos);
                popFrontAndIncCol(_state);
                _state.type = EntityType.docTypeEnd;
                _state.grammarPos = GrammarPos.prologMisc2;
                return;
            }
            // PEReference ::= '%' Name ';'
            case '%':
            {
                popFrontAndIncCol(_state);
                checkNotEmpty(_state);
                _state.type = EntityType.peReference;
                _state.savedText.input = _state.takeUntilAndDrop!";"();
                checkNotEmpty(_state);
                return;
            }
            default: throw new XMLParsingException("Expected <, %, ]", _state.pos);
        }
        switch(_state.input.front)
        {
            case '!':
            {
                popFrontAndIncCol(_state);
                checkNotEmpty(_state);
                break;
            }
            // PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
            case '?':
            {
                _parsePI();
                return;
            }
            default: throw new XMLParsingException("Expected ! or ?", _state.pos);
        }

        void stripTagName(string tagName, SourcePos posBefore)
        {
            popFrontAndIncCol(_state);
            if(!_state.stripStartsWith(tagName[1 .. $]))
                throw new XMLParsingException("Unknown tag type", posBefore);
            if(!stripWS(_state))
                throw new XMLParsingException("There must be whitespace after " ~ tagName, _state.pos);
        }

        switch(_state.input.front)
        {
            // Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
            case '-':
            {
                popFrontAndIncCol(_state);
                checkNotEmpty(_state);
                if(_state.input.front != '-')
                    throw new XMLParsingException("Invalid start of comment. Missing a '-'.", _state.pos);
                popFrontAndIncCol(_state);
                _parseComment();
                return;
            }
            // elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
            // EntityDecl  ::= GEDecl | PEDecl
            // GEDecl      ::= '<!ENTITY' S Name S EntityDef S? '>'
            // PEDecl      ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
            case 'E':
            {
                immutable posBefore = _state.pos;
                popFrontAndIncCol(_state);
                if(_state.stripStartsWith("LEMENT"))
                {
                    if(!stripWS(_state))
                        throw new XMLParsingException("There must be whitespace after ELEMENT", _state.pos);
                    _parseElementDecl();
                }
                else
                {
                    stripTagName("NTITY", posBefore);
                    _parseEntityDecl();
                }
                return;
            }
            // AttlistDecl    ::= '<!ATTLIST' S Name AttDef* S? '>'
            case 'A':
            {
                stripTagName("ATTLIST", _state.pos);
                _parseAttlistDecl();
                return;
            }
            // NotationDecl ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
            case 'N':
            {
                stripTagName("NOTATION", _state.pos);
                _parseNotationDecl();
                return;
            }
            default: throw new XMLParsingException("Unknown tag type", _state.pos);
        }
    }


    // elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
    // contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
    // Mixed       ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*' | '(' S? '#PCDATA' S? ')'
    // children    ::= (choice | seq) ('?' | '*' | '+')?
    // cp          ::= (Name | choice | seq) ('?' | '*' | '+')?
    // choice      ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
    // seq         ::= '(' S? cp ( S? ',' S? cp )* S? ')'
    // <!ELEMENT and any whitespace after it should have already been removed
    // from the input.
    void _parseElementDecl()
    {
        checkNotEmpty(_state);
        _state.type = EntityType.elementDecl;
        _state.name = _state.takeName!true();
        if(!stripWS(_state))
            throw new XMLParsingException("There must be whitespace after the ELEMENT tag's name", _state.pos);

        if(_state.stripStartsWith("EMPTY"))
            _state.contentSpec.type = ContentSpecType.empty;
        else if(_state.stripStartsWith("ANY"))
            _state.contentSpec.type = ContentSpecType.any;
        else
        {
            import std.algorithm.searching : startsWith;

            checkNotEmpty(_state);
            if(_state.input.front != '(')
                throw new XMLParsingException("Expected EMPTY, ANY, or (", _state.pos);
            _state.contentSpec._savedText.pos = _state.pos;
            _state.contentSpec._savedText.input = _state.takeParenedText();

            auto temp = _state.contentSpec._savedText.save;
            auto tempState = &temp;
            popFrontAndIncCol(tempState);
            stripWS(tempState);
            if(tempState.stripStartsWith("#PCDATA"))
            {
                _state.contentSpec.type = _state.stripStartsWith("*")
                                          ? ContentSpecType.mixedStar : ContentSpecType.mixedSingle;
            }
            else
                _state.contentSpec.type = ContentSpecType.children;
        }

        stripWS(_state);
        if(_state.input.front != '>')
            throw new XMLParsingException("Expected >", _state.pos);
        popFrontAndIncCol(_state);
    }

    static if(compileInTests) unittest
    {
        import std.algorithm.comparison : equal;
        import std.exception : assertThrown;
        import std.format : format;

        static void test(alias func)(string elementDecl, string name, ContentSpecType type,
                                     int row, int col, size_t line = __LINE__)
        {
            auto xml = func(format("<!DOCTYPE surgeon [\n%s\n]></root>", elementDecl));

            static foreach(i, config; testConfigs)
            {{
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto cursor = parseXML!config(xml.save);
                enforceTest(cursor.next() == EntityType.docTypeStart, "unittest failure 1", line);
                enforceTest(cursor.next() == EntityType.elementDecl, "unittest failure 2", line);
                enforceTest(equal(cursor.name, name), "unittest failure 3", line);
                enforceTest(cursor.contentSpec.type == type, "unittest failure 4", line);
                enforceTest(cursor._state.pos == pos, "unittest failure 5", line);
            }}
        }

        static void testFail(alias func)(string elementDecl, size_t line = __LINE__)
        {
            auto xml = func(format("<!DOCTYPE surgeon [\n%s\n]></root>", elementDecl));

            static foreach(i, config; testConfigs)
            {{
                auto cursor = parseXML!config(xml.save);
                enforceTest(cursor.next() == EntityType.docTypeStart, "unittest failure 1", line);
                assertThrown!XMLParsingException(cursor.next(), "unittest failure 2", __FILE__, line);
            }}
        }

        static foreach(i, func; testRangeFuncs)
        {
            test!func("<!ELEMENT name EMPTY>", "name", ContentSpecType.empty, 2, 22);
            test!func("<!ELEMENT foo   EMPTY   >", "foo", ContentSpecType.empty, 2, 26);
            test!func("<!ELEMENT\n\n\nname EMPTY>", "name", ContentSpecType.empty, 5, 12);
            test!func("<!ELEMENT bar \n\n EMPTY >", "bar", ContentSpecType.empty, 4, 9);
            test!func("<!ELEMENT bar  ANY \n >", "bar", ContentSpecType.any, 3, 3);
            test!func("<!ELEMENT whatever (#PCDATA)>", "whatever", ContentSpecType.mixedSingle, 2, 30);
            test!func("<!ELEMENT whatever (\n#PCDATA )>", "whatever", ContentSpecType.mixedSingle, 3, 11);
            test!func("<!ELEMENT whatever (#PCDATA)*>", "whatever", ContentSpecType.mixedStar, 2, 31);
            test!func("<!ELEMENT whatever (\n#PCDATA )*>", "whatever", ContentSpecType.mixedStar, 3, 12);
            test!func("<!ELEMENT whatever (Foo)>", "whatever", ContentSpecType.children, 2, 26);

            testFail!func("<!ELEMEN name>");
            testFail!func("<!ELEMENTname>");
            testFail!func("<!ELEMENTTname>");
            testFail!func("<!ELEMENTn ame>");
            testFail!func("<!ELEMENT name>");
            testFail!func("<!ELEMENT >");
            testFail!func("<!ELEMENT>");

            testFail!func("<!ELEMENT name EMPT>");
            testFail!func("<!ELEMENT name AN>");
            testFail!func("<!ELEMENT name EMPTY foo>");
            testFail!func("<!ELEMENT name ANY foo>");
            testFail!func("<!ELEMENT name (>");
            testFail!func("<!ELEMENT name (>)>");
            testFail!func("<!ELEMENT whatever(#PCDATA)>");
            testFail!func("<!ELEMENT whatever(Foo)*>");
        }
    }


    // AttlistDecl    ::= '<!ATTLIST' S Name AttDef* S? '>'
    // AttDef         ::= S Name S AttType S DefaultDecl
    // <!ATTLIST and any whitespace after it should have already been removed
    // from the input.
    void _parseAttlistDecl()
    {
        _state.type = EntityType.attlistDeclStart;
        checkNotEmpty(_state);
        _state.name = _state.takeName!(true, '>')();
        _state.grammarPos = GrammarPos.intSubsetAttDef;
    }

    static if(compileInTests) unittest
    {
        import std.algorithm.comparison : equal;
        import std.exception : assertNotThrown, assertThrown;
        import std.format : format;
        import std.stdio : writefln;

        static struct TestAttDefInfo
        {
            string name;
            AttType attType;
            string[] notationValues;
            string[] enumValues;
            DefaultDecl defaultDecl;
            string attValue;
        }

        static void test(alias func)(string attlist, string name, TestAttDefInfo[] attDefInfos,
                                     int row, int col, size_t line = __LINE__)
        {
            auto xml = func(format("<!DOCTYPE surgeon [\n%s\n]></root>", attlist));

            static foreach(i, config; testConfigs)
            {{
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto cursor = parseXML!config(xml.save);
                enforceTest(cursor.next() == EntityType.docTypeStart, "unittest failure 1", line);
                enforceTest(cursor.next() == EntityType.attlistDeclStart, "unittest failure 2", line);
                enforceTest(equal(cursor.name, name), "unittest failure 3", line);
                auto expectedADIs = attDefInfos;
                for(int i = 0; true; ++i)
                {
                    assertNotThrown!XMLParsingException(cursor.next(), "unittest failure 4", __FILE__, line);
                    if(cursor.type != EntityType.attDef)
                        break;
                    scope(failure) writefln("AttDef #%s", i);

                    enforceTest(!expectedADIs.empty, "unittest failure 5", line);
                    auto expected = expectedADIs.front;
                    expectedADIs.popFront();

                    enforceTest(equal(cursor.name, expected.name), "unittest failure 6", line);
                    enforceTest(cursor.attDefInfo.attType == expected.attType, "unittest failure 7", line);
                    if(expected.attType == AttType.notationType)
                    {
                        enforceTest(equal!((a, b) => equal(a, b))(cursor.attDefInfo.notationValues,
                                                                  expected.notationValues),
                                    "unittest failure 8", line);
                    }
                    else if(expected.attType == AttType.enumeration)
                    {
                        enforceTest(equal!((a, b) => equal(a, b))(cursor.attDefInfo.enumValues,
                                                                  expected.enumValues),
                                    "unittest failure 9", line);
                    }
                    enforceTest(cursor.attDefInfo.defaultDecl == expected.defaultDecl, "unittest failure 10", line);
                    if(expected.defaultDecl == DefaultDecl.fixed || expected.defaultDecl == DefaultDecl.unfixed)
                        enforceTest(equal(cursor.attDefInfo.attValue, expected.attValue), "unittest failure 11", line);
                }
                enforceTest(expectedADIs.empty, "unittest failure 12", line);
                enforceTest(cursor.type == EntityType.attlistDeclEnd, "unittest failure 13", line);
                enforceTest(cursor._state.pos == pos, "unittest failure 14", line);
            }}
        }

        static void testFail(alias func)(string attlist, size_t line = __LINE__)
        {
            auto xml = func(format("<!DOCTYPE surgeon [\n%s\n]></root>", attlist));

            static foreach(i, config; testConfigs)
            {{
                auto cursor = parseXML!config(xml.save);
                enforceTest(cursor.next() == EntityType.docTypeStart, "unittest failure 1", line);
                assertThrown!XMLParsingException({while(cursor.next() != EntityType.docTypeEnd){}}(),
                                                 "unittest failure 2", __FILE__, line);
            }}
        }

        static void testBadValues(alias func)(string attlist, size_t line = __LINE__)
        {
            import std.range : walkLength;
            auto xml = func(format("<!DOCTYPE surgeon [\n%s\n]></root>", attlist));

            static foreach(i, config; testConfigs)
            {{
                auto cursor = parseXML!config(xml.save);
                enforceTest(cursor.next() == EntityType.docTypeStart, "unittest failure 1", line);
                enforceTest(cursor.next() == EntityType.attlistDeclStart, "unittest failure 2", line);
                enforceTest(cursor.next() == EntityType.attDef, "unittest failure 3", line);
                if(cursor.attDefInfo.attType == AttType.notationType)
                {
                    assertThrown!XMLParsingException(walkLength(cursor.attDefInfo.notationValues),
                                                     "unittest failure 4", __FILE__, line);
                }
                else if(cursor.attDefInfo.attType == AttType.enumeration)
                {
                    assertThrown!XMLParsingException(walkLength(cursor.attDefInfo.enumValues),
                                                     "unittest failure 4", __FILE__, line);
                }
            }}
        }

        static auto token(Args...)(string name, AttType attType, DefaultDecl defaultDecl, Args args)
        {
            TestAttDefInfo retval;
            retval.name = name;
            retval.attType = attType;
            retval.defaultDecl = defaultDecl;
            static if(args.length != 0)
                retval.attValue = args[0];
            return retval;
        }

        static auto notation(Args...)(string name, string[] notationValues, DefaultDecl defaultDecl, Args args)
        {
            auto retval = token(name, AttType.notationType, defaultDecl, args);
            retval.notationValues = notationValues;
            return retval;
        }

        static auto enum_(Args...)(string name, string[] enumValues, DefaultDecl defaultDecl, Args args)
        {
            auto retval = token(name, AttType.enumeration, defaultDecl, args);
            retval.enumValues = enumValues;
            return retval;
        }

        with(AttType) with(DefaultDecl) static foreach(i, func; testRangeFuncs)
        {
            test!func("<!ATTLIST name>", "name", null, 2, 16);
            test!func("<!ATTLIST foo     >", "foo", null, 2, 20);
            test!func("<!ATTLIST\n\n\nname>", "name", null, 5, 6);
            test!func("<!ATTLIST bar \n\n  >", "bar", null, 4, 4);

            static foreach(name; ["cdata", "id", "idref", "idrefs", "entity", "entities", "nmtoken", "nmtokens"])
            {{
                import std.uni : toUpper;
                enum upper = name.toUpper();
                test!func(format("<!ATTLIST whatever cname %s #REQUIRED>", upper), "whatever",
                         [token("cname", mixin(name), required)], 2, 37 + name.length);
                test!func(format("<!ATTLIST whatever cname %s #IMPLIED>", upper), "whatever",
                         [token("cname", mixin(name), implied)], 2, 36 + name.length);
                test!func(format("<!ATTLIST everwhat noname %s #FIXED 'leak'>", upper), "everwhat",
                         [token("noname", mixin(name), fixed, "leak")], 2, 42 + name.length);
                test!func(format(`<!ATTLIST everwhat xyzzy %s "faucet">`, upper), "everwhat",
                         [token("xyzzy", mixin(name), unfixed, "faucet")], 2, 36 + name.length);

                test!func(format("<!ATTLIST whatever blah %s #REQUIRED foo CDATA #IMPLIED>", upper), "whatever",
                         [token("blah", mixin(name), required),
                          token("foo", cdata, implied)], 2, 55 + name.length);
                test!func(format("<!ATTLIST whatever bleh %s #IMPLIED bar ID #REQUIRED>", upper), "whatever",
                         [token("bleh", mixin(name), implied),
                          token("bar", id, required)], 2, 52 + name.length);
                test!func(format("<!ATTLIST everwhat bluh %s #FIXED 'leak' a IDREF #IMPLIED>", upper), "everwhat",
                         [token("bluh", mixin(name), fixed, "leak"),
                          token("a", idref, implied)], 2, 57 + name.length);
                test!func(format(`<!ATTLIST everwhat blug %s "faucet" arg %s 'pipe'>`, upper, upper), "everwhat",
                         [token("blug", mixin(name), unfixed, "faucet"),
                          token("arg", mixin(name), unfixed, "pipe")], 2, 47 + name.length * 2);
            }}

            test!func(format("<!ATTLIST\nlistname\n\ndefname1\n\n\nID\n#REQUIRED\ndefname2\n\n\nCDATA\n#IMPLIED\n >"),
                      "listname", [token("defname1", id, required),
                                   token("defname2", cdata, implied)], 15, 3);

            test!func(format("<!ATTLIST a note NOTATION (foo) #REQUIRED>"),
                      "a", [notation("note", ["foo"], required)], 2, 43);
            test!func(format("<!ATTLIST a note NOTATION (foo) #IMPLIED>"),
                      "a", [notation("note", ["foo"], implied)], 2, 42);
            test!func(format("<!ATTLIST a note NOTATION (foo) #FIXED 'bug'>"),
                      "a", [notation("note", ["foo"], fixed, "bug")], 2, 46);
            test!func(format("<!ATTLIST a note NOTATION (foo) 'bug'>"),
                      "a", [notation("note", ["foo"], unfixed, "bug")], 2, 39);

            test!func(format("<!ATTLIST a note NOTATION (\n  foo \n ) #REQUIRED>"),
                      "a", [notation("note", ["foo"], required)], 4, 14);

            test!func(format("<!ATTLIST a note NOTATION (foo|bar) #REQUIRED>"),
                      "a", [notation("note", ["foo", "bar"], required)], 2, 47);
            test!func(format("<!ATTLIST a note NOTATION (foo|bar|baz) #REQUIRED>"),
                      "a", [notation("note", ["foo", "bar", "baz"], required)], 2, 51);
            test!func(format("<!ATTLIST a note NOTATION (a|bc|d) #REQUIRED>"),
                      "a", [notation("note", ["a", "bc", "d"], required)], 2, 46);
            test!func(format("<!ATTLIST a note NOTATION ( foo\n|\nbar   |   baz \n ) #REQUIRED>"),
                      "a", [notation("note", ["foo", "bar", "baz"], required)], 5, 14);

            test!func(format("<!ATTLIST a note (foo) #REQUIRED>"),
                      "a", [enum_("note", ["foo"], required)], 2, 34);
            test!func(format("<!ATTLIST a note (foo) #IMPLIED>"),
                      "a", [enum_("note", ["foo"], implied)], 2, 33);
            test!func(format("<!ATTLIST a note (foo) #FIXED 'bug'>"),
                      "a", [enum_("note", ["foo"], fixed, "bug")], 2, 37);
            test!func(format("<!ATTLIST a note (foo) 'bug'>"),
                      "a", [enum_("note", ["foo"], unfixed, "bug")], 2, 30);

            test!func(format("<!ATTLIST a note (\n  foo \n ) #REQUIRED>"),
                      "a", [enum_("note", ["foo"], required)], 4, 14);

            test!func(format("<!ATTLIST a note (foo|bar) #REQUIRED>"),
                      "a", [enum_("note", ["foo", "bar"], required)], 2, 38);
            test!func(format("<!ATTLIST a note (foo|bar|baz) #REQUIRED>"),
                      "a", [enum_("note", ["foo", "bar", "baz"], required)], 2, 42);
            test!func(format("<!ATTLIST a note (a|bc|d) #REQUIRED>"),
                      "a", [enum_("note", ["a", "bc", "d"], required)], 2, 37);
            test!func(format("<!ATTLIST a note ( foo\n|\nbar   |   baz \n ) #REQUIRED>"),
                      "a", [enum_("note", ["foo", "bar", "baz"], required)], 5, 14);

            testFail!func("<!ATTLIS name>");
            testFail!func("<!ATTLISTname>");
            testFail!func("<!ATTLISTTname>");
            testFail!func("<!ATTLISTn ame>");
            testFail!func("<!ATTLISt name>");
            testFail!func("<!ATTLIST >");
            testFail!func("<!ATTLIST>");
            testFail!func("<!ATTLIST foo #REQUIRED>");
            testFail!func("<!ATTLIST foo bar #REQUIRED>");
            testFail!func("<!ATTLIST foo CDATA #REQUIRED>");
            testFail!func("<!ATTLIST foo cname CDATA #REQUIRED");

            static foreach(name; ["CDATA", "ID", "IDREF", "IDREFS", "ENTITY", "ENTITIES", "NMTOKEN", "NMTOKENS"])
            {{
                testFail!func(format!"<!ATTLIST foo bar%s #REQUIRED>"(name));
                testFail!func(format!"<!ATTLIST foo bar %s#REQUIRED>"(name));
                testFail!func(format!"<!ATTLIST foo bar %s#IMPLIED>"(name));
                testFail!func(format!"<!ATTLIST foo bar %s#FIXED 'bar'>"(name));
                testFail!func(format!"<!ATTLIST foo bar %s #FIXED'bar'>"(name));
                testFail!func(format!"<!ATTLIST foo bar %s'bar'>"(name));
            }}

            testFail!func("<!ATTLIST foo bar NOTATION(foo) #REQUIRED>");
            testFail!func("<!ATTLIST foo bar NOTATION (foo)#REQUIRED>");
            testFail!func("<!ATTLIST foo bar NOTATION (foo)#IMPLIED>");
            testFail!func("<!ATTLIST foo bar NOTATION (foo)#FIXED 'bar'>");
            testFail!func("<!ATTLIST foo bar NOTATION (foo) #FIXED'bar'>");
            testFail!func("<!ATTLIST foo bar NOTATION bar) #REQUIRED>");
            testFail!func("<!ATTLIST foo bar NOTATION (bar #REQUIRED>");
            testFail!func("<!ATTLIST foo bar NOTATION ((bar)) #REQUIRED>");

            testFail!func("<!ATTLIST foo bar(foo) #REQUIRED>");
            testFail!func("<!ATTLIST foo bar (foo)#REQUIRED>");
            testFail!func("<!ATTLIST foo bar (foo)#IMPLIED>");
            testFail!func("<!ATTLIST foo bar (foo)#FIXED 'bar'>");
            testFail!func("<!ATTLIST foo bar (foo) #FIXED'bar'>");
            testFail!func("<!ATTLIST foo bar bar) #REQUIRED>");
            testFail!func("<!ATTLIST foo bar (bar #REQUIRED>");
            testFail!func("<!ATTLIST foo bar ((bar)) #REQUIRED>");

            testBadValues!func("<!ATTLIST foo bar NOTATION () #REQUIRED>");
            testBadValues!func("<!ATTLIST foo bar NOTATION (foo|) #REQUIRED>");
            testBadValues!func("<!ATTLIST foo bar NOTATION (foo|bar|) #REQUIRED>");
            testBadValues!func("<!ATTLIST foo bar NOTATION (|bar|) #REQUIRED>");
            testBadValues!func("<!ATTLIST foo bar NOTATION (|bar) #REQUIRED>");
            testBadValues!func("<!ATTLIST foo bar NOTATION ((bar) #REQUIRED>");

            testBadValues!func("<!ATTLIST foo bar () #REQUIRED>");
            testBadValues!func("<!ATTLIST foo bar (foo|) #REQUIRED>");
            testBadValues!func("<!ATTLIST foo bar (foo|bar|) #REQUIRED>");
            testBadValues!func("<!ATTLIST foo bar (|bar|) #REQUIRED>");
            testBadValues!func("<!ATTLIST foo bar (|bar) #REQUIRED>");
            testBadValues!func("<!ATTLIST foo bar ((bar) #REQUIRED>");
        }
    }


    // AttlistDecl    ::= '<!ATTLIST' S Name AttDef* S? '>'
    // AttDef         ::= S Name S AttType S DefaultDecl
    void _parseAtAttDef()
    {
        immutable wasSpace = stripWS(_state);
        checkNotEmpty(_state);
        if(_state.input.front == '>')
        {
            popFrontAndIncCol(_state);
            _state.type = EntityType.attlistDeclEnd;
            _state.grammarPos = GrammarPos.intSubset;
            return;
        }
        if(!wasSpace)
            throw new XMLParsingException("Must have whitespace before AttDef's name", _state.pos);
        _state.type = EntityType.attDef;
        _state.name = _state.takeName!true();
        _parseAttType();
        if(!stripWS(_state))
            throw new XMLParsingException("Whitespace missing", _state.pos);
        checkNotEmpty(_state);
        _parseDefaultDecl();
    }


    // AttType        ::= StringType | TokenizedType | EnumeratedType
    // StringType     ::= 'CDATA'
    // TokenizedType  ::= 'ID' | 'IDREF' | 'IDREFS' | 'ENTITY' | 'ENTITIES' | 'NMTOKEN' | 'NMTOKENS'
    // EnumeratedType ::= NotationType | Enumeration
    // NotationType   ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
    // Enumeration    ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
    // Nmtoken        ::= (NameChar)+
    void _parseAttType()
    {
        if(!stripWS(_state))
            throw new XMLParsingException("Must have whitespace after name", _state.pos);
        checkNotEmpty(_state);
        switch(_state.input.front)
        {
            case 'C':
            {
                if(!_state.stripStartsWith("CDATA"))
                    throw new XMLParsingException("Expected CDATA", _state.pos);
                _state.attDefInfo.attType = AttType.cdata;
                break;
            }
            case 'I':
            {
                if(!_state.stripStartsWith("ID"))
                    throw new XMLParsingException("Expected ID, IDREF, or IDREFS", _state.pos);
                if(_state.stripStartsWith("REF"))
                {
                    checkNotEmpty(_state);
                    if(_state.input.front == 'S')
                    {
                        popFrontAndIncCol(_state);
                        _state.attDefInfo.attType = AttType.idrefs;
                    }
                    else
                        _state.attDefInfo.attType = AttType.idref;
                }
                else
                    _state.attDefInfo.attType = AttType.id;
                break;
            }
            case 'E':
            {
                if(!_state.stripStartsWith("ENTIT"))
                    throw new XMLParsingException("Expected ENTITY or ENTITIES", _state.pos);
                checkNotEmpty(_state);
                if(_state.input.front == 'Y')
                {
                    popFrontAndIncCol(_state);
                    _state.attDefInfo.attType = AttType.entity;
                }
                else if(_state.stripStartsWith("IES"))
                    _state.attDefInfo.attType = AttType.entities;
                else
                    throw new XMLParsingException("Expected ENTITY or ENTITIES", _state.pos);
                break;
            }
            case 'N':
            {
                if(_state.stripStartsWith("NMTOKEN"))
                {
                    checkNotEmpty(_state);
                    if(_state.input.front == 'S')
                    {
                        popFrontAndIncCol(_state);
                        _state.attDefInfo.attType = AttType.nmtokens;
                    }
                    else
                        _state.attDefInfo.attType = AttType.nmtoken;
                }
                else if(_state.stripStartsWith("NOTATION"))
                {
                    if(!stripWS(_state))
                        throw new XMLParsingException("Whitespace missing after NOTATION", _state.pos);
                    checkNotEmpty(_state);
                    _state.attDefInfo.attType = AttType.notationType;
                    if(_state.input.front == '(')
                    {
                        _state.attDefInfo._savedText.pos = _state.pos;
                        _state.attDefInfo._savedText.input = _state.takeUntilAndKeep!")"();
                    }
                    else
                        throw new XMLParsingException("Expected (", _state.pos);
                }
                else
                    throw new XMLParsingException("Expection NMTOKEN, NMTOKENS, or NOTATION", _state.pos);
                break;
            }
            case '(':
            {
                _state.attDefInfo.attType = AttType.enumeration;
                _state.attDefInfo._savedText.pos = _state.pos;
                _state.attDefInfo._savedText.input = _state.takeUntilAndKeep!")"();
                break;
            }
            default: throw new XMLParsingException("Invalid AttType", _state.pos);
        }
    }


    // DefaultDecl    ::= '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
    // AttValue       ::= '"' ([^<&"] | Reference)* '"' |  "'" ([^<&'] | Reference)* "'"
    // Reference      ::= EntityRef | CharRef
    // EntityRef      ::= '&' Name ';'
    // CharRef        ::= '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'
    void _parseDefaultDecl()
    {
        if(_state.stripStartsWith("#REQUIRED"))
            _state.attDefInfo.defaultDecl = DefaultDecl.required;
        else if(_state.stripStartsWith("#IMPLIED"))
            _state.attDefInfo.defaultDecl = DefaultDecl.implied;
        else
        {
            if(_state.stripStartsWith("#FIXED"))
            {
                if(!stripWS(_state))
                    throw new XMLParsingException("Whitespace missing after #FIXED", _state.pos);
                _state.attDefInfo.defaultDecl = DefaultDecl.fixed;
            }
            else
                _state.attDefInfo.defaultDecl = DefaultDecl.unfixed;
            _state.attDefInfo._attValue = stripBCU!R(_state.takeEnquotedText());
        }
    }


    // EntityDecl    ::= GEDecl | PEDecl
    // GEDecl        ::= '<!ENTITY' S Name S EntityDef S? '>'
    // PEDecl        ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
    // EntityDef     ::= EntityValue | (ExternalID NDataDecl?)
    // PEDef         ::= EntityValue | ExternalID
    // EntityValue   ::= '"' ([^%&"] | PEReference | Reference)* '"' |  "'" ([^%&'] | PEReference | Reference)* "'"
    // PEReference   ::= '%' Name ';'
    // Reference     ::= EntityRef | CharRef
    // EntityRef     ::= '&' Name ';'
    // CharRef       ::= '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'
    // ExternalID    ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
    // NDataDecl     ::= S 'NDATA' S Name
    // SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
    // PubidLiteral  ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
    // PubidChar     ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
    // <!ENTITY and any whitespace after it should have already been removed
    // from the input.
    void _parseEntityDecl()
    {
        _state.type = EntityType.entityDecl;
        _state.entityDef = typeof(_state.entityDef).init;

        checkNotEmpty(_state);
        if(_state.input.front == '%')
        {
            _state.entityDef.parsedEntity = true;
            popFrontAndIncCol(_state);
            if(!stripWS(_state))
                throw new XMLParsingException("Whitespace missing after %", _state.pos);
            checkNotEmpty(_state);
        }

        _state.name = _state.takeName!true();
        if(!stripWS(_state))
            throw new XMLParsingException("Missing whitespace", _state.pos);

        checkNotEmpty(_state);
        switch(_state.input.front)
        {
            case 'P':
            case 'S':
            {
                auto temp = _state.takeID!(true, true)();
                _state.entityDef.externalID = temp[0];
                switch(_state.input.front)
                {
                    case '>':
                    {
                        if(!_state.entityDef.parsedEntity)
                            throw new XMLParsingException("Expected NDATA", _state.pos);
                        popFrontAndIncCol(_state);
                        return;
                    }
                    case 'N':
                    {
                        if(_state.entityDef.parsedEntity)
                            throw new XMLParsingException("Expected >", _state.pos);
                        popFrontAndIncCol(_state);
                        if(!_state.stripStartsWith("DATA"))
                            throw new XMLParsingException("Expected NDATA", _state.pos);
                        if(!stripWS(_state))
                            throw new XMLParsingException("There must be whitespace after NDATA", _state.pos);
                        _state.entityDef.ndataName = stripBCU!R(_state.takeName!(true, '>')());
                        break;

                    }
                    default:
                    {
                        if(_state.entityDef.parsedEntity)
                            throw new XMLParsingException("Expected >", _state.pos);
                        else
                            throw new XMLParsingException("Expected NDATA", _state.pos);
                    }
                }
                break;
            }
            case '"':
            {
                popFrontAndIncCol(_state);
                _state.entityDef.value = stripBCU!R(_state.takeUntilAndDrop!`"`());
                break;
            }
            case '\'':
            {
                popFrontAndIncCol(_state);
                _state.entityDef.value = stripBCU!R(_state.takeUntilAndDrop!"'"());
                break;
            }
            default: throw new XMLParsingException(`Expected PUBLIC, SYSTEM, ", or '`, _state.pos);
        }

        stripWS(_state);
        if(_state.input.front != '>')
            throw new XMLParsingException("Expected >", _state.pos);
        popFrontAndIncCol(_state);
    }


    // NotationDecl ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
    // PublicID    ::= 'PUBLIC' S PubidLiteral
    // ExternalID    ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
    // NDataDecl     ::= S 'NDATA' S Name
    // SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
    // PubidLiteral  ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
    // PubidChar     ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
    // <!NOTATION and any whitespace after it should have already been removed
    // from the input.
    void _parseNotationDecl()
    {
        checkNotEmpty(_state);
        _state.type = EntityType.notationDecl;
        _state.name = _state.takeName!true();
        if(!stripWS(_state))
            throw new XMLParsingException("There must be whitespace after the NOTATION tag's name", _state.pos);
        _state.id = _state.takeID!(false, false)();
        if(_state.input.front != '>')
            throw new XMLParsingException("Expected >", _state.pos);
        popFrontAndIncCol(_state);
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

        Nullable!ID id;
        EntityDef entityDef;
        AttDefInfo attDefInfo;
        ContentSpec contentSpec;

        this(R xmlText)
        {
            input = byCodeUnit(xmlText);

            // None of these initializations should be required. https://issues.dlang.org/show_bug.cgi?id=13945
            savedText = typeof(savedText).init;
            name = typeof(name).init;
            id = typeof(id).init;
            entityDef = typeof(entityDef).init;
            attDefInfo = typeof(attDefInfo).init;
            contentSpec = typeof(contentSpec).init;
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
               "<foo attr='42'>\n" ~
               "    <bar/>\n" ~
               "    <!-- no comment -->\n" ~
               "    <baz hello='world'>\n" ~
               "    nothing to say.\n" ~
               "    nothing at all...\n" ~
               "    </baz>\n" ~
               "</foo>";

    {
        auto cursor = parseXML(xml);
        assert(cursor.next() == EntityType.xmlDecl);

        auto xmlDecl = cursor.xmlDecl;
        assert(xmlDecl.xmlVersion == "1.0");
        assert(xmlDecl.encoding.isNull);
        assert(xmlDecl.standalone.isNull);

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
        assert(cursor.next() == EntityType.documentEnd);
    }
    {
        auto cursor = parseXML!simpleXML(xml);
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
    Represents the $(LINK2 http://www.w3.org/TR/REC-xml/#NT-AttType, $(I AttType))
    portion of an  $(LINK2 http://www.w3.org/TR/REC-xml/#NT-AttDef, $(I AttDef)).

    See_Also: $(LREF AttDefInfo)$(BR)
              $(LINK http://www.w3.org/TR/REC-xml/#attdecls)
  +/
enum AttType
{
    /++
        The $(D CDATA) from
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-StringType, $(I StringType)).
      +/
    cdata,
    /++
        The $(D ID) from
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-TokenizedType, $(I TokenizedType)).
      +/
    id,
    /++
        The $(D IDREF) from
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-TokenizedType, $(I TokenizedType)).
      +/
    idref,
    /++
        The $(D IDREFS) from
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-TokenizedType, $(I TokenizedType)).
      +/
    idrefs,
    /++
        The $(D ENTITY) from
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-TokenizedType, $(I TokenizedType)).
      +/
    entity,
    /++
        The $(D ENTITIES) from
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-TokenizedType, $(I TokenizedType)).
      +/
    entities,
    /++
        The $(D NMTOKEN) from
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-TokenizedType, $(I TokenizedType)).
      +/
    nmtoken,
    /++
        The $(D NMTOKENS) from
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-TokenizedType, $(I TokenizedType)).
      +/
    nmtokens,
    /++
        The $(LINK2 http://www.w3.org/TR/REC-xml/#NT-NotationType, $(I NotationType))
        from $(LINK2 http://www.w3.org/TR/REC-xml/#NT-EnumeratedType, $(I EnumeratedType)).
      +/
    notationType,
    /++
        The $(LINK2 http://www.w3.org/TR/REC-xml/#NT-Enumeration, $(I Enumeration))
        from $(LINK2 http://www.w3.org/TR/REC-xml/#NT-EnumeratedType, $(I EnumeratedType)).
      +/
    enumeration
}


/++
    Represents the $(LINK2 http://www.w3.org/TR/REC-xml/#NT-DefaultDecl, $(I DefaultDecl))
    portion of an  $(LINK2 http://www.w3.org/TR/REC-xml/#NT-AttDef, $(I AttDef)).

    See_Also: $(LREF AttDefInfo)$(BR)
              $(LINK http://www.w3.org/TR/REC-xml/#attdecls)
  +/
enum DefaultDecl
{
    /// The $(I DefaultDecl) was $(D #REQUIRED).
    required,
    /// The $(I DefaultDecl) was $(D #IMPLIED).
    implied,
    /// The $(I DefaultDecl) was an $(I AttValue) prefixed with $(D #FIXED).
    fixed,
    /// The $(I DefaultDecl) was an $(I AttValue) without $(D #FIXED).
    unfixed
}


/++
    Represents the type of the
    $(LINK2 http://www.w3.org/TR/REC-xml/#NT-contentspec, $(I contentspec))
    portion of an
    $(LINK2 http://www.w3.org/TR/REC-xml/#NT-elemendecl, $(I elementDecl)).

    See_Also: $(LREF ContentSpec)$(BR)
              $(LINK http://www.w3.org/TR/REC-xml/#elemdecls)
  +/
enum ContentSpecType
{
    /// The $(I contentspec) was $(D EMPTY).
    empty,

    /// The $(I contentspec) was $(D ANY).
    any,

    /++
        The $(I contentspec) was a
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-Mixed, mixed-content declaration)
        that terminated with a $(D '*') and thus contains $(D #PCDATA) and zero
        or more $(I Name)s.
      +/
    mixedStar,

    /++
        The $(I contentspec) was a
        $(LINK2 http://www.w3.org/TR/REC-xml/#NT-Mixed, mixed-content declaration).
        that did $(I not) terminate with a $(D '*') and thus contains only a
        single $(D #PCDATA).
      +/
    mixedSingle,

    /++
        The $(I contentspec) was an
        $(LINK2 http://www.w3.org/TR/REC-xml/#sec-element-content, element content model).
      +/
    children
}


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
        case attlistDeclStart: return cursor.skipToEntityType(docTypeEnd);
        case attDef: return cursor.skipToEntityType(attlistDeclEnd);
        case attlistDeclEnd: goto case attlistDeclStart;
        case cdata:
        case comment:
        {
            immutable type = cursor.skipToEntityType(elementStart, elementEnd);
            if(type == documentEnd || type == elementEnd)
                return type;
            goto case elementStart;
        }
        case docTypeStart: return cursor.skipToEntityType(documentEnd);
        case docTypeEmpty: return documentEnd;
        case docTypeEnd: return documentEnd;
        case documentEnd: return cursor.next(); // will assert(0), but we don't have to duplicate the message this way
        case documentStart: goto case docTypeStart;
        case elementDecl: goto case attlistDeclStart;
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
        case entityDecl: goto case attlistDeclStart;
        case notationDecl: goto case attlistDeclStart;
        case peReference: goto case attlistDeclStart;
        case pi:
        {
            immutable type = cursor.skipToEntityType(docTypeStart, docTypeEnd, elementStart, elementEnd);
            if(type == docTypeStart)
                return documentEnd;
            if(type == documentEnd || type == docTypeEnd || type == elementEnd)
                return type;
            goto case elementStart;
        }
        case text: goto case comment;
        case xmlDecl: goto case docTypeStart;
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

    // attlistDeclStart
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    <!ATTLIST foo>\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.attlistDeclStart);
        enforceTest(cursor.skipToParentDepth() == EntityType.docTypeEnd);
    }
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    <!ATTLIST foo>\n" ~
                   "    <!-- comment -->\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.attlistDeclStart);
        enforceTest(cursor.skipToParentDepth() == EntityType.docTypeEnd);
    }
    // attDef
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    <!ATTLIST foo a CDATA #REQUIRED>\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.attlistDeclStart);
        enforceTest(cursor.next() == EntityType.attDef);
        enforceTest(cursor.skipToParentDepth() == EntityType.attlistDeclEnd);
    }
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    <!ATTLIST foo a CDATA #REQUIRED b ID #IMPLIED>\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.attlistDeclStart);
        enforceTest(cursor.next() == EntityType.attDef);
        enforceTest(cursor.skipToParentDepth() == EntityType.attlistDeclEnd);
    }
    // attlistDeclEnd
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    <!ATTLIST foo>\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.attlistDeclStart);
        enforceTest(cursor.next() == EntityType.attlistDeclEnd);
        enforceTest(cursor.skipToParentDepth() == EntityType.docTypeEnd);
    }
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    <!ATTLIST foo>\n" ~
                   "    <!-- comment -->\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.attlistDeclStart);
        enforceTest(cursor.next() == EntityType.attlistDeclEnd);
        enforceTest(cursor.skipToParentDepth() == EntityType.docTypeEnd);
    }
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
    // docTypeStart
    {
        auto xml = "<!DOCTYPE sturgeon []>" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
    }
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    <!ELEMENT foo EMPTY>\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
    }
    // docTypeEmpty
    {
        auto xml = "<!DOCTYPE sturgeon>" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeEmpty);
        enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
    }
    // docTypeEnd
    {
        auto xml = "<!DOCTYPE sturgeon []>" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.docTypeEnd);
        enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
    }
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    <!ELEMENT foo EMPTY>\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.elementDecl);
        enforceTest(cursor.next() == EntityType.docTypeEnd);
        enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
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
    // elementDecl
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    <!ELEMENT foo EMPTY>\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.elementDecl);
        enforceTest(cursor.skipToParentDepth() == EntityType.docTypeEnd);
    }
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    <!ELEMENT foo EMPTY>\n" ~
                   "    <!-- comment -->\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.elementDecl);
        enforceTest(cursor.skipToParentDepth() == EntityType.docTypeEnd);
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
    // entityDecl
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    <!ENTITY foo 'bar'>\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.entityDecl);
        enforceTest(cursor.skipToParentDepth() == EntityType.docTypeEnd);
    }
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    <!ENTITY foo 'bar'>\n" ~
                   "    <!-- comment -->\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.entityDecl);
        enforceTest(cursor.skipToParentDepth() == EntityType.docTypeEnd);
    }
    // notationDecl
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    <!NOTATION name PUBLIC 'lit'>\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.notationDecl);
        enforceTest(cursor.skipToParentDepth() == EntityType.docTypeEnd);
    }
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    <!NOTATION name PUBLIC 'lit'>\n" ~
                   "    <!-- comment -->\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.notationDecl);
        enforceTest(cursor.skipToParentDepth() == EntityType.docTypeEnd);
    }
    // peReference
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    %name;\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.peReference);
        enforceTest(cursor.skipToParentDepth() == EntityType.docTypeEnd);
    }
    {
        auto xml = "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    %name;\n" ~
                   "    <!-- comment -->\n" ~
                   "]>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.docTypeStart);
        enforceTest(cursor.next() == EntityType.peReference);
        enforceTest(cursor.skipToParentDepth() == EntityType.docTypeEnd);
    }
    // pi
    {
        auto xml = "<?Sherlock?>\n" ~
                   "<!DOCTYPE sturgeon\n" ~
                   "[\n" ~
                   "    <?Poirot?>\n" ~
                   "    <!-- comment -->\n" ~
                   "    <?Conan?>\n" ~
                   "]>\n" ~
                   "<?Dick?>\n" ~
                   "<?Tracy?>\n" ~
                   "<root>\n" ~
                   "    <?Foo?>\n" ~
                   "    <nothing/>\n" ~
                   "    <?Bar?>\n" ~
                   "    <foo></foo>\n" ~
                   "    <?Baz?>\n" ~
                   "</root>\n" ~
                   "<?post?>\n" ~
                   "<?master?>";
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
            enforceTest(cursor.next() == EntityType.docTypeStart);
            enforceTest(cursor.next() == EntityType.pi);
            if(i == 1)
            {
                enforceTest(cursor.name == "Poirot");
                enforceTest(cursor.skipToParentDepth() == EntityType.docTypeEnd);
                continue;
            }
            enforceTest(cursor.next() == EntityType.comment);
            enforceTest(cursor.next() == EntityType.pi);
            if(i == 2)
            {
                enforceTest(cursor.name == "Conan");
                enforceTest(cursor.skipToParentDepth() == EntityType.docTypeEnd);
                continue;
            }
            enforceTest(cursor.next() == EntityType.docTypeEnd);
            enforceTest(cursor.next() == EntityType.pi);
            if(i == 3)
            {
                enforceTest(cursor.name == "Dick");
                enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
                continue;
            }
            enforceTest(cursor.next() == EntityType.pi);
            if(i == 4)
            {
                enforceTest(cursor.name == "Tracy");
                enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
                continue;
            }
            enforceTest(cursor.next() == EntityType.elementStart);
            enforceTest(cursor.next() == EntityType.pi);
            if(i == 5)
            {
                enforceTest(cursor.name == "Foo");
                enforceTest(cursor.skipToParentDepth() == EntityType.elementEnd);
                enforceTest(cursor.name == "root");
                continue;
            }
            enforceTest(cursor.next() == EntityType.elementEmpty);
            enforceTest(cursor.next() == EntityType.pi);
            if(i == 6)
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
            if(i == 7)
            {
                enforceTest(cursor.name == "post");
                enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
                continue;
            }
            enforceTest(cursor.next() == EntityType.pi);
            enforceTest(cursor.name == "master");
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
    // xmlDecl
    {
        auto xml = "<?xml version='1.0'?>\n" ~
                   "<root></root>";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.xmlDecl);
        enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
    }
    {
        auto xml = "<?xml version='1.0'?>\n" ~
                   "<!DOCTYPE foo []>\n" ~
                   "<!-- beginning -->\n" ~
                   "<root>\n" ~
                   "</root>\n" ~
                   "<!-- end -->";
        auto cursor = parseXML(xml);
        enforceTest(cursor.next() == EntityType.xmlDecl);
        enforceTest(cursor.skipToParentDepth() == EntityType.documentEnd);
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

    // doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
    // intSubset   ::= (markupdecl | DeclSep)*
    // This is the intSubset such that the next thing to parse is a markupdecl,
    // DeclSep, or the ']' that follows the intSubset.
    intSubset,

    // doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
    // intSubset   ::= (markupdecl | DeclSep)*
    // markupdecl  ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
    // AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
    // AttDef      ::= S Name S AttType S DefaultDecl
    // This is the AttDef such that the next thing to parse is '>', whitespace
    // followed by '>', or whitespace followed by Name.
    intSubsetAttDef,

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



// ExternalID    ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
// SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
// PubidLiteral  ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
// PubidChar     ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
// NotationDecl  ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
// PublicID      ::= 'PUBLIC' S PubidLiteral
// This extracts the exteral ID or Public ID. Since the whitespace on the right
// must be stripped to determine whether there's a second literal, the
// whitespace is always stripped in order to be consistent.
auto takeID(bool requireSystemAfterPublic, bool returnWasSpace, PS)(PS state)
{
    static assert(isPointer!PS, "_state.savedText was probably passed rather than &_state.savedText");

    pragma(inline, true) static isQuote(typeof(state.input.front) c) { return c == '"' || c == '\''; }

    alias config = PS.config;
    alias Text = PS.Text;
    alias SliceOfR = PS.SliceOfR;

    typeof(PS.id.get.init) retval;
    bool wasSpace;

    if(state.stripStartsWith("PUBLIC"))
    {
        if(!stripWS(state))
            throw new XMLParsingException("There must be whitespace after PUBLIC", state.pos);
        if(!isQuote(state.input.front))
            throw new XMLParsingException("Missing quote", state.pos);
        retval.publicLiteral = stripBCU!Text(state.takePubidLiteral());
        wasSpace = stripWS(state);
        checkNotEmpty(state);
        if(isQuote(state.input.front))
        {
            if(!wasSpace)
            {
                throw new XMLParsingException("There must be whitespace between a Public ID literal " ~
                                              "and a System literal", state.pos);
            }
            retval.systemLiteral = stripBCU!Text(state.takeSystemLiteral());
            static if(returnWasSpace)
                wasSpace = stripWS(state);
            else
                stripWS(state);
            checkNotEmpty(state);
        }
        else
        {
            static if(requireSystemAfterPublic)
                throw new XMLParsingException("System literal missing after public ID literal", state.pos);
        }
    }
    else if(state.stripStartsWith("SYSTEM"))
    {
        if(!stripWS(state))
            throw new XMLParsingException("There must be whitespace after SYSTEM", state.pos);
        if(!isQuote(state.input.front))
            throw new XMLParsingException("Missing quote", state.pos);
        retval.systemLiteral = stripBCU!Text(state.takeSystemLiteral());
        static if(returnWasSpace)
            wasSpace = stripWS(state);
        else
            stripWS(state);
        checkNotEmpty(state);
    }
    else
        throw new XMLParsingException("Expected SYSTEM or PUBLIC", state.pos);

    static if(returnWasSpace)
    {
        import std.typecons : tuple;
        return tuple(retval, wasSpace);
    }
    else
        return retval;
}

unittest
{
    import std.algorithm.comparison : equal;
    import std.exception : assertThrown;
    import std.typecons : Nullable, nullable, Tuple;

    static bool eqLit(T, U)(T lhs, U rhs)
    {
        if(lhs.isNull != rhs.isNull)
            return false;
        if(lhs.isNull)
            return true;
        return equal(lhs, rhs);
    }

    static void test(alias func, bool rsap, bool rws, ID)(string origHaystack, ID expected, string remainder,
                                                          bool expectedWS, int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);

        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto state = testParser!config(haystack.save);
                auto result = state.takeID!(rsap, rws)();
                static if(rws)
                    auto id = result[0];
                else
                    auto id = result;
                enforceTest(eqLit(id.publicLiteral, expected.publicLiteral), "unittest failure 1", line);
                enforceTest(eqLit(id.systemLiteral, expected.systemLiteral), "unittest failure 2", line);
                enforceTest(equal(state.input, remainder), "unittest failure 3", line);
                enforceTest(state.pos == pos, "unittest failure 4", line);
                static if(rws)
                    enforceTest(result[1] == expectedWS, "unittest failure 5", line);
            }
            static if(i != 2)
            {
                auto pos = SourcePos(row + 3, i == 0 ? (row == 1 ? col + 7 : col) : -1);
                auto state = testParser!config(haystack.save);
                state.pos.line += 3;
                static if(i == 0)
                    state.pos.col += 7;
                auto result = state.takeID!(rsap, rws)();
                static if(rws)
                    auto id = result[0];
                else
                    auto id = result;
                enforceTest(eqLit(id.publicLiteral, expected.publicLiteral), "unittest failure 6", line);
                enforceTest(eqLit(id.systemLiteral, expected.systemLiteral), "unittest failure 7", line);
                enforceTest(equal(state.input, remainder), "unittest failure 8", line);
                enforceTest(state.pos == pos, "unittest failure 9", line);
                static if(rws)
                    enforceTest(result[1] == expectedWS, "unittest failure 10", line);
            }
        }}
    }

    static void testFail(alias func, bool rsap, bool rws)(string origHaystack, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);
        static foreach(i, config; testConfigs)
        {{
            auto state = testParser!config(haystack.save);
            assertThrown!XMLParsingException(state.takeID!(rsap, rws)(), "unittest failure", __FILE__, line);
        }}
    }

    static auto pubLit(string[] literals...)
    {
        Tuple!(Nullable!string, "publicLiteral", Nullable!string, "systemLiteral") retval;
        retval.publicLiteral = nullable(literals[0]);
        if(literals.length != 1)
            retval.systemLiteral = nullable(literals[1]);
        return retval;
    }

    static auto sysLit(string literal)
    {
        Tuple!(Nullable!string, "publicLiteral", Nullable!string, "systemLiteral") retval;
        retval.systemLiteral = nullable(literal);
        return retval;
    }

    static foreach(func; testRangeFuncs)
    {
        static foreach(rws; [false, true])
        {
            static foreach(rsap; [false, true])
            {
                test!(func, rsap, rws)(`SYSTEM "Name">`, sysLit(`Name`), ">", false, 1, 14);
                test!(func, rsap, rws)(`SYSTEM 'Name'>`, sysLit(`Name`), ">", false, 1, 14);
                test!(func, rsap, rws)(`SYSTEM "Name"   >`, sysLit(`Name`), ">", true, 1, 17);
                test!(func, rsap, rws)("SYSTEM 'Name'\n\n>", sysLit(`Name`), ">", true, 3, 1);
                test!(func, rsap, rws)("SYSTEM\n\n\n'Name'    >", sysLit("Name"), ">", true, 4, 11);
                test!(func, rsap, rws)("SYSTEM  'Foo\nBar'>", sysLit("Foo\nBar"), ">", false, 2, 5);
                test!(func, rsap, rws)(`SYSTEM "">`, sysLit(``), ">", false, 1, 10);

                test!(func, rsap, rws)(`PUBLIC "Name" 'Foo'>`, pubLit(`Name`, `Foo`), ">", false, 1, 20);
                test!(func, rsap, rws)(`PUBLIC 'Name' "Foo">`, pubLit(`Name`, `Foo`), ">", false, 1, 20);
                test!(func, rsap, rws)(`PUBLIC "Name"   "thing"  >`, pubLit(`Name`, "thing"), ">", true, 1, 26);
                test!(func, rsap, rws)("PUBLIC 'Name' \n 'thing'\n\n>", pubLit(`Name`, "thing"), ">", true, 4, 1);
                test!(func, rsap, rws)("PUBLIC\n\n\n'Name'    'Foo' >", pubLit("Name", "Foo"), ">", true, 4, 17);
                test!(func, rsap, rws)("PUBLIC  'Foo\nBar' 'Foo'>", pubLit("Foo\nBar", "Foo"), ">", false, 2, 11);
                test!(func, rsap, rws)("PUBLIC 'A' \n\n 'B'>", pubLit("A", "B"), ">", false, 3, 5);
                test!(func, rsap, rws)("PUBLIC 'A' 'B\n\n\n'>", pubLit("A", "B\n\n\n"), ">", false, 4, 2);
                test!(func, rsap, rws)("PUBLIC '' ''>", pubLit("", ""), ">", false, 1, 13);

                testFail!(func, rsap, rws)(`SYSTEM`);
                testFail!(func, rsap, rws)(`SYSTEM>`);
                testFail!(func, rsap, rws)(`SYSTEM >`);
                testFail!(func, rsap, rws)(`SYSTEM ">`);
                testFail!(func, rsap, rws)(`SYSTEM '>`);
                testFail!(func, rsap, rws)(`SYSTEM ""`);
                testFail!(func, rsap, rws)(`SYSTEM ''`);
                testFail!(func, rsap, rws)(`SYSTEM "'>`);
                testFail!(func, rsap, rws)(`SYSTEM '">`);

                testFail!(func, rsap, rws)(`PUBLIC`);
                testFail!(func, rsap, rws)(`PUBLIC>`);
                testFail!(func, rsap, rws)(`PUBLIC >`);
                testFail!(func, rsap, rws)(`PUBLIC ">`);
                testFail!(func, rsap, rws)(`PUBLIC '>`);
                testFail!(func, rsap, rws)(`PUBLIC ""`);
                testFail!(func, rsap, rws)(`PUBLIC "'>`);
                testFail!(func, rsap, rws)(`PUBLIC '">`);
                testFail!(func, rsap, rws)(`PUBLIC "" ">`);
                testFail!(func, rsap, rws)(`PUBLIC "" '>`);
                testFail!(func, rsap, rws)(`PUBLIC '' ">`);
                testFail!(func, rsap, rws)(`PUBLIC '' '>`);
                testFail!(func, rsap, rws)(`PUBLIC "" ""`);
                testFail!(func, rsap, rws)(`PUBLIC """"`);
                testFail!(func, rsap, rws)(`PUBLIC '' ''`);
                testFail!(func, rsap, rws)(`PUBLIC ''''`);
            }

            test!(func, false, rws)(`PUBLIC "Name">`, pubLit(`Name`), ">", false, 1, 14);
            test!(func, false, rws)(`PUBLIC 'Name'>`, pubLit(`Name`), ">", false, 1, 14);
            test!(func, false, rws)(`PUBLIC "Name"   >`, pubLit(`Name`), ">", true, 1, 17);
            test!(func, false, rws)("PUBLIC 'Name'\n\n>", pubLit(`Name`), ">", true, 3, 1);
            test!(func, false, rws)("PUBLIC\n\n\n'Name'    >", pubLit("Name"), ">", true, 4, 11);
            test!(func, false, rws)("PUBLIC  'Foo\nBar'>", pubLit("Foo\nBar"), ">", false, 2, 5);
            test!(func, false, rws)(`PUBLIC "">`, pubLit(``), ">", false, 1, 10);

            testFail!(func, true, rws)(`PUBLIC "Name">`);
            testFail!(func, true, rws)(`PUBLIC 'Name'>`);
            testFail!(func, true, rws)(`PUBLIC "Name"  >`);
            testFail!(func, true, rws)("PUBLIC 'Name'\n\n>");
            testFail!(func, true, rws)("PUBLIC\n\n\n'Name'    >");
            testFail!(func, true, rws)("PUBLIC  'Foo\nBar'>");
            testFail!(func, true, rws)(`PUBLIC "">`);
        }
    }
}


// ExternalID    ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
// SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
// PubidLiteral  ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
// PubidChar     ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
// NotationDecl  ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
// PublicID      ::= 'PUBLIC' S PubidLiteral
// This extracts the PubidLiteral from a previously extracted public or external
// ID. The PUBLIC and any whitespace after it has already been removed.
auto takePubidLiteral(PS)(PS state)
{
    return takeLiteral!(true, "PubidLiteral")(state);
}

auto takeLiteral(bool checkPubidChar, string literalName, PS)(PS state)
{
    static assert(isPointer!PS, "_state.savedText was probably passed rather than &_state.savedText");

    alias config = PS.config;

    immutable quote = state.input.front;
    assert(quote == '"' || quote == '\'');
    state.input.popFront();
    nextCol!config(state.pos);
    checkNotEmpty(state);

    auto temp = state.input.save;
    size_t takeLen;

    static if(config.posType != PositionType.none)
        size_t lineStart = 0;

    for(auto c = cast()state.input.front;
        c != quote;
        state.input.popFront(), checkNotEmpty(state), c = state.input.front)
    {
        static if(checkPubidChar)
        {
            if(!isPubidChar(c))
            {
                import std.format : format;
                auto pos = state.pos;
                static if(config.posType == PositionType.lineAndCol)
                    pos.col += takeLen - lineStart;
                throw new XMLParsingException(format!"%s literal contains invalid character: '%s'"(literalName, c),
                                              state.pos);
            }
        }
        static if(config.posType != PositionType.none)
        {
            if(c == '\n')
            {
                lineStart = ++takeLen;
                nextLine!config(state.pos);
            }
            else
                ++takeLen;
        }
        else
            ++takeLen;
    }

    static if(config.posType == PositionType.lineAndCol)
        state.pos.col += takeLen - lineStart;
    state.input.popFront();
    nextCol!config(state.pos);

    return takeExactly(temp, takeLen);
}

unittest
{
    import std.algorithm.comparison : equal;
    import std.exception : assertThrown;

    static void test(alias func)(string origHaystack, string expected, string remainder,
                                 int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);

        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto state = testParser!config(haystack.save);
                enforceTest(equal(state.takePubidLiteral(), expected), "unittest failure 1", line);
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
                enforceTest(equal(state.takePubidLiteral(), expected), "unittest failure 4", line);
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
            assertThrown!XMLParsingException(state.takePubidLiteral(), "unittest failure", __FILE__, line);
        }}
    }

    static foreach(func; testRangeFuncs)
    {
        test!func(`""`, ``, "", 1, 3);
        test!func(`"Name"`, `Name`, "", 1, 7);
        test!func(`'Name'`, `Name`, "", 1, 7);
        test!func(`'Name'    `, `Name`, "    ", 1, 7);
        test!func("'Name'\n\n  ", `Name`, "\n\n  ", 1, 7);
        test!func("'Name'  'Bar'  ", `Name`, "  'Bar'  ", 1, 7);
        test!func("'\n\n\n'  'Bar'  ", "\n\n\n", "  'Bar'  ", 4, 2);
        test!func(`"'''''''"`, "'''''''", "", 1, 10);
        test!func(`"""`, ``, `"`, 1, 3);
        test!func(`'''`, ``, `'`, 1, 3);

        foreach(char c; 0 .. 128)
        {
            import std.algorithm : canFind;
            import std.ascii : isAlphaNum;
            if(isAlphaNum(c) || "\r -'()+,./:=?;!*#@$_%".canFind(c))
                test!func(`"` ~ c ~ `"`, [c], "", 1, 4);
            else if(c == '\n')
                test!func("'\n'", "\n", "", 2, 2);
            else if(c == '"')
                testFail!func(`'"'`);
            else
                testFail!func(`"` ~ c ~ `"`);
        }

        testFail!func(`"`);
        testFail!func(`'`);
        testFail!func(`">><<<>>><<>"`);
        testFail!func("'>><>\n\n\"><>'");
        testFail!func(`'"'`);
    }
}


// ExternalID    ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
// SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
// PubidLiteral  ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
// PubidChar     ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
// NotationDecl  ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
// PublicID      ::= 'PUBLIC' S PubidLiteral
// This extracts the SystemLiteral from a previously extracted external ID. The
// SYSTEM and any whitespace after it has already been removed.
auto takeSystemLiteral(PS)(PS state)
{
    return takeLiteral!(false, "SystemLiteral")(state);
}

unittest
{
    import std.algorithm.comparison : equal;
    import std.exception : assertThrown;

    static void test(alias func)(string origHaystack, string expected, string remainder,
                                 int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);

        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto state = testParser!config(haystack.save);
                enforceTest(equal(state.takeSystemLiteral(), expected), "unittest failure 1", line);
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
                enforceTest(equal(state.takeSystemLiteral(), expected), "unittest failure 4", line);
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
            assertThrown!XMLParsingException(state.takeSystemLiteral(), "unittest failure", __FILE__, line);
        }}
    }

    static foreach(func; testRangeFuncs)
    {
        test!func(`""`, ``, "", 1, 3);
        test!func(`"Name"`, `Name`, "", 1, 7);
        test!func(`'Name'`, `Name`, "", 1, 7);
        test!func(`'Name'    `, `Name`, "    ", 1, 7);
        test!func("'Name'\n\n  ", `Name`, "\n\n  ", 1, 7);
        test!func("'Name'  'Bar'  ", `Name`, "  'Bar'  ", 1, 7);
        test!func("'\n\n\n'  'Bar'  ", "\n\n\n", "  'Bar'  ", 4, 2);
        test!func(`"'''''''"`, "'''''''", "", 1, 10);
        test!func(`"""`, ``, `"`, 1, 3);
        test!func(`'''`, ``, `'`, 1, 3);
        test!func(`">><<<>>><<''''''>"`, `>><<<>>><<''''''>`, "", 1, 20);
        test!func("'>><>\n\n\"><>'", ">><>\n\n\"><>", "", 3, 6);

        foreach(char c; 0 .. 128)
        {
            if(c == '"')
                test!func(`'"'`, `"`,  "", 1, 4);
            else if(c  == '\n')
                test!func("'\n'", "\n", "", 2, 2);
            else
                test!func(`"` ~ c ~ `"`, [c], "", 1, 4);
        }

        testFail!func(`"`);
        testFail!func(`'`);
    }
}


// Returns a block of text encased by parens. The first character must be an
// '(', and the parens on either end of the text will be included in the
// slice/takeExactly that's returned. Parens are allowed to nest and will be
// matched appropriately. If '>' is encountered or the end of the text is
// reached prior to the last matching ')', then an XMLParsingException is thrown.
auto takeParenedText(PS)(PS state)
{
    static assert(isPointer!PS, "_state.savedText was probably passed rather than &_state.savedText");
    assert(!state.input.empty);
    assert(state.input.front == '(');

    static if(PS.config.posType == PositionType.lineAndCol)
        size_t lineStart;
    size_t takeLen = 1;
    auto orig = state.input.save;

    state.input.popFront();

    for(int parens = 1; parens != 0; state.input.popFront())
    {
        checkNotEmpty(state);
        switch(state.input.front)
        {
            case '(':
            {
                ++takeLen;
                ++parens;
                break;
            }
            case ')':
            {
                ++takeLen;
                --parens;
                break;
            }
            case '>':
            {
                static if(PS.config.posType == PositionType.lineAndCol)
                    state.pos.col += takeLen - lineStart;
                throw new XMLParsingException("found > when searching for matching paren", state.pos);
            }
            static if(PS.config.posType != PositionType.none)
            {
                case '\n':
                {
                    ++takeLen;
                    nextLine!(PS.config)(state.pos);
                    static if(PS.config.posType == PositionType.lineAndCol)
                        lineStart = takeLen;
                    break;
                }
            }
            default:
            {
                ++takeLen;
                break;
            }
        }
    }

    static if(PS.config.posType == PositionType.lineAndCol)
        state.pos.col += takeLen - lineStart;

    return takeExactly(orig, takeLen);
}

unittest
{
    import std.algorithm.comparison : equal;
    import std.exception : assertThrown;

    static void test(alias func)(string origHaystack, string expected, string remainder,
                                 int row, int col, size_t line = __LINE__)
    {
        auto haystack = func(origHaystack);

        static foreach(i, config; testConfigs)
        {{
            {
                auto pos = SourcePos(i < 2 ? row : -1, i == 0 ? col : -1);
                auto state = testParser!config(haystack.save);
                enforceTest(equal(state.takeParenedText(), expected), "unittest failure 1", line);
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
                enforceTest(equal(state.takeParenedText(), expected), "unittest failure 4", line);
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
            assertThrown!XMLParsingException(state.takeParenedText(), "unittest failure", __FILE__, line);
        }}
    }

    static foreach(func; testRangeFuncs)
    {
        test!func("(hello)", "(hello)", "", 1, 8);
        test!func("(hello)world", "(hello)", "world", 1, 8);
        test!func("(hello|world) bar>", "(hello|world)", " bar>", 1, 14);
        test!func("(()()()())", "(()()()())", "", 1, 11);
        test!func("((((()))))", "((((()))))", "", 1, 11);
        test!func("((()())())", "((()())())", "", 1, 11);
        test!func("(((\n)())\n())", "(((\n)())\n())", "", 3, 4);
        test!func("(((\n)())\n())\n\nfoo", "(((\n)())\n())", "\n\nfoo", 3, 4);
        test!func("(((\n)())\n())>", "(((\n)())\n())", ">", 3, 4);
        test!func("()(", "()", "(", 1, 3);
        test!func("((((()))))(", "((((()))))", "(", 1, 11);

        testFail!func("(");
        testFail!func("((");
        testFail!func("(((()))");
        testFail!func("(>");
        testFail!func("(>)");
        testFail!func("(hello>");
        testFail!func("(hello>)");
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


// PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
bool isPubidChar(dchar c)
{
    import std.ascii : isAlphaNum;
    return isAlphaNum(c) || c == ' ' || c == '\r' || c == '\n' ||
           c == '-' || c == '\'' || c == '(' || c == ')' || c == '+' || c == ',' ||
           c == '.' || c == '/' || c == ':' || c == '=' || c == '?' || c == ';' ||
           c == '!' || c == '*' || c == '#' || c == '@' || c == '$' || c == '_' || c == '%';
}

unittest
{
    import std.algorithm : canFind;
    import std.ascii : isAlphaNum;
    import std.meta : AliasSeq;
    import std.utf : byCodeUnit;

    static foreach(C; AliasSeq!(char, wchar, dchar))
    {
        foreach(c; cast(C)0 .. cast(C)129)
        {
            if(isAlphaNum(c) || " \r\n-'()+,./:=?;!*#@$_%".byCodeUnit().canFind(c))
                assert(isPubidChar(c));
            else
                assert(!isPubidChar(c));
        }
        foreach(c; cast(C)129 .. cast(C)256)
            assert(!isPubidChar(c));
        static if(!is(C == char))
        {
            foreach(c; cast(C)256 .. cast(C)300)
                assert(!isPubidChar(c));
        }
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
