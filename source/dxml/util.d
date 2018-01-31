// Written in the D programming language

/++
    This module contains helper functions which aren't specific to a particular
    parser or writer.

    Copyright: Copyright 2018
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   Jonathan M Davis
    Source:    $(LINK_TO_SRC dxml/parser/_cursor.d)

    See_Also: $(LINK2 http://www.w3.org/TR/REC-xml/, Official Specification for XML 1.0)
  +/
module dxml.util;

import std.range.primitives;
import std.traits;
import std.typecons : Nullable;;


/++
    "Normalizes" the given text and transforms character references to the
    characters that they represent. normalize combines $(LREF parseStdEntityRef)
    and $(LREF parseCharRef) along with processing for $(D_STRING '\r') to
    normalize an entire character range. It's intended to be used on on the text
    fields of entities and on the values of start tag attributes.

    There are a number of characters that either can't be directly represented
    in the text fields or attribute values in XML or which can sometimes be
    directly represented but not always (e.g. an attribute value can contain
    either $(D_STRING '\'') or $(D_STRING '"'), but it can't contain both at the
    same time, because one of them would match the opening quote). So, those
    characters have alternate representations in order to be allowed
    (e.g. $(D_STRING "&lt;") for $(D_STRING '>'), because $(D_STRING '>') would
    normally be the end of an entity). Technically, they're entity references,
    but the ones handled by normalize are the predefined ones mentioned in the
    XML standard and which don't require a DTD section.

    Ideally, the parser would transform all such alternate representations to
    what they represent when providing the text to the application, but that
    would make it impossible to return slices of the original text from the
    properties of a $(LREF2 Entity, EntityRange). So, instead of having those
    properties do the transformation themselves, normalize and asNormalized do
    that so that the application can choose to do it or not (in many cases,
    there is nothing to normalize, making the calls unnecessary).

    Similarly, an application can choose to encode a character as a character
    reference (e.g. $(D_STRING '&#65") or $(D_STRING '&#x40") for
    $(D_STRING 'A')). normalize will decode such character references to their
    corresponding character.

    However, normalize does not handle any entity references beyond the five
    predefined ones listed below. All others are left unprocessed. Processing
    them properly would require handling the DTD section, which dxml does not
    do. The parser considers any entity references other than the predefined
    ones to be invalid XML, so unless the text being passed to normalize
    doesn't come from one of dxml's parsers, it can't have any entity
    references in it other than the predefined ones. Similarly, invalid
    character references are left unprocessed as well as any character that is
    not valid in an XML document. normalize never throws on invalid XML.

    Also, $(D_STRING '\r') is not supposed to appear in an XML document except
    as a character reference unless it's in a CDATA section. So, it really
    should be stripped out before being handed off to the application, but
    again, that doesn't work with slices. So, normalize also handles that.

    Specifically, what normalize and asNormalized do is

    $(TABLE
        $(TR $(TD convert $(D_STRING "&amp;") to $(D_STRING '&')))
        $(TR $(TD convert $(D_STRING "&gt;") to $(D_STRING '>')))
        $(TR $(TD convert $(D_STRING "&lt;") to $(D_STRING '<')))
        $(TR $(TD convert $(D_STRING "&apos;") to $(D_STRING '\'')))
        $(TR $(TD convert $(D_STRING "&quot;") to $(D_STRING '"')))
        $(TR $(TD remove all instances of $(D_STRING '\r')))
        $(TR $(TD convert all character references (e.g. $(D_STRING "&#xA;"))
             to the characters that they represent))
    )

    All other entity references are left untouched, and any $(D_STRING '&')
    which is not used in one of the constructs listed in the table as well as
    any malformed constructs (e.g. $(D_STRING "&Amp;" or $(D_STRING &#xGGA2;")
    are left untouched.

    The difference between normalize and asNormalized is that normalize returns
    a $(D string), whereas asNormalized returns a lazy range of code units. In
    the case where a $(D string) is passed to normalize, it will simply return
    the original string if there is no text to normalize (whereas in other
    cases, normalize and asNormalized are forced to return new ranges even if
    there is no un-normalized text).

    Returns: The normalized text. normalize returns a $(D string), whereas
             asNormalized returns a lazy range of code units (so it could be a
             range of $(D char) or $(D wchar) and not just $(D dchar) - which
             it is depends on the code units of the range being passed in).

    See_Also: $(LINK http://www.w3.org/TR/REC-xml/#dt-chardata)$(BR)
              $(LREF parseStdEntityRef)$(BR)
              $(LREF parseCharRef)$(BR)
              $(REF EntityRange.Entity.attributes, dxml, parser, stax)($BR)
              $(REF EntityRange.Entity.text, dxml, parser, stax)
  +/
string normalize(R)(R range)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    static if(isDynamicArray!R && is(Unqual!(ElementEncodingType!R) == char))
    {
        import std.algorithm.searching : find, startsWith;
        import std.array : appender;
        import std.meta : AliasSeq;

        auto found = range.find('&', '\r');
        if(found[1] == 0)
            return range;

        auto retval = appender!string();
        retval.reserve(range.length);
        put(retval, range[0 .. $ - found[0].length]);
        range = range[$ - found[0].length .. $];

        size_t i = 0;
        loop: for(; i != range.length;)
        {
            switch(range[i])
            {
                case '&':
                {
                    if(i + 1 == range.length)
                    {
                        ++i;
                        break loop;
                    }
                    put(retval, range[0 .. i]);
                    range = range[i .. $];
                    i = 0;
                    static foreach(func; AliasSeq!(parseStdEntityRef, parseCharRef))
                    {{
                        immutable c = func(range);
                        if(!c.isNull)
                        {
                            put(retval, c.get);
                            continue loop;
                        }
                    }}
                    put(retval, '&');
                    range = range[1 .. $];
                    continue;
                }
                case '\r':
                {
                    if(i != 0)
                    {
                        put(retval, range[0 .. i]);
                        range = range[i + 1 .. $];
                        i = 0;
                    }
                    else
                        range = range[1 .. $];
                    continue;
                }
                default: ++i; continue;
            }
        }

        if(i != 0)
            put(retval, range[0 .. i]);

        return retval.data;
    }
    else
    {
        import std.conv : to;
        return range.asNormalized().to!string();
    }
}


/// Ditto
auto asNormalized(R)(R range)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    import std.meta : AliasSeq;
    import std.utf : byCodeUnit, encode, UseReplacementDchar;

    static struct NormalizedResult
    {
    public:

        @property empty() { return _range.empty && _front.empty; }

        void popFront()
        {
            if(!_front.empty)
            {
                _front = _front[1 .. $];
                if(!_front.empty)
                    return;
            }
            else
                _range.popFront();
            _popFrontImpl();
        }

        @property save()
        {
            auto retval = this;
            retval._range = _range.save;
            return retval;
        }

        this(this)
        {
            () @trusted { _front = _buffer[0 .. _front.length]; }();
        }

    private:

        void _popFrontImpl()
        {
            while(!_range.empty)
            {
                switch(_range.front)
                {
                    case '&':
                    {
                        static foreach(func; AliasSeq!(parseStdEntityRef, parseCharRef))
                        {{
                            immutable c = func(_range);
                            if(!c.isNull)
                            {
                                () @trusted { _front = _buffer[0 .. _buffer.encode!(UseReplacementDchar.yes)(c)]; }();
                                return;
                            }
                        }}
                        goto default;
                    }
                    case '\r':
                    {
                        _front = null;
                        _range.popFront();
                        continue;
                    }
                    default:
                    {
                        _front = null;
                        return;
                    }
                }
            }
        }

        this(R range) @safe
        {
            _range = byCodeUnit(range);
            _popFrontImpl();
        }

        typeof(byCodeUnit(R.init)) _range;
        static if(is(Unqual!(ElementEncodingType!R) == char))
            char[4] _buffer;
        else static if(is(Unqual!(ElementEncodingType!R) == wchar))
            wchar[2] _buffer;
        else
            dchar[1] _buffer;
        typeof(_buffer[0])[] _front;

    public:

        // FIXME A compiler bug prevents this from going with the public declarations
        // above. If it's there, the compiler thinks that _buffer isn't defined when
        // it tries to compile _front. It needs to be reduced and reported.
        @property typeof(_front[0]) front() { return _front.empty ? _range.front : _front[0]; }
    }

    return NormalizedResult(range);
}

///
unittest
{
    assert(normalize("hello world &amp;&gt;&lt;&apos;&quot; \r\r\r\r\r foo") ==
           `hello world &><'"  foo`);

    assert(normalize("if(foo &amp;&amp; bar)\r\n" ~
                     "    left = right;") ==
           "if(foo && bar)\n" ~
           "    left = right;");

    assert(normalize("&#12487;&#12451;&#12521;&#12531;") == "ディラン");
    assert(normalize("foo") == "foo");
    assert(normalize("&#   ;") == "&#   ;");

    {
        import std.algorithm.comparison : equal;
        auto range = asNormalized("hello world &amp;&gt;&lt;&apos;&quot; " ~
                                  "\r\r\r\r\r foo");
        assert(equal(range, `hello world &><'"  foo`));
    }

    {
        import dxml.parser.stax;
        auto xml = "<root>\n" ~
                   "    <function return='vector&lt;int&gt;' name='foo'>\r\n" ~
                   "        <doc_comment>This function does something really\r\n" ~
                   "                 fancy, and you will love it.</doc_comment>\r\n" ~
                   "        <param type='int' name='i'>\r\n" ~
                   "        <param type='const std::string&amp;' name='s'>\r\n" ~
                   "    </function>\n" ~
                   "</root>";
        auto range = parseXML!simpleXML(xml);
        range.popFront();
        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "function");
        {
            auto attrs = range.front.attributes;
            assert(attrs.front.name == "return");
            assert(attrs.front.value == "vector&lt;int&gt;");
            assert(normalize(attrs.front.value) == "vector<int>");
            attrs.popFront();
            assert(attrs.front.name == "name");
            assert(attrs.front.value == "foo");
            assert(normalize(attrs.front.value) == "foo");
        }
        range.popFront();

        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "doc_comment");
        range.popFront();

        assert(range.front.text ==
               "This function does something really\r\n" ~
               "                 fancy, and you will love it.");
        assert(normalize(range.front.text) ==
               "This function does something really\n" ~
               "                 fancy, and you will love it.");
        range.popFront();

        assert(range.front.type == EntityType.elementEnd);
        assert(range.front.name == "doc_comment");
        range.popFront();

        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "param");
        {
            auto attrs = range.front.attributes;
            assert(attrs.front.name == "type");
            assert(attrs.front.value == "int");
            assert(normalize(attrs.front.value) == "int");
            attrs.popFront();
            assert(attrs.front.name == "name");
            assert(attrs.front.value == "i");
            assert(normalize(attrs.front.value) == "i");
        }
        range.popFront();

        assert(range.front.type == EntityType.elementStart);
        assert(range.front.name == "param");
        {
            auto attrs = range.front.attributes;
            assert(attrs.front.name == "type");
            assert(attrs.front.value == "const std::string&amp;");
            assert(normalize(attrs.front.value) == "const std::string&");
            attrs.popFront();
            assert(attrs.front.name == "name");
            assert(attrs.front.value == "s");
            assert(normalize(attrs.front.value) == "s");
        }
    }
}

unittest
{
    import core.exception : AssertError;
    import std.algorithm.comparison : equal;
    import std.exception : enforce;
    import std.utf : byUTF;
    import dxml.internal : testRangeFuncs;

    static void test(alias func)(string text, string expected, size_t line = __LINE__)
    {
        auto range = func(text);
        enforce!AssertError(range.save.normalize() == expected, "unittest failed 1", __FILE__, line);
        alias C = ElementType!(typeof(range.save.asNormalized()));
        enforce!AssertError(equal(range.save.asNormalized(), expected.byUTF!C), "unittest failed 2", __FILE__, line);
    }

    static foreach(func; testRangeFuncs)
    {{
        test!func("hello world &amp;  &gt;  &lt;  &apos;  &quot; \r\r\r\r\r foo", `hello world &  >  <  '  "  foo`);
        test!func("&amp", "&amp");
        test!func("&#01234567890;", "&#01234567890;");
        test!func("&", "&");
        test!func("&&&&", "&&&&");
        test!func("&&&&amp;", "&&&&");
        test!func("&#", "&#");
        test!func("&#;", "&#;");
        test!func("&#0", "&#0");
        test!func("&#0;", "&#0;");
        test!func("&#48;", "0");
        test!func("&#0amp;", "&#0amp;");
        test!func("&#amp;", "&#amp;");
        test!func("&#x", "&#x");
        test!func("&#x;", "&#x;");
        test!func("&#x0;", "&#x0;");
        test!func("&#x9;", "\t");
        test!func("&#x20;", " ");
        test!func("&#12487;&#12451;&#12521;&#12531;", "ディラン");
    }}
}

@safe pure unittest
{
    import std.algorithm.comparison : equal;
    import dxml.internal : testRangeFuncs;

    static foreach(func; testRangeFuncs)
    {{
        assert(normalize(func("foo")) == "foo");
        assert(equal(asNormalized(func("foo")), "foo"));
    }}
}


/++
    This parses one of the five, predefined entity references mention in the XML
    spec from the front of a range.

    If the given range starts with one of the five, predefined entity references,
    then it is removed from the range, and the corresponding character is
    returned.

    If the range does not start with one of those references, then the return
    value is null, and the range is unchanged.

    $(TABLE
        $(TR $(TH Std Entity Ref)$(TH Converts To))
        $(TR $(TD $(D_STRING "&amp;"))$(TD $(D_STRING '&')))
        $(TR $(TD $(D_STRING "&gt;"))$(TD $(D_STRING '>')))
        $(TR $(TD $(D_STRING "&lt;"))$(TD $(D_STRING '<')))
        $(TR $(TD $(D_STRING "&apos;"))$(TD $(D_STRING '\'')))
        $(TR $(TD $(D_STRING "&quot;"))$(TD $(D_STRING '"')))
    )

    Any other entity references would require processing a DTD section in order
    to be handled and are untouched by parseStdEntity as are any other types
    of references.

    See_Also: $(LINK http://www.w3.org/TR/REC-xml/#dt-chardata)$(BR)
              $(LREF parseCharRef)$(BR)
              $(LREF normalize)$(BR)
              $(LREF asNormalized)
  +/
Nullable!dchar parseStdEntityRef(R)(ref R range)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    import std.algorithm.searching : startsWith;
    import std.typecons : nullable, tuple;
    import std.utf : byCodeUnit;

    auto orig = range.save;

    static if(isNarrowString!R)
        auto cuRange = range.byCodeUnit();
    else
        alias cuRange = range;

    if(!cuRange.save.startsWith('&'))
        return typeof(return).init;
    cuRange.popFront();

    if(cuRange.empty)
        goto invalid;

    static foreach(t; [tuple("amp;", '&'), tuple("gt;", '>'), tuple("lt;", '<'),
                       tuple("apos;", '\''), tuple("quot;", '"')])
    {
        if(cuRange.save.startsWith(t[0]))
        {
            cuRange.popFrontN(t[0].length);
            static if(isNarrowString!R)
                range = cuRange.source;
            return nullable(cast(dchar)t[1]);
        }
    }

    invalid: range = orig;
    return typeof(return).init;
}

///
unittest
{
    {
        auto range = "&amp;foo";
        assert(range.parseStdEntityRef() == '&');
        assert(range == "foo");
    }
    {
        auto range = "&gt;bar";
        assert(range.parseStdEntityRef() == '>');
        assert(range == "bar");
    }
    {
        auto range = "&lt;baz";
        assert(range.parseStdEntityRef() == '<');
        assert(range == "baz");
    }
    {
        auto range = "&apos;dlang";
        assert(range.parseStdEntityRef() == '\'');
        assert(range == "dlang");
    }
    {
        auto range = "&quot;rocks";
        assert(range.parseStdEntityRef() == '"');
        assert(range == "rocks");
    }
    {
        auto range = " &amp;foo";
        assert(range.parseStdEntityRef().isNull);
        assert(range == " &amp;foo");
    }
    {
        auto range = "&Amp;hello";
        assert(range.parseStdEntityRef().isNull);
        assert(range == "&Amp;hello");
    }
    {
        auto range = "&nbsp;foo";
        assert(range.parseStdEntityRef().isNull);
        assert(range == "&nbsp;foo");
    }
    {
        auto range = "hello world";
        assert(range.parseStdEntityRef().isNull);
        assert(range == "hello world");
    }
}

unittest
{
    import std.algorithm.comparison : equal;
    import dxml.internal : testRangeFuncs;

    static foreach(func; testRangeFuncs)
    {
        for(auto range = func(";Amp;amp;&#amp;&copy;& amp;"); !range.empty; range.popFront())
        {
            auto temp = range.save;
            assert(temp.parseStdEntityRef().isNull);
            assert(equal(range.save, temp.save));
        }
        {
            auto range = func("&amp");
            assert(range.parseStdEntityRef().isNull);
            assert(equal(range.save, "&amp"));
        }
        {
            auto range = func(" &amp;&gt;&lt;&apos;&quot;");
            assert(range.parseStdEntityRef().isNull);
            assert(equal(range.save, " &amp;&gt;&lt;&apos;&quot;"));
            range.popFront();

            assert(range.parseStdEntityRef() == '&');
            assert(equal(range.save, "&gt;&lt;&apos;&quot;"));
            assert(range.parseStdEntityRef() == '>');
            assert(equal(range.save, "&lt;&apos;&quot;"));
            assert(range.parseStdEntityRef() == '<');
            assert(equal(range.save, "&apos;&quot;"));
            assert(range.parseStdEntityRef() == '\'');
            assert(equal(range.save, "&quot;"));
            assert(range.parseStdEntityRef() == '"');
            assert(range.empty);
        }
    }
}

@safe pure unittest
{
    import dxml.internal : testRangeFuncs;

    static foreach(func; testRangeFuncs)
    {{
        auto range = func("foo");
        assert(range.parseStdEntityRef().isNull);
    }}
}


/++
    If the given range starts with a valid, XML, character reference, it is
    removed from the range, and the corresponding character is returned.

    If the range does not start with a valid, XML, character reference, then
    the return value is null, and the range is unchanged.

    See_Also: $(LINK http://www.w3.org/TR/REC-xml/#NT-CharRef)$(BR)
              $(LREF parseStdEntityRef)$(BR)
              $(LREF normalize)$(BR)
              $(LREF asNormalized)
  +/
Nullable!dchar parseCharRef(R)(ref R range)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    import std.algorithm.searching : startsWith;
    import std.conv : ConvException, parse, to;
    import std.range : popFrontN;
    import std.typecons : nullable;
    import std.utf : byCodeUnit;

    auto orig = range.save;

    static if(isNarrowString!R)
        auto cuRange = range.byCodeUnit();
    else
        alias cuRange = range;

    if(!cuRange.save.startsWith("&#"))
        return typeof(return).init;
    cuRange.popFrontN(2);

    if(cuRange.empty)
        goto invalid;

    {
        bool hex = false;
        if(cuRange.front == 'x')
        {
            cuRange.popFront();
            hex = true;
            // https://issues.dlang.org/show_bug.cgi?id=18248
            import std.ascii : isHexDigit;
            if(cuRange.empty || !isHexDigit(cuRange.front))
                goto invalid;
        }
        try
        {
            immutable c = to!dchar(cuRange.parse!uint(hex ? 16 : 10));
            if(!cuRange.startsWith(';') || (c != '\n' && !isXMLChar(c)))
                goto invalid;
            cuRange.popFront();
            static if(isNarrowString!R)
                range = cuRange.source;
            return nullable(cast()c);
        }
        catch(ConvException)
        {}
    }

    invalid: range = orig;
    return typeof(return).init;
}

///
unittest
{
    {
        auto range = "&#48; hello world";
        assert(parseCharRef(range) == '0');
        assert(range == " hello world");
    }
    {
        auto range = "&#x30; hello world";
        assert(parseCharRef(range) == '0');
        assert(range == " hello world");
    }
    {
        auto range = "&#12487;&#12451;&#12521;&#12531;";
        assert(parseCharRef(range) == 'デ');
        assert(range == "&#12451;&#12521;&#12531;");
        assert(parseCharRef(range) == 'ィ');
        assert(range == "&#12521;&#12531;");
        assert(parseCharRef(range) == 'ラ');
        assert(range == "&#12531;");
        assert(parseCharRef(range) == 'ン');
        assert(range.empty);
    }
    {
        auto range = "&#x;foo";
        assert(parseCharRef(range).isNull);
        assert(range == "&#x;foo");
    }
    {
        auto range = "foobar";
        assert(parseCharRef(range).isNull);
        assert(range == "foobar");
    }
    {
        auto range = " &x48;";
        assert(parseCharRef(range).isNull);
        assert(range == " &x48;");
    }
}

unittest
{
    import std.algorithm.comparison : equal;
    import dxml.internal : testRangeFuncs;

    static foreach(func; testRangeFuncs)
    {
        for(auto range = func(";;&#;&#G;&#1234567890;&#F;"); !range.empty; range.popFront())
        {
            auto temp = range.save;
            assert(temp.parseCharRef().isNull);
            assert(equal(range.save, temp.save));
        }
        {
            auto range = func("&#65");
            assert(range.parseCharRef().isNull);
            assert(equal(range.save, "&#65"));
        }
        {
            auto range = func(" &#65;&#x42;&#67; &#x4EAC;&#x90FD;&#x5E02;");
            assert(range.parseCharRef().isNull);
            assert(equal(range.save, " &#65;&#x42;&#67; &#x4EAC;&#x90FD;&#x5E02;"));
            range.popFront();

            assert(range.parseCharRef() == 'A');
            assert(equal(range.save, "&#x42;&#67; &#x4EAC;&#x90FD;&#x5E02;"));
            assert(range.parseCharRef() == 'B');
            assert(equal(range.save, "&#67; &#x4EAC;&#x90FD;&#x5E02;"));
            assert(range.parseCharRef() == 'C');
            assert(equal(range.save, " &#x4EAC;&#x90FD;&#x5E02;"));

            assert(range.parseCharRef().isNull);
            assert(equal(range.save, " &#x4EAC;&#x90FD;&#x5E02;"));
            range.popFront();

            assert(range.parseCharRef() == '京');
            assert(equal(range.save, "&#x90FD;&#x5E02;"));
            assert(range.parseCharRef() == '都');
            assert(equal(range.save, "&#x5E02;"));
            assert(range.parseCharRef() == '市');
            assert(range.empty);
        }
    }
}

@safe pure unittest
{
    import dxml.internal : testRangeFuncs;

    static foreach(func; testRangeFuncs)
    {{
        auto range = func("foo");
        assert(range.parseCharRef().isNull);
    }}
}


/++
    Strips the indent from a character range (most likely from
    $(REF_ALTTEXT Entity.text, EntityRange.Entity.text, dxml, parser, stax)).
    The idea is that if the XML is formatted to be human-readable, and it's
    multiple lines long, it's likely going to be indented so that it's to the
    right of the tags containing it (even on the lines that the tags aren't on),
    but the application probably doesn't want that extra whitespace. So,
    stripIndent and withoutIndent attempt to intelligently strip off the leading
    whitespace.

    Whitespace characters are stripped from the start of the first line, and
    then those same number of whitespace characters are stripped from the
    beginning of each subsequent line (or up to the first non-whitespace
    character). If there is no whitespace at the start of the range,
    then nothing will be stripped.

    For these functions, $(D_STRING ' '), $(D_STRING '\t'), and $(D_STRING '\r')
    are considered whitespace.

    If the first line has no leading whitespace, then the leading whitespace on
    the second line is treated as the indent. If it has no leading whitespace,
    then no whitespace is stripped.

    So, if the text is well-formatted, then the indent should be cleanly
    removed, and if it's unformatted or badly formatted, then no characters
    other than leading whitespace will be removed, and in principle, no real
    data will have been lost - though of course, it's up to the programmer to
    decide whether it's better for the application to try to cleanly strip the
    indent or to leave the text as-is.

    The difference between stripIndent and withoutIndent is that stripIndent
    returns a $(D string), whereas withoutIndent returns a lazy range of code
    units. In the case where a $(D string) is passed to stripIndent, it will
    simply return the original string if the indent is determined to be zero
    (whereas in other cases, stripIndent and withoutIndent are forced to return
    new ranges).

    Returns: The text with the indent stripped from each line. stripIndent
             returns a $(D string), whereas withoutIndent returns a lazy range
             of code units (so it could be a range of $(D char) or $(D wchar)
             and not just $(D dchar) - which it is depends on the code units of
             the range being passed in).

    See_Also: $(REF EntityRange.Entity.text, dxml, parser, stax)
  +/
string stripIndent(R)(R range)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    static if(isDynamicArray!R && is(Unqual!(ElementEncodingType!R) == char))
    {
        static bool notHWhite(char c)
        {
            switch(c)
            {
                case ' ':
                case '\t':
                case '\r': return false;
                default : return true;
            }
        }

        import std.algorithm.searching : find;
        import std.utf : byCodeUnit;

        if(range.empty)
            return range;

        auto orig = range.save;
        auto text = range.byCodeUnit();
        string firstLine;

        if(notHWhite(text.front))
        {
            text = text.find('\n');
            if(text.empty)
                return orig;
            text.popFront();
            firstLine = orig[0 .. orig.length - text.length];
        }

        immutable beforeIndent = text.length;
        text = text.find!notHWhite();
        if(text.empty)
            return text.source;
        immutable indent = beforeIndent - text.length;

        if(indent == 0)
            return orig;

        import std.array : appender;
        auto retval = appender!string();
        retval.reserve(orig.length / 3);

        // > 1 because we don't want a newline by itself.
        if(firstLine.length > 1)
            put(retval, firstLine);

        outer: while(true)
        {
            auto start = text.save;
            text = text.find('\n');
            if(text.empty)
            {
                if(!start.empty)
                    put(retval, start);
                return retval.data;
            }
            text.popFront();
            auto line = start[0 .. $ - text.length];
            foreach(_; 0 .. indent)
            {
                if(text.empty)
                    goto isEmpty;
                if(notHWhite(text.front))
                    goto notEmpty;
                text.popFront();
            }
            if(text.empty)
            {
                isEmpty: put(retval, line[0 .. $ - 1]);
                return retval.data;
            }
            notEmpty: put(retval, line);
        }
        // The compiler is not smart enough to realize that this line is unreachable.
        assert(0);
    }
    else
    {
        import std.conv : to;
        return range.withoutIndent().to!string();
    }
}

///
auto withoutIndent(R)(R range)
    if(isForwardRange!R && isSomeChar!(ElementType!R))
{
    import std.utf : byCodeUnit;

    static struct WithoutIndent
    {
    public:

        @property empty() { return _line.empty; }

        @property front() { return _line.front; }

        void popFront()
        {
            if(_indent == 0)
            {
                _line.popFront();
                return;
            }

            if(_line.front == '\n')
                _nextLine();
            else
                _line.popFront();
            // Skip last newline
            if(_range.empty && !_line.empty && _line.front == '\n')
                _line = _range;
        }

        @property save()
        {
            auto retval = this;
            retval._line = _line.save;
            retval._range = _range.save;
            return retval;
        }

    private:

        static bool notHWhite(ElementEncodingType!R c)
        {
            switch(c)
            {
                case ' ':
                case '\t':
                case '\r': return false;
                default : return true;
            }
        }

        void _nextLine()
        {
            import std.algorithm.searching : find;
            _line = _range.save;
            _range = _range.find('\n');
            if(_range.empty)
                return;
            _range.popFront();
            _popIndent();
        }

        void _popIndent()
        {
            foreach(_; 0 .. _indent)
            {
                if(_range.empty)
                    return;
                if(notHWhite(_range.front))
                    return;
                _range.popFront();
            }
        }

        this(R range)
        {
            import std.algorithm : countUntil, find;
            import std.range : popFrontN;

            _range = byCodeUnit(range);
            if(_range.empty)
            {
                _line = _range;
                return;
            }

            auto orig = _range.save;
            immutable noFirstIndent = notHWhite(_range.front);
            if(noFirstIndent)
            {
                _range = _range.find('\n');
                if(_range.empty)
                    goto noIndent;
                _range.popFront();
            }

            _indent = _range.save.countUntil!(a => notHWhite(a))();
            if(_indent == 0)
            {
                noIndent: _line = orig;
                return;
            }
            if(noFirstIndent && orig.front != '\n')
            {
                _range = orig;
                _popIndent();
            }
            else
                _range.popFrontN(_indent);
            _nextLine();
        }

        typeof(byCodeUnit(R.init)) _range;
        typeof(byCodeUnit(R.init)) _line;
        size_t _indent;
    }

    return WithoutIndent(range);
}

///
unittest
{
    import std.algorithm.comparison : equal;

    // The prime use case for these two functions is for an Entity.text section
    // that is formatted to be human-readable, and the rules of what whitespace
    // is stripped from the beginning or end of the range are geared towards
    // the text coming from a well-formatted Entity.text section.
    {
        import dxml.parser.stax;
        auto xml = "<root>\n" ~
                   "    <code>\n" ~
                   "    bool isASCII(string str)\n" ~
                   "    {\n" ~
                   "        import std.algorithm : all;\n" ~
                   "        import std.ascii : isASCII;\n" ~
                   "        return str.all!isASCII();\n" ~
                   "    }\n" ~
                   "    </code>\n" ~
                   "<root>";
        auto range = parseXML(xml);
        range.popFront();
        range.popFront();
        assert(range.front.text ==
               "\n" ~
               "    bool isASCII(string str)\n" ~
               "    {\n" ~
               "        import std.algorithm : all;\n" ~
               "        import std.ascii : isASCII;\n" ~
               "        return str.all!isASCII();\n" ~
               "    }\n" ~
               "    ");
        assert(range.front.text.stripIndent() ==
               "bool isASCII(string str)\n" ~
               "{\n" ~
               "    import std.algorithm : all;\n" ~
               "    import std.ascii : isASCII;\n" ~
               "    return str.all!isASCII();\n" ~
               "}");
    }

    // The indent that is stripped matches the amount of whitespace at the front
    // of the first line.
    assert(("    start\n" ~
            "    foo\n" ~
            "    bar\n" ~
            "        baz\n" ~
            "        xyzzy\n" ~
            "           ").stripIndent() ==
           "start\n" ~
           "foo\n" ~
           "bar\n" ~
           "    baz\n" ~
           "    xyzzy\n" ~
           "       ");

    // If the first has no leading whitespace but the second line does, then
    // the second line's leading whitespace is treated as the indent.
    assert(("foo\n" ~
            "    bar\n" ~
            "        baz\n" ~
            "        xyzzy").stripIndent() ==
           "foo\n" ~
           "bar\n" ~
           "    baz\n" ~
           "    xyzzy");

    assert(("\n" ~
            "    foo\n" ~
            "    bar\n" ~
            "        baz\n" ~
            "        xyzzy").stripIndent() ==
           "foo\n" ~
           "bar\n" ~
           "    baz\n" ~
           "    xyzzy");

    // If neither of the first two lines has leading whitespace, then nothing
    // is stripped.
    assert(("foo\n" ~
            "bar\n" ~
            "    baz\n" ~
            "    xyzzy\n" ~
            "    ").stripIndent() ==
           "foo\n" ~
           "bar\n" ~
           "    baz\n" ~
           "    xyzzy\n" ~
           "    ");

    // If a subsequent line starts with less whitespace than the indent, then
    // all of its leading whitespace is stripped but no other characters are
    // stripped.
    assert(("      foo\n" ~
            "         bar\n" ~
            "   baz\n" ~
            "         xyzzy").stripIndent() ==
           "foo\n" ~
           "   bar\n" ~
           "baz\n" ~
           "   xyzzy");

    // If the last line is just the indent, then it and the newline before it
    // are stripped.
    assert(("    foo\n" ~
            "       bar\n" ~
            "    ").stripIndent() ==
           "foo\n" ~
           "   bar");

    // If the last line is just whitespace, but it's more than the indent, then
    // the whitespace after the indent is kept.
    assert(("    foo\n" ~
            "       bar\n" ~
            "       ").stripIndent() ==
           "foo\n" ~
           "   bar\n" ~
           "   ");

    // withoutIndent does the same as stripIndent but with a lazy range.
    assert(equal(("  foo\n" ~
                  "    bar\n" ~
                  "    baz\n").withoutIndent(),
                 "foo\n" ~
                 "  bar\n" ~
                 "  baz"));
}

unittest
{
    import core.exception : AssertError;
    import std.algorithm.comparison : equal;
    import std.exception : enforce;
    import std.utf : byUTF;
    import dxml.internal : testRangeFuncs;

    static void test(alias func)(string text, string expected, size_t line = __LINE__)
    {
        auto range = func(text);
        enforce!AssertError(range.save.stripIndent() == expected, "unittest failed 1", __FILE__, line);
        alias C = ElementType!(typeof(range.save.withoutIndent()));
        enforce!AssertError(equal(range.save.withoutIndent(), expected.byUTF!C), "unittest failed 2", __FILE__, line);
    }

    static foreach(func; testRangeFuncs)
    {
        test!func("", "");
        test!func("     ", "");
        test!func("foo", "foo");
        test!func("\nfoo", "\nfoo");
        test!func("    foo", "foo");
        test!func("\n    foo", "foo");
        test!func("\n    foo\n", "foo");
        test!func("\n    foo\n    ", "foo");
        test!func("\n    foo\n     ", "foo\n ");

        test!func("  foo\n  bar  \n    baz", "foo\nbar  \n  baz");
        test!func("  foo\nbar\n  baz", "foo\nbar\nbaz");
        test!func("  foo\n bar\n  baz", "foo\nbar\nbaz");
        test!func("  foo\n  bar\n  baz", "foo\nbar\nbaz");
        test!func("  foo\n   bar\n  baz", "foo\n bar\nbaz");
        test!func("  foo\n    bar\n  baz", "foo\n  bar\nbaz");
        test!func("  foo\n     bar\n  baz", "foo\n   bar\nbaz");
        test!func("  foo\n     bar\n  baz\n\n\n\n\n", "foo\n   bar\nbaz\n\n\n\n");

        test!func("     foo\n  bar\n       baz", "foo\nbar\n  baz");

        test!func("foo\n     bar\n      baz", "foo\nbar\n baz");
        test!func("foo\nbar\n      baz\n", "foo\nbar\n      baz\n");
    }
}

@safe pure unittest
{
    import std.algorithm.comparison : equal;
    import dxml.internal : testRangeFuncs;

    static foreach(func; testRangeFuncs)
    {{
        assert(stripIndent(func("foo")) == "foo");
        assert(equal(withoutIndent(func("foo")), "foo"));
    }}
}



package(dxml):

// Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
bool isXMLChar(dchar c) pure nothrow @safe @nogc
{
    // The rule says '\n' and not '\r', but we're not going to pass '\n' to
    // this function, because it has to be handled separately for keeping track
    // of SourcePos. Look at the documentation for EntityRange for the
    // explanation of how we're treating '\r' and why.
    import std.ascii : isASCII;
    assert(c != '\n');
    return isASCII(c) ? c >= ' ' || c == '\t' || c == '\r'
                      : c > 127 && (c <= 0xD7FF || (c >= 0xE000 && c <= 0xFFFD) || (c >= 0x10000 && c <= 0x10FFFF));
}

pure nothrow @safe @nogc unittest
{
    import std.range : only;
    import std.typecons : tuple;

    foreach(c; char.min .. ' ')
    {
        if(c == '\n')
            continue;
        if(c == ' ' || c == '\t' || c == '\r')
            assert(isXMLChar(c));
        else
            assert(!isXMLChar(c));
    }
    foreach(dchar c; ' ' .. 256)
        assert(isXMLChar(c));

    assert(isXMLChar(0xD7FF - 1));
    assert(isXMLChar(0xD7FF));
    assert(!isXMLChar(0xD7FF + 1));

    foreach(t; only(tuple(0xE000, 0xFFFD),
                    tuple(0x10000, 0x10FFFF)))
    {
        assert(!isXMLChar(t[0] - 1));
        assert(isXMLChar(t[0]));
        assert(isXMLChar(t[0] + 1));
        assert(isXMLChar(t[0] + (t[1] - t[0])));
        assert(isXMLChar(t[1] - 1));
        assert(isXMLChar(t[1]));
        assert(!isXMLChar(t[1] + 1));
    }
}
