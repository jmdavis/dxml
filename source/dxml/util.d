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

    Similarly, an application can choose to encode any character as a character
    reference (e.g. $(D_STRING '&#65") or $(D_STRING '&#x40") for
    $(D_STRING 'A')). normalize will decode such character references to their
    corresponding character. However, it does not handle any parsed entity
    references or any entity references beyond the five listed below (handling
    any others would require handling the DTD section).

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

    Other types of references (such as entity references) are left untouched,
    and any $(D_STRING '&') which is not used in one of the constructs listed in
    the table as well as any malformed constructs (e.g. $(D_STRING "&Amp;" or
    $(D_STRING &#xGGA2;") are left untouched.

    The difference between normalize and asNormalized is that normalize returns
    a $(D string), whereas asNormalized returns a lazy range. In the case where
    a $(D string) is passed to normalize, it will simply return the original
    string if there is no text to normalize (whereas in other cases, normalize
    and asNormalized are forced to return new ranges even if there is no
    un-normalized text).

    Returns: The normalized text. normalize returns a $(D string), whereas
             asNormalized returns a lazy range.

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
            _front = _buffer[0 .. _front.length];
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
                                _front = _buffer[0 .. _buffer.encode!(UseReplacementDchar.yes)(c)];
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

        this(R range)
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

    static void test(alias func)(string text, string expected, size_t line = __LINE__)
    {
        auto range = func(text);
        enforce!AssertError(range.save.normalize() == expected, "unittest failed 1", __FILE__, line);
        enforce!AssertError(equal(range.save.asNormalized(), expected), "unittest failed 2", __FILE__, line);
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
        test!func("&#0;", "\0");
        test!func("&#0", "&#0");
        test!func("&#0amp;", "&#0amp;");
        test!func("&#amp;", "&#amp;");
        test!func("&#x", "&#x");
        test!func("&#x;", "&#x;");
        test!func("&#x0;", "\0");
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
    import std.algorithm : equal;

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
    import std.utf : byCodeUnit, isValidDchar, nullable;

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
            if(!isValidDchar(c))
                goto invalid;
            if(!cuRange.startsWith(';'))
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
    import std.algorithm : equal;

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
        import dxml.internal : fwdCharRange, fwdRefCharRange, raCharRange, rasCharRange, rasRefCharRange;
        alias _testRangeFuncs = AliasSeq!(a => to!string(a), a => to!wstring(a), a => to!dstring(a),
                                          a => filter!"true"(a), a => fwdCharRange(a), a => fwdRefCharRange(a),
                                          a => raCharRange(a), a => rasCharRange(a), a => rasRefCharRange(a),
                                          a => byCodeUnit(a));
    }
}
