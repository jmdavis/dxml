// Written in the D programming language

/++
    This module provides functionality for creating XML documents.

    Copyright: Copyright 2018
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTPS jmdavisprog.com, Jonathan M Davis)
    Source:    $(LINK_TO_SRC dxml/_writer.d)

    See_Also: $(LINK2 http://www.w3.org/TR/REC-xml/, Official Specification for XML 1.0)
  +/
module dxml.writer;

import std.range.primitives;
import std.traits;
import std.typecons : Flag;


/++
    Exception thrown when the writer is given data that would result in invalid
    XML.
  +/
class XMLWritingException : Exception
{
private:

    this(string msg, string file = __FILE__, size_t line = __LINE__) @safe pure nothrow @nogc
    {
        super(msg, file, line);
    }
}


/++
    $(PHOBOS_REF Flag, std, typecons) indicating whether
    $(LREF2 closeStartTag, XMLWriter.closeStartTag) or
    $(LREF2 writeStartTag, XMLWriter.writeStartTag) or
    will write an empty element tag (which then does not require a corresponding
    end tag).
  +/
alias EmptyTag = Flag!"EmptyTag";


/++
    $(PHOBOS_REF Flag, std, typecons) indicating whether a write function of
    $(LREF XMLWriter) will write a newline followed by an indent before the
    entity being written.
  +/
alias Newline = Flag!"Newline";


/++
    $(PHOBOS_REF Flag, std, typecons) indicating whether a write function of
    $(LREF XMLWriter) which accepts text which may include newlines will write
    an indent after each newline is written.
  +/
alias InsertIndent = Flag!"InsertIndent";


/++
    Writes XML to an output range of characters.

    Note that default initialization and copying are disabled for XMLWriter.
    This is because XMLWriter is essentially a reference type, but in many
    cases, it doesn't need to be passed around and thus can simply be
    constructed on the stack. So, it's a struct with default initialization and
    copying disabled so that its will be treated like a reference type, and code
    that needs to pass it around can pass it by ref or allocate it on the heap
    and pass a pointer to it around.

    The optional $(LREF Newline) and $(LREF InsertIndent) parameters to the
    various write functions are used to control the formatting of the XML, and
    $(LREF2 writeIndent, XMLWriter.writeIndent) and
    $(LREF2 output, XMLWriter.output) can be used for additional control over
    the formatting.

    The indent provided to the XMLWriter is the base indent that will be used
    whenever $(LREF2 writeIndent, XMLWriter.writeIndent) and any write
    functions using $(D Newline.yes) or $(D InsertIndent.yes) are called - e.g.
    if the base indent is 4 spaces,
    $(D $(LREF2 tagDepth, XMLWriter.tagDepth) == 3), and
    $(D Newline.yes) is passed to a writeComment, then a newline followed by
    12 spaces will be written to the output range after the comment.

    $(LREF writeXMLDecl) can be used to write the $(D <?xml...?>) declaration
    to the output range before constructing an XML writer, but if an application
    wishes to do anything with a DTD section, it will have to write that to the
    output range on its own before constructing the XMLWriter. XMLWriter expects
    to start writing XML after any $(D <?xml...?>) or $(D <!DOCTYPE...>)
    declarations.

    The write functions check the arguments prior to writing anything to the
    output range, so the XMLWriter is not in an invalid state after an
    $(LREF XMLWritingException is thrown), but is $(I is) in an invalid state
    if any other exception is thrown (which will only occur if an input range
    that is passed to a write function throws or if the ouput range throws).

    Params:
        output = The output range that the XML will be written to.
        baseIndent = Optional argument indicating the base indent to be used
                     when an indent is inserted after a newline in the XML (with
                     the actual indent being the base indent inserted once for
                     each level of the $(LREF2 tagDepth, XMLWriter.tagDepth).
                     The default is four spaces. The indent may only contain
                     spaces and/or tabs. If the indent is empty, then the
                     XMLWriter will never insert newlines or indents such that
                     $(D Newline.yes) will be the same as $(D Newline.no), and
                     $(D InsertIndent.yes) will be the same as
                     $(D InsertIndent.no).

    See_Also: $(LREF writeXMLDecl)$(BR)
              $(REF encodeAttr, dxml, util)$(BR)
              $(REF encodeText, dxml, util)$(BR)
              $(REF StdEntityRef, dxml, util)$(BR)
              $(REF toCharRef, dxml, util)$(BR)
  +/
struct XMLWriter(OR)
    if(isOutputRange!(OR, char))
{
    import std.range.primitives;
    import std.traits;

    enum compileInTests = is(OR == XMLWriterCompileTests);

public:


    /++
        Writes the first portion of a start tag to the given output range.

        Once openStartTag has been called,
        $(LREF2 writeAttr, XMLWriter.writeAttr) can be called to add attributes
        to the start tag.: $(LREF2 closeStartTag, XMLWriter.closeStartTag)
        writes the closing portion of the start tag.

        Once openStartTag has been called, it is an error to call any
        function on XMLWriter other than
        $(LREF2 closeStartTag, XMLWriter.closeStartTag),
        $(LREF2 writeAttr, XMLWriter.writeAttr),
        $(LREF2 writeIndent, XMLWriter.writeIndent),
        $(LREF2 tagDepth, XMLWriter.tagDepth), or
        $(LREF2 baseIndent, XMLWriter.baseIndent), or
        $(LREF2 output, XMLWriter.output) until
        $(LREF2 closeStartTag, XMLWriter.closeStartTag) has been called.

        It is also an error to call openStartTag after the end tag for the root
        element has been written.

        Params:
            name = The name of the start tag.
            newline = Whether a newline followed by an indent will be written to
                      the output range before the start tag's name.

        Throws: $(LREF XMLWritingException) if the name is not a valid XML name.

        See_Also: $(LREF2 writeStartTag, XMLWriter.writeStartTag)$(BR)
                  $(LREF2 writeAttr, XMLWriter.writeAttr)$(BR)
                  $(LREF2 closeStratTag, XMLWriter.closeStartTag)$(BR)
                  $(LREF2 writeEndTag, XMLWriter.writeEndTag)
      +/
    void openStartTag(string name, Newline newline = Newline.yes)
    {
        assert(!_startTagOpen, "openStartTag cannot be called when a start tag is already open");
        assert(!_writtenRootEnd, "openStartTag cannot be called after the root element's end tag has been written.");
        checkName(name);
        if(newline == Newline.yes && !_baseIndent.empty)
            put(_output, _getIndent(tagDepth));
        _startTagOpen = true;
        _incLevel(name);
        put(_output, '<');
        put(_output, name);
    }

    ///
    static if(compileInTests) unittest
    {
        import std.array : appender;
        import std.exception : assertThrown;

        auto writer = xmlWriter(appender!string());

        writer.openStartTag("root", Newline.no);
        assert(writer.output.data == "<root");

        writer.closeStartTag();
        assert(writer.output.data == "<root>");

        assertThrown!XMLWritingException(writer.openStartTag("<tag>"));
        assert(writer.output.data == "<root>");

        writer.openStartTag("foo");
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo");

        writer.writeAttr("answer", "42");
        assert(writer.output.data ==
               "<root>\n" ~
               `    <foo answer="42"`);

        writer.closeStartTag(EmptyTag.yes);
        assert(writer.output.data ==
               "<root>\n" ~
               `    <foo answer="42"/>`);

        writer.writeEndTag();
        assert(writer.output.data ==
               "<root>\n" ~
               `    <foo answer="42"/>` ~ "\n" ~
               "</root>");
    }

    static if(compileInTests) @safe pure unittest
    {
        import dxml.internal : TestAttrOR;
        auto writer = xmlWriter(TestAttrOR.init);
        writer.openStartTag("root");
    }


    /++
        Writes an attribute for a start tag to the output range.

        It is an error to call writeAttr except between calls to
        $(LREF2 openStartTag, XMLWriter.openStartTag) and
        $(LREF2 closeStartTag, XMLWriter.closeStartTag).

        Params:
            quote = The quote character to use for the attribute value's
                    delimiter.
            name = The name of the attribute.
            value = The value of the attribute.
            newline = Whether a newline followed by an indent will be written to
                      the output range before the attribute.

        Throws: $(LREF XMLWritingException) if the name is not a valid XML name,
                if the value is not a valid XML value, or if the given name has
                already been written to the current start tag.

        See_Also: $(REF encodeAttr, dxml, util)$(BR)
                  $(REF StdEntityRef, dxml, util)$(BR)
                  $(REF toCharRef, dxml, util)
      +/
    void writeAttr(char quote = '"', R)(string name, R value, Newline newline = Newline.no)
        if((quote == '"' || quote == '\'') &&
           isForwardRange!R && isSomeChar!(ElementType!R))
    {
        assert(_startTagOpen, "writeAttr cannot be called except when a start tag is open");

        checkName(name.save);
        static if(quote == '"')
            checkText!(CheckText.attValueQuot)(value.save);
        else
            checkText!(CheckText.attValueApos)(value.save);

        import std.algorithm.searching : canFind;
        if(_attributes.canFind(name))
            throw new XMLWritingException("Duplicate attribute name: " ~ name);
        _attributes ~= name;

        if(newline == Newline.yes && !_baseIndent.empty)
            put(_output, _getIndent(tagDepth));
        else
            put(_output, ' ');
        put(_output, name);
        put(_output, "=" ~ quote);
        put(_output, value);
        put(_output, quote);
    }

    ///
    static if(compileInTests) unittest
    {
        import std.array : appender;
        import std.exception : assertThrown;
        import dxml.util : encodeAttr;

        auto writer = xmlWriter(appender!string());

        writer.openStartTag("root", Newline.no);
        assert(writer.output.data == "<root");

        writer.writeAttr("a", "one");
        assert(writer.output.data == `<root a="one"`);

        writer.writeAttr("b", "two");
        assert(writer.output.data == `<root a="one" b="two"`);

        // It's illegal for two attributes on the same start tag
        // to have the same name.
        assertThrown!XMLWritingException(writer.writeAttr("a", "three"));

        // Invalid name
        assertThrown!XMLWritingException(writer.writeAttr("=", "value"));

        // Can't have a quote that matches the enclosing quote.
        assertThrown!XMLWritingException(writer.writeAttr("c", `foo"bar`));

        // Unchanged after an XMLWritingException is thrown.
        assert(writer.output.data == `<root a="one" b="two"`);

        writer.closeStartTag();
        assert(writer.output.data == `<root a="one" b="two">`);

        writer.openStartTag("foobar");
        assert(writer.output.data ==
               `<root a="one" b="two">` ~ "\n" ~
               "    <foobar");

        // " is the default for the quote character, but ' can be specified.
        writer.writeAttr!'\''("answer", "42");
        assert(writer.output.data ==
               `<root a="one" b="two">` ~ "\n" ~
               "    <foobar answer='42'");

        writer.writeAttr("base", "13", Newline.yes);
        assert(writer.output.data ==
               `<root a="one" b="two">` ~ "\n" ~
               "    <foobar answer='42'\n" ~
               `        base="13"`);

        writer.closeStartTag();
        assert(writer.output.data ==
               `<root a="one" b="two">` ~ "\n" ~
               "    <foobar answer='42'\n" ~
               `        base="13">`);

        writer.openStartTag("tag");
        assert(writer.output.data ==
               `<root a="one" b="two">` ~ "\n" ~
               "    <foobar answer='42'\n" ~
               `        base="13">` ~ "\n" ~
               "        <tag");

        // &, <, and > are not legal in an attribute value.
        assertThrown!XMLWritingException(writer.writeAttr("foo", "&"));

        // Unchanged after an XMLWritingException is thrown.
        assert(writer.output.data ==
               `<root a="one" b="two">` ~ "\n" ~
               "    <foobar answer='42'\n" ~
               `        base="13">` ~ "\n" ~
               "        <tag");

        // Use dxml.util.encodeAttr to encode characters that aren't
        // legal in an attribute value but can legally be encoded.
        writer.writeAttr("foo", encodeAttr("&"));
        assert(writer.output.data ==
               `<root a="one" b="two">` ~ "\n" ~
               "    <foobar answer='42'\n" ~
               `        base="13">` ~ "\n" ~
               `        <tag foo="&amp;"`);

        writer.closeStartTag(EmptyTag.yes);
        assert(writer.output.data ==
               `<root a="one" b="two">` ~ "\n" ~
               "    <foobar answer='42'\n" ~
               `        base="13">` ~ "\n" ~
               `        <tag foo="&amp;"/>`);

        writer.writeEndTag();
        writer.writeEndTag();
        assert(writer.output.data ==
               `<root a="one" b="two">` ~ "\n" ~
               "    <foobar answer='42'\n" ~
               `        base="13">` ~ "\n" ~
               `        <tag foo="&amp;"/>` ~ "\n" ~
               "    </foobar>\n" ~
               "</root>");
    }

    static if(compileInTests) @safe pure unittest
    {
        import dxml.internal : TestAttrOR;
        auto writer = xmlWriter(TestAttrOR.init);
        writer.openStartTag("root");
        writer.writeAttr("attr", "42");
    }


    /++
        Writes the end of a start tag to the ouput range.

        It is an error to call closeStartTag unless a start tag has been opened
        and not yet closed.

        Params:
            emptyTag = Whether the start tag will be empty (i.e. terminated with
                       $(D_CODE_STRING '/>') so that there is no corresponding
                       end tag).

        See_Also: $(LREF2 openStartTag, XMLWriter.openStartTag)$(BR)
                  $(LREF2 writeAttr, XMLWriter.writeAttr)$(BR)
                  $(LREF2 writeStartTag, XMLWriter.writeStartTag)$(BR)
                  $(LREF2 writeEndTag, XMLWriter.writeEndTag)
      +/
    void closeStartTag(EmptyTag emptyTag = EmptyTag.no)
    {
        assert(_startTagOpen, "closeStartTag cannot be called when a start tag is not open");
        if(emptyTag == EmptyTag.yes)
        {
            put(_output, "/>");
            _decLevel();
        }
        else
            put(_output, '>');
        _startTagOpen = false;
        _attributes.length = 0;
        () @trusted { _attributes.assumeSafeAppend(); } ();
    }

    ///
    static if(compileInTests) unittest
    {
        import std.array : appender;

        auto writer = xmlWriter(appender!string());

        writer.openStartTag("root", Newline.no);
        assert(writer.output.data == "<root");

        writer.closeStartTag();
        assert(writer.output.data == "<root>");

        writer.openStartTag("foo");
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo");

        writer.closeStartTag(EmptyTag.yes);
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo/>");

        writer.writeEndTag();
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo/>\n" ~
               "</root>");
    }

    // _decLevel currently can't be pure.
    static if(compileInTests) @safe /+pure+/ unittest
    {
        import dxml.internal : TestAttrOR;
        auto writer = xmlWriter(TestAttrOR.init);
        writer.openStartTag("root");
        writer.closeStartTag();
    }


    /++
        Writes a start tag with no attributes.

        This is equivalent to call $(LREF2 openStartTag, XMLWriter.openStartTag)
        immediately followed by $(LREF2 closeStartTag, XMLWriter.closeStartTag).

        It is also an error to call writeStartTag after the end tag for the root
        element has been written.

        Params:
            name = The name of the start tag.
            emptyTag = Whether the start tag will be empty (i.e. terminated with
                       $(D_CODE_STRING '/>') so that there is no corresponding
                       end tag).
            newline = Whether a newline followed by an indent will be written to
                      the output range before the start tag.

        Throws: $(LREF XMLWritingException) if the name is not a valid XML name.

        See_Also: $(LREF2 openStartTag, XMLWriter.openStartTag)$(BR)
                  $(LREF2 writeAttr, XMLWriter.writeAttr)$(BR)
                  $(LREF2 closeStartTag, XMLWriter.closeStartTag)$(BR)
                  $(LREF2 writeEndTag, XMLWriter.writeEndTag)
      +/
    void writeStartTag(string name, EmptyTag emptyTag = EmptyTag.no, Newline newline = Newline.yes)
    {
        assert(!_startTagOpen, "writeStartTag cannot be called when a start tag is open");
        assert(!_writtenRootEnd, "writeStartTag cannot be called after the root element's end tag has been written.");
        checkName(name);
        if(newline == Newline.yes && !_baseIndent.empty)
            put(_output, _getIndent(tagDepth));
        put(_output, '<');
        put(_output, name);
        if(emptyTag == EmptyTag.yes)
            put(_output, "/>");
        else
        {
            _incLevel(name);
            put(_output, '>');
        }
    }

    /// Ditto
    void writeStartTag(R)(R name, Newline newline, EmptyTag emptyTag = EmptyTag.no)
    {
        writeStartTag(name, emptyTag, newline);
    }

    ///
    static if(compileInTests) unittest
    {
        import std.array : appender;
        import std.exception : assertThrown;

        auto writer = xmlWriter(appender!string());
        writer.writeStartTag("root", Newline.no);
        assert(writer.output.data == "<root>");

        writer.writeStartTag("foo");
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo>");

        assertThrown!XMLWritingException(writer.writeStartTag("="));
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo>");

        writer.writeStartTag("bar", EmptyTag.yes);
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo>\n" ~
               "        <bar/>");

        writer.writeStartTag("baz", EmptyTag.yes, Newline.no);
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo>\n" ~
               "        <bar/><baz/>");

        writer.writeStartTag("bloop");
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo>\n" ~
               "        <bar/><baz/>\n" ~
               "        <bloop>");

        writer.writeEndTag();
        writer.writeEndTag();
        writer.writeEndTag();
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo>\n" ~
               "        <bar/><baz/>\n" ~
               "        <bloop>\n" ~
               "        </bloop>\n" ~
               "    </foo>\n" ~
               "</root>");
    }

    static if(compileInTests) @safe pure unittest
    {
        import dxml.internal : TestAttrOR;
        auto writer = xmlWriter(TestAttrOR.init);
        writer.writeStartTag("root");
    }


    /++
        Writes an end tag to the output range with the name of whichever start
        tag is waiting for a matching end tag.

        If a name is provided, then it will be validated against the matching
        start tag.

        Param
            name = Name to check against the matching start tag.
            newline = Whether a newline followed by an indent will be written to
                      the output range before the end tag.

        Throws: $(LREF XMLWritingException) if no start tag is waiting for a
                matching end tag or if the given name does not match the name
                of the start tag waiting for an end tag.
      +/
    void writeEndTag(string name, Newline newline = Newline.yes)
    {
        assert(!_startTagOpen, "writeEndTag cannot be called when a start tag is open");

        if(name != _tagStack.back)
        {
            import std.format : format;
            auto msg = format!"End tag name does not match start tag name: <%s> vs </%s>"(_tagStack.back, name);
            throw new XMLWritingException(msg);
        }

        writeEndTag(newline);
    }

    /// Ditto
    void writeEndTag(Newline newline = Newline.yes)
    {
        assert(!_startTagOpen, "writeEndTag cannot be called when a start tag is open");

        immutable name = _tagStack.back;
        _decLevel();
        if(newline == Newline.yes && !_baseIndent.empty)
            put(_output, _getIndent(tagDepth));
        put(_output, "</");
        put(_output, name);
        put(_output, ">");

        version(assert)
            _writtenRootEnd = tagDepth == 0;
    }

    ///
    static if(compileInTests) unittest
    {
        import std.array : appender;
        import std.exception : assertThrown;

        auto writer = xmlWriter(appender!string());
        writer.writeStartTag("root", Newline.no);
        assert(writer.output.data == "<root>");

        writer.writeStartTag("foo");
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo>");

        // Name doesn't match start tag, which is <foo>
        assertThrown!XMLWritingException(writer.writeEndTag("bar"));
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo>");

        writer.writeEndTag("foo", Newline.no);
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo></foo>");

        writer.writeStartTag("bar");
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo></foo>\n" ~
               "    <bar>");

        writer.writeEndTag("bar");
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo></foo>\n" ~
               "    <bar>\n" ~
               "    </bar>");

        writer.writeEndTag();
        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo></foo>\n" ~
               "    <bar>\n" ~
               "    </bar>\n" ~
               "</root>");
    }

    // _decLevel currently can't be pure.
    static if(compileInTests) @safe /+pure+/ unittest
    {
        import dxml.internal : TestAttrOR;
        auto writer = xmlWriter(TestAttrOR.init);
        writer.writeStartTag("root");
        writer.writeStartTag("tag");
        writer.writeEndTag("tag");
        () @safe nothrow { writer.writeEndTag(); } ();
    }


    /++
        This writes the text that goes between start tags and end tags.

        It can be called multiple times in a row, and the given text will just
        end up being appended to the current text field.

        It is also an error to call writeText after the end tag for the root
        element has been written.

        Param:
            text = The text to write.
            newline = Whether a newline followed by an indent will be written to
                      the output range before the text. It will not include an
                      indent if $(D insertIndent == InsertIndent.no).
            insertIndent = Whether an indent will be inserted after each newline
                           within the text.

        Throws: $(LREF XMLWritingException) if the given text is not legal in
                an XML document.

        See_Also: $(LREF writeTaggedText)$(BR)
                  $(REF encodeText, dxml, util)$(BR)
                  $(REF StdEntityRef, dxml, util)$(BR)
                  $(REF toCharRef, dxml, util)$(BR)
      +/
    void writeText(R)(R text, Newline newline = Newline.yes, InsertIndent insertIndent = InsertIndent.yes)
        if(isForwardRange!R && isSomeChar!(ElementType!R))
    {
        assert(!_startTagOpen, "writeText cannot be called when a start tag is open");
        assert(!_writtenRootEnd, "writeText cannot be called after the root end tag has been written");
        assert(tagDepth != 0, "writeText cannot be called before the root start tag has been written");
        checkText!(CheckText.text)(text);
        if(newline == Newline.yes && !_baseIndent.empty)
            put(_output, insertIndent == InsertIndent.yes ? _getIndent(tagDepth) : "\n");
        if(insertIndent == InsertIndent.yes)
            _insertIndent(text, tagDepth);
        else
            put(_output, text);
    }

    /// Ditto
    void writeText(R)(R text, InsertIndent insertIndent, Newline newline = Newline.yes)
        if(isForwardRange!R && isSomeChar!(ElementType!R))
    {
        writeText(text, newline, insertIndent);
    }

    ///
    static if(compileInTests) unittest
    {
        import std.array : appender;
        import std.exception : assertThrown;
        import dxml.util : encodeText;

        {
            auto writer = xmlWriter(appender!string());
            writer.writeStartTag("root", Newline.no);
            writer.writeStartTag("foo");

            // By default, a newline is inserted before the text, and the text
            // is indented.
            writer.writeText("hello world");
            assert(writer.output.data ==
                   "<root>\n" ~
                   "    <foo>\n" ~
                   "        hello world");

            writer.writeEndTag("foo");
            assert(writer.output.data ==
                   "<root>\n" ~
                   "    <foo>\n" ~
                   "        hello world\n" ~
                   "    </foo>");

            writer.writeStartTag("foo");

            // With Newline.no, no newline is inserted prior to the text.
            writer.writeText("hello world", Newline.no);
            writer.writeEndTag("foo");
            assert(writer.output.data ==
                   "<root>\n" ~
                   "    <foo>\n" ~
                   "        hello world\n" ~
                   "    </foo>\n" ~
                   "    <foo>hello world\n" ~
                   "    </foo>");

            writer.writeStartTag("foo");
            writer.writeText("hello world", Newline.no);

            // Newline.no on the end tag also makes it so that there is no
            // newline after the text.
            writer.writeEndTag("foo", Newline.no);
            assert(writer.output.data ==
                   "<root>\n" ~
                   "    <foo>\n" ~
                   "        hello world\n" ~
                   "    </foo>\n" ~
                   "    <foo>hello world\n" ~
                   "    </foo>\n" ~
                   "    <foo>hello world</foo>");

        }
        {
            auto writer = xmlWriter(appender!string());
            writer.writeStartTag("root", Newline.no);
            writer.writeStartTag("bar");

            // By default, if there are newlines in the text, they are indented.
            writer.writeText("The dish\nran away\nwith the spoon");
            writer.writeEndTag("bar");
            assert(writer.output.data ==
                   "<root>\n" ~
                   "    <bar>\n" ~
                   "        The dish\n" ~
                   "        ran away\n" ~
                   "        with the spoon\n" ~
                   "    </bar>");
        }
        {
            auto writer = xmlWriter(appender!string());
            writer.writeStartTag("root", Newline.no);
            writer.writeStartTag("bar");

            // InsertIndent.no tells it to not indent each line.
            writer.writeText("The dish\nran away\nwith the spoon",
                             InsertIndent.no);
            writer.writeEndTag("bar");
            assert(writer.output.data ==
                   "<root>\n" ~
                   "    <bar>\n" ~
                   "The dish\n" ~
                   "ran away\n" ~
                   "with the spoon\n" ~
                   "    </bar>");
        }
        {
            auto writer = xmlWriter(appender!string());
            writer.writeStartTag("root", Newline.no);
            writer.writeStartTag("bar");

           // Of course, Newline.no and InsertIndent.no can be combined.
            writer.writeText("The dish\nran away\nwith the spoon",
                             Newline.no, InsertIndent.no);
            writer.writeEndTag("bar");
            assert(writer.output.data ==
                   "<root>\n" ~
                   "    <bar>The dish\n" ~
                   "ran away\n" ~
                   "with the spoon\n" ~
                   "    </bar>");
        }
        {
            auto writer = xmlWriter(appender!string());
            writer.writeStartTag("root", Newline.no);
            writer.writeStartTag("code");
            assert(writer.output.data ==
                   "<root>\n" ~
                   "    <code>");

            auto text = "if(--foo > bar && bar < baz)\n" ~
                        "    doSomething();";

            // &, <, and > are not legal in XML text.
            assertThrown!XMLWritingException(writer.writeText(text));

            // Unchanged after an XMLWritingException is thrown.
            assert(writer.output.data ==
                   "<root>\n" ~
                   "    <code>");

            // Use dxml.util.encodeText to encode characters that aren't
            // legal in a text field but can legally be encoded.
            writer.writeText(encodeText(text));
            writer.writeEndTag("code");
            assert(writer.output.data ==
                   "<root>\n" ~
                   "    <code>\n" ~
                   "        if(--foo &gt; bar &amp;&amp; bar &lt; baz)\n" ~
                   "            doSomething();\n" ~
                   "    </code>");
        }
    }

    static if(compileInTests) @safe pure unittest
    {
        import dxml.internal : TestAttrOR;
        auto writer = xmlWriter(TestAttrOR.init);
        writer.writeStartTag("root");
        writer.writeText("");
    }


    /++
        Writes a comment to the output range.

        Param:
            text = The text of the comment.
            newline = Whether a newline followed by an indent will be written to
                      the output range before the end tag.
            insertIndent = Whether an indent will be inserted after each newline
                           within the text.

        Throws: $(LREF XMLWritingException) if the given text is not legal in
                an XML document.
      +/
    void writeComment(R)(R text, Newline newline = Newline.yes, InsertIndent insertIndent = InsertIndent.yes)
        if(isForwardRange!R && isSomeChar!(ElementType!R))
    {
        assert(!_startTagOpen, "writeComment cannot be called when a start tag is open");
        checkText!(CheckText.comment)(text);
        if(newline == Newline.yes && !_baseIndent.empty)
            put(_output, _getIndent(tagDepth));
        put(_output, "<!--");
        if(insertIndent == InsertIndent.yes)
            _insertIndent(text, tagDepth + 1);
        else
            put(_output, text);
        put(_output, "-->");
    }

    /// Ditto
    void writeComment(R)(R text, InsertIndent insertIndent, Newline newline = Newline.yes)
        if(isForwardRange!R && isSomeChar!(ElementType!R))
    {
        writeComment(text, newline, insertIndent);
    }

    ///
    static if(compileInTests) unittest
    {
        import std.array : appender;
        import std.exception : assertThrown;

        auto writer = xmlWriter(appender!string());

        writer.writeComment(" And so it begins... ", Newline.no);
        writer.writeStartTag("root");
        writer.writeComment("A comment");
        writer.writeComment("Another comment");
        writer.writeComment("No preceding newline", Newline.no);
        writer.writeComment("A comment\nwith a newline");
        writer.writeComment("Another newline\nbut no indent",
                            InsertIndent.no);
        writer.writeStartTag("tag");
        writer.writeComment("Deeper comment");
        writer.writeEndTag("tag");
        writer.writeEndTag("root");
        writer.writeComment(" And so it ends... ");

        assert(writer.output.data ==
               "<!-- And so it begins... -->\n" ~
               "<root>\n" ~
               "    <!--A comment-->\n" ~
               "    <!--Another comment--><!--No preceding newline-->\n" ~
               "    <!--A comment\n" ~
               "        with a newline-->\n" ~
               "    <!--Another newline\n" ~
               "but no indent-->\n" ~
               "    <tag>\n" ~
               "        <!--Deeper comment-->\n" ~
               "    </tag>\n" ~
               "</root>\n" ~
               "<!-- And so it ends... -->");

        // -- is not legal in an XML comment.
        assertThrown!XMLWritingException(writer.writeComment("foo--bar"));

        // - is not legal at the end of an XML comment.
        assertThrown!XMLWritingException(writer.writeComment("foo-"));

        // Unchanged after an XMLWritingException is thrown.
        assert(writer.output.data ==
               "<!-- And so it begins... -->\n" ~
               "<root>\n" ~
               "    <!--A comment-->\n" ~
               "    <!--Another comment--><!--No preceding newline-->\n" ~
               "    <!--A comment\n" ~
               "        with a newline-->\n" ~
               "    <!--Another newline\n" ~
               "but no indent-->\n" ~
               "    <tag>\n" ~
               "        <!--Deeper comment-->\n" ~
               "    </tag>\n" ~
               "</root>\n" ~
               "<!-- And so it ends... -->");
    }

    static if(compileInTests) @safe pure unittest
    {
        import dxml.internal : TestAttrOR;
        auto writer = xmlWriter(TestAttrOR.init);
        writer.writeComment("");
    }


    /++
        Writes a $(D <![CDATA[...]]>) section with the given text between the
        brackets.

        Param:
            text = The text of the CDATA declaration.
            newline = Whether a newline followed by an indent will be written to
                      the output range before the end tag.
            insertIndent = Whether an indent will be inserted after each newline
                           within the text. If
                           $(D insertIndent == InsertIndent.yes), then the text
                           will be indented so that subsequent lines line up
                           with the first line.

        Throws: $(LREF XMLWritingException) if the given text is not legal in
                a CDATA section.
      +/
    void writeCDATA(R)(R text, Newline newline = Newline.yes, InsertIndent insertIndent = InsertIndent.yes)
        if(isForwardRange!R && isSomeChar!(ElementType!R))
    {
        assert(!_startTagOpen, "writeCDATA cannot be called when a start tag is open");
        checkText!(CheckText.cdata)(text);
        if(newline == Newline.yes && !_baseIndent.empty)
            put(_output, _getIndent(tagDepth));
        put(_output, "<![CDATA[");
        if(insertIndent == InsertIndent.yes)
            _insertIndent(text, tagDepth + 1);
        else
            put(_output, text);
        put(_output, "]]>");
    }

    /// Ditto
    void writeCDATA(R)(R text, InsertIndent insertIndent, Newline newline = Newline.yes)
        if(isForwardRange!R && isSomeChar!(ElementType!R))
    {
        writeCDATA(text, newline, insertIndent);
    }

    ///
    static if(compileInTests) unittest
    {
        import std.array : appender;

        auto writer = xmlWriter(appender!string());

        writer.writeStartTag("root", Newline.no);
        writer.writeCDATA("see data run");
        writer.writeCDATA("More data");
        writer.writeCDATA("No preceding newline", Newline.no);
        writer.writeCDATA("some data\nwith a newline");
        writer.writeCDATA("Another newline\nbut no indent", InsertIndent.no);
        writer.writeStartTag("tag");
        writer.writeCDATA(" Deeper data <><> ");
        writer.writeEndTag("tag");
        writer.writeEndTag("root");

        assert(writer.output.data ==
               "<root>\n" ~
               "    <![CDATA[see data run]]>\n" ~
               "    <![CDATA[More data]]><![CDATA[No preceding newline]]>\n" ~
               "    <![CDATA[some data\n" ~
               "        with a newline]]>\n" ~
               "    <![CDATA[Another newline\n" ~
               "but no indent]]>\n" ~
               "    <tag>\n" ~
               "        <![CDATA[ Deeper data <><> ]]>\n" ~
               "    </tag>\n" ~
               "</root>");
    }

    static if(compileInTests) @safe pure unittest
    {
        import dxml.internal : TestAttrOR;
        auto writer = xmlWriter(TestAttrOR.init);
        writer.writeStartTag("root");
        writer.writeCDATA("");
    }


    /++
        Writes a parsing instruction to the output range.

        Param:
            name = The name of the parsing instruction.
            text = The text of the parsing instruction.
            newline = Whether a newline followed by an indent will be written to
                      the output range before the processing instruction.
            insertIndent = Whether an indent will be inserted after each newline
                           within the text.

        Throws: $(LREF XMLWritingException) if the given name or text is not
                legal in a processing instruction.
      +/
    void writePI(R)(R name, Newline newline = Newline.yes)
        if(isForwardRange!R && isSomeChar!(ElementType!R))
    {
        assert(!_startTagOpen, "writePI cannot be called when a start tag is open");
        checkName(name);
        if(newline == Newline.yes && !_baseIndent.empty)
            put(_output, _getIndent(tagDepth));
        put(_output, "<?");
        put(_output, name);
        put(_output, "?>");
    }

    /// Ditto
    void writePI(R1, R2)(R1 name, R2 text, Newline newline = Newline.yes, InsertIndent insertIndent = InsertIndent.yes)
        if(isForwardRange!R1 && isSomeChar!(ElementType!R1) &&
           isForwardRange!R2 && isSomeChar!(ElementType!R2))
    {
        assert(!_startTagOpen, "writePI cannot be called when a start tag is open");
        checkName(name);
        checkText!(CheckText.pi)(text);
        if(newline == Newline.yes && !_baseIndent.empty)
            put(_output, _getIndent(tagDepth));
        put(_output, "<?");
        put(_output, name);
        put(_output, ' ');
        if(insertIndent == InsertIndent.yes)
            _insertIndent(text, tagDepth + 1);
        else
            put(_output, text);
        put(_output, "?>");
    }

    /// Ditto
    void writePI(R1, R2)(R1 name, R2 text, InsertIndent insertIndent, Newline newline = Newline.yes)
        if(isForwardRange!R1 && isSomeChar!(ElementType!R1) &&
           isForwardRange!R2 && isSomeChar!(ElementType!R2))
    {
        writePI(name, text, newline, insertIndent);
    }

    ///
    static if(compileInTests) unittest
    {
        import std.array : appender;

        auto writer = xmlWriter(appender!string());

        writer.writePI("pi", Newline.no);
        writer.writeStartTag("root");
        writer.writePI("Poirot", "has a cane");
        writer.writePI("Sherlock");
        writer.writePI("No", "preceding newline", Newline.no);
        writer.writePI("Ditto", Newline.no);
        writer.writePI("target", "some data\nwith a newline");
        writer.writePI("name", "Another newline\nbut no indent",
                       InsertIndent.no);
        writer.writeStartTag("tag");
        writer.writePI("Deep", "Thought");
        writer.writeEndTag("tag");
        writer.writeEndTag("root");

        assert(writer.output.data ==
               "<?pi?>\n" ~
               "<root>\n" ~
               "    <?Poirot has a cane?>\n" ~
               "    <?Sherlock?><?No preceding newline?><?Ditto?>\n" ~
               "    <?target some data\n" ~
               "        with a newline?>\n" ~
               "    <?name Another newline\n" ~
               "but no indent?>\n" ~
               "    <tag>\n" ~
               "        <?Deep Thought?>\n" ~
               "    </tag>\n" ~
               "</root>");
    }

    static if(compileInTests) @safe pure unittest
    {
        import dxml.internal : TestAttrOR;
        auto writer = xmlWriter(TestAttrOR.init);
        writer.writePI("name");
        writer.writePI("name", "text");
    }


    /++
        The current depth of the tag stack.
      +/
    @property int tagDepth() @safe const pure nothrow @nogc
    {
        return cast(int)_tagStack.length;
    }

    ///
    static if(compileInTests) unittest
    {
        import std.array : appender;

        auto writer = xmlWriter(appender!string());
        assert(writer.tagDepth == 0);

        writer.writeStartTag("root", Newline.no);
        assert(writer.tagDepth == 1);
        assert(writer.output.data == "<root>");

        writer.writeStartTag("a");
        assert(writer.tagDepth == 2);
        assert(writer.output.data ==
               "<root>\n" ~
               "    <a>");

        writer.writeStartTag("b");
        assert(writer.tagDepth == 3);
        assert(writer.output.data ==
               "<root>\n" ~
               "    <a>\n" ~
               "        <b>");

        writer.writeEndTag("b");
        assert(writer.tagDepth == 2);
        assert(writer.output.data ==
               "<root>\n" ~
               "    <a>\n" ~
               "        <b>\n" ~
               "        </b>");

        writer.writeEndTag("a");
        assert(writer.tagDepth == 1);
        assert(writer.output.data ==
               "<root>\n" ~
               "    <a>\n" ~
               "        <b>\n" ~
               "        </b>\n" ~
               "    </a>");

        writer.writeEndTag("root");
        assert(writer.tagDepth == 0);
        assert(writer.output.data ==
               "<root>\n" ~
               "    <a>\n" ~
               "        <b>\n" ~
               "        </b>\n" ~
               "    </a>\n" ~
               "</root>");
    }


    /++
        The text that will be written for each level of the tag depth when an
        indent is written.
      +/
    @property string baseIndent() @safe const pure nothrow @nogc
    {
        return _baseIndent;
    }

    ///
    static if(compileInTests) unittest
    {
        import std.array : appender;
        {
            auto writer = xmlWriter(appender!string());
            assert(writer.baseIndent == "    ");
        }
        {
            auto writer = xmlWriter(appender!string(), "  ");
            assert(writer.baseIndent == "  ");
        }
        {
            auto writer = xmlWriter(appender!string(), "\t");
            assert(writer.baseIndent == "\t");
        }
    }


    /++
        Writes a newline followed by an indent to the output range.

        In general, the various write functions already provide this
        functionality via their $(LREF Newline) parameter, but there may be
        cases where it is desirable to insert a newline independently of calling
        a write function.

        If the base indent is empty, then this function does nothing.
      +/
    void writeIndent()
    {
        if(!_baseIndent.empty)
            put(_output, _getIndent(tagDepth));
    }

    ///
    static if(compileInTests) unittest
    {
        import std.array : appender;

        auto writer = xmlWriter(appender!string());
        writer.writeStartTag("root", Newline.no);
        assert(writer.output.data == "<root>");

        writer.writeIndent();
        assert(writer.output.data ==
               "<root>\n" ~
               "    ");

        writer.writeStartTag("foo");
        assert(writer.output.data ==
               "<root>\n" ~
               "    \n" ~
               "    <foo>");

        writer.writeIndent();
        assert(writer.output.data ==
               "<root>\n" ~
               "    \n" ~
               "    <foo>\n" ~
               "        ");

        writer.writeText("some text");
        assert(writer.output.data ==
               "<root>\n" ~
               "    \n" ~
               "    <foo>\n" ~
               "        \n" ~
               "        some text");

        writer.writeIndent();
        assert(writer.output.data ==
               "<root>\n" ~
               "    \n" ~
               "    <foo>\n" ~
               "        \n" ~
               "        some text\n" ~
               "        ");

        writer.writeEndTag();
        writer.writeEndTag();
        assert(writer.output.data ==
               "<root>\n" ~
               "    \n" ~
               "    <foo>\n" ~
               "        \n" ~
               "        some text\n" ~
               "        \n" ~
               "    </foo>\n" ~
               "</root>");
    }

    static if(compileInTests) @safe pure nothrow unittest
    {
        import dxml.internal : TestAttrOR;
        auto writer = xmlWriter(TestAttrOR.init);
        writer.writeIndent();
    }


    /++
        Returns the output range that's used by XMLWriter.

        Note that if any data written to the output range without using
        XMLWriter could result in invalid XML.

        This property is here primarily to provide easy access to the output
        range when XMLWriter is done writing (e.g. to get at its $(D data)
        member if it's an $(PHOBOS_REF Appender, std, array)), but programs can
        use it to write other data to the output range while XMLWriter is still
        writing so long as it's understood that unlike when the XMLWriter's
        write functions are called, calling $(D put) on the output range
        directly is unchecked and therefore does risk making the XML invalid.
      +/
    @property ref output() @safe pure nothrow @nogc
    {
        return _output;
    }


    // See main ddoc comment for XMLWriter.
    @disable this();
    @disable this(this);


    /++
        Params:
            output = The output range that the XML will be written to.
            baseIndent = Optional argument indicating the base indent to be
                         used when an indent is inserted after a newline in the
                         XML (with the actual indent being the base indent
                         inserted once for each level of the
                         $(LREF2 tagDepth, XMLWriter.tagDepth). The default is
                         four spaces. If the indent is empty, then the XMLWriter
                         will never insert newlines or indents such that
                         $(D Newline.yes) will be the same as $(D Newline.no),
                         and $(D InsertIndent.yes) will be the same as
                         $(D InsertIndent.no).

        See_Also: $(LREF xmlWriter)
      +/
    this(OR output, string baseIndent = "    ")
    {
        import std.algorithm.searching : find;
        import std.utf : byCodeUnit; // Allows this code to be nothrow

        assert(baseIndent.byCodeUnit().find!(a => a != ' ' && a != '\t')().empty,
               "XMLWriter's base indent can only contain ' ' and '\t'");

        _output = output;
        _tagStack.reserve(10);
        _attributes.reserve(10);

        if(!baseIndent.empty)
        {
            static makeIndent(string baseIndent) pure @safe nothrow
            {
                import std.array : uninitializedArray;

                immutable indentLen = baseIndent.length;
                auto retval = uninitializedArray!(char[])(indentLen * 10 + 1);
                retval[0] = '\n';
                foreach(i; 0 .. 10)
                {
                    immutable start = i * indentLen + 1;
                    retval[start .. start + indentLen] = baseIndent;
                }
                return retval;
            }

            _baseIndent = baseIndent;
            _totalIndent = makeIndent(_baseIndent);
        }
    }


private:

    pragma(inline, true) void _incLevel(string tagName) @safe pure nothrow
    {
        _tagStack ~= tagName;
    }


    pragma(inline, true) void _decLevel() @safe /+pure+/ nothrow
    {
        --_tagStack.length;
        () @trusted { _tagStack.assumeSafeAppend(); } ();
    }


    string _getIndent(int depth) @safe pure nothrow
    {
        immutable targetLen = _baseIndent.length * depth + 1;
        while(targetLen > _totalIndent.length)
            _totalIndent ~= _baseIndent;
        return _totalIndent[0 .. targetLen];
    }

    static if(compileInTests) unittest
    {
        import std.array : appender, replicate;

        {
            auto writer = xmlWriter(appender!string());
            // We want to make sure that we have to append to _totalIndent at
            // least once.
            foreach(i; 0 .. 20)
                assert(writer._getIndent(i) == "\n" ~ "    ".replicate(i));
            foreach_reverse(i; 0 .. 20)
                assert(writer._getIndent(i) == "\n" ~ "    ".replicate(i));
        }
        {
            immutable indent = "   ";
            auto writer = xmlWriter(appender!string(), indent);
            foreach(i; 0 .. 20)
                assert(writer._getIndent(i) == "\n" ~ indent.replicate(i));
            foreach_reverse(i; 0 .. 20)
                assert(writer._getIndent(i) == "\n" ~ indent.replicate(i));
        }
    }


    void _insertIndent(R)(R text, int depth)
    {
        import std.algorithm.searching : find;
        import std.utf : byCodeUnit;

        auto bcu = text.byCodeUnit();
        static if(hasLength!(typeof(bcu)) && hasSlicing!(typeof(bcu)))
        {
            while(true)
            {
                auto found = bcu.find('\n');
                if(found.empty)
                {
                    put(_output, bcu);
                    break;
                }
                put(_output, bcu[0 .. $ - found.length]);
                put(_output, _getIndent(depth));
                bcu = found[1 .. $];
            }
        }
        else
        {
            foreach(c; bcu)
            {
                if(c == '\n')
                    put(_output, _getIndent(depth));
                else
                    put(_output, c);
            }
        }
    }


    OR _output;
    string[] _tagStack;
    string[] _attributes;
    string _baseIndent;
    string _totalIndent;
    bool _startTagOpen;
    version(assert) bool _writtenRootEnd;
}

/// Ditto
auto xmlWriter(OR)(OR output, string baseIndent = "    ")
{
    return XMLWriter!OR(output, baseIndent);
}

///
version(dxmlTests) unittest
{
    import std.array : appender;
    {
        auto writer = xmlWriter(appender!string());
        writer.writeStartTag("root");

        writer.openStartTag("foo");
        writer.writeAttr("a", "42");
        writer.closeStartTag();

        writer.writeText("bar");

        writer.writeEndTag("foo");

        writer.writeEndTag("root");

        assert(writer.output.data ==
               "\n" ~
               "<root>\n" ~
               `    <foo a="42">` ~ "\n" ~
               "        bar\n" ~
               "    </foo>\n" ~
               "</root>");
    }
    // Newline.no can be used to selectively avoid inserting newlines.
    {
        auto writer = xmlWriter(appender!string());

        // Unless writeXMLDecl was used, Newline.no is needed on the first
        // entity to avoid having the document start with a newline.
        writer.writeStartTag("root", Newline.no);

        writer.openStartTag("foo");
        writer.writeAttr("a", "42");
        writer.closeStartTag();

        writer.writeText("bar", Newline.no);

        writer.writeEndTag("foo", Newline.no);

        writer.writeEndTag("root");

        assert(writer.output.data ==
               "<root>\n" ~
               `    <foo a="42">bar</foo>` ~ "\n" ~
               "</root>");
    }
    // An empty base indent means that no newlines or indents are inserted.
    {
        auto writer = xmlWriter(appender!string(), "");
        writer.writeStartTag("root");

        writer.openStartTag("foo");
        writer.writeAttr("a", "42");
        writer.closeStartTag();

        writer.writeText("bar");

        writer.writeEndTag("foo");

        writer.writeEndTag("root");

        assert(writer.output.data == `<root><foo a="42">bar</foo></root>`);
    }
}

version(dxmlTests) @safe pure nothrow unittest
{
    import dxml.internal : TestAttrOR;
    auto writer = xmlWriter(TestAttrOR.init);
}

// This is purely to provide a way to trigger the unittest blocks in XMLWriter
// without compiling them in normally.
private struct XMLWriterCompileTests
{
    void put(char c) @safe pure nothrow @nogc { assert(0); }
}

version(dxmlTests)
    auto _xmlWriterTests = XMLWriter!(XMLWriterCompileTests).init;


/++
    Writes the $(D <?xml...?>) declaration to the given output range. If it's
    going to be used in conjunction with $(LREF XMLWriter), then either
    writeXMLDecl will need to be called before constructing the
    $(LREF XMLWriter), or $(LREF XMLWriter.output) will need to be used to write
    to the output range before writing anything else using the
    $(LREF XMLWriter). $(LREF XMLWriter) expects to be writing XML after the
    $(D <?xml...?>) and $(D <!DOCTYPE...>) declarations (assuming they're
    present at all).

    Params:
        S = The string type used to infer the encoding type. Ideally, it would
            be inferred from the type of the output range, but unfortunately,
            that's not possible. However, if S does not match the encoding of
            the output range, then the result will be invalid XML.
        output = The output range to write to.
  +/
void writeXMLDecl(S, OR)(ref OR output)
    if(isOutputRange!(OR, char) && isSomeString!S)
{
    put(output, `<?xml version="1.0"`);
    static if(is(Unqual!(ElementEncodingType!S) == char))
        put(output, ` encoding="UTF-8"?>`);
    else static if(is(Unqual!(ElementEncodingType!S) == wchar))
        put(output, ` encoding="UTF-16"?>`);
    else
        put(output, ` encoding="UTF-32"?>`);
}

///
version(dxmlTests) unittest
{
    import std.array : appender;

    {
        auto app = appender!string();
        app.writeXMLDecl!string();
        assert(app.data == `<?xml version="1.0" encoding="UTF-8"?>`);
    }
    {
        auto app = appender!wstring();
        app.writeXMLDecl!wstring();
        assert(app.data == `<?xml version="1.0" encoding="UTF-16"?>`w);
    }
    {
        auto app = appender!dstring();
        app.writeXMLDecl!dstring();
        assert(app.data == `<?xml version="1.0" encoding="UTF-32"?>`d);
    }

    // This would be invalid XML, because the output range contains UTF-8, but
    // writeXMLDecl is told to write that the encoding is UTF-32.
    {
        auto app = appender!string();
        app.writeXMLDecl!dstring();
        assert(app.data == `<?xml version="1.0" encoding="UTF-32"?>`);
    }
}


/++
    Helper function for writing text which has a start tag and end tag on each
    side and no attributes so that it can be done with one function call instead
    of three.

    writeTaggedText is equivalent to calling

    ---
    writer.writeStartTag(name, newline);
    writer.writeText(text, insertIndent, Newline.no);
    writer.writeEndTag(Newline.no);
    ---

    If more control is needed over the formatting, or if attributes are needed
    on the start tag, then the functions will have to be called separately
    instead of calling writeTaggedText.

    Params:
            writer = The $(LREF XMLWriter) to write to.
            name = The name of the start tag.
            newline = Whether a newline followed by an indent will be written to
                      the output range before the start tag.
            insertIndent = Whether an indent will be inserted after each newline
                           within the text.

    See_Also: $(LREF2 writeStartTag, XMLWriter.writeStartTag)$(BR)
              $(LREF2 writeText, XMLWriter.writeText)$(BR)
              $(LREF2 writeEndTag, XMLWriter.writeEndTag)
  +/
void writeTaggedText(XW, R1, R2)(ref XW writer, R1 name, R2 text, Newline newline = Newline.yes,
                                 InsertIndent insertIndent = InsertIndent.yes)
    if(isInstanceOf!(XMLWriter, XW) &&
       isForwardRange!R1 && isSomeChar!(ElementType!R1) &&
       isForwardRange!R2 && isSomeChar!(ElementType!R2))
{
    writer.writeStartTag(name, newline);
    writer.writeText(text, insertIndent, Newline.no);
    writer.writeEndTag(Newline.no);
}

/// Ditto
void writeTaggedText(XW, R1, R2)(ref XW writer, R1 name, R2 text, InsertIndent insertIndent,
                                 Newline newline = Newline.yes)
    if(isInstanceOf!(XMLWriter, XW) &&
       isForwardRange!R1 && isSomeChar!(ElementType!R1) &&
       isForwardRange!R2 && isSomeChar!(ElementType!R2))
{
    writeTaggedText(writer, name, text, newline, insertIndent);
}

///
version(dxmlTests) unittest
{
    import std.array : appender;

    {
        auto writer = xmlWriter(appender!string());
        writer.writeStartTag("root", Newline.no);
        writer.writeTaggedText("foo", "Some text between foos");
        writer.writeEndTag("root");

        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo>Some text between foos</foo>\n" ~
               "</root>");
    }

    // With Newline.no
    {
        auto writer = xmlWriter(appender!string());
        writer.writeStartTag("root", Newline.no);
        writer.writeTaggedText("foo", "Some text between foos", Newline.no);
        writer.writeEndTag("root");

        assert(writer.output.data ==
               "<root><foo>Some text between foos</foo>\n" ~
               "</root>");
    }

    // With InsertIndent.yes
    {
        auto writer = xmlWriter(appender!string());
        writer.writeStartTag("root", Newline.no);
        writer.writeTaggedText("foo", "Some text\nNext line");
        writer.writeEndTag("root");

        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo>Some text\n" ~
               "        Next line</foo>\n" ~
               "</root>");
    }

    // With InsertIndent.no
    {
        auto writer = xmlWriter(appender!string());
        writer.writeStartTag("root", Newline.no);
        writer.writeTaggedText("foo", "Some text\nNext line", InsertIndent.no);
        writer.writeEndTag("root");

        assert(writer.output.data ==
               "<root>\n" ~
               "    <foo>Some text\n" ~
               "Next line</foo>\n" ~
               "</root>");
    }
}

// _decLevel cannot currently be pure.
version(dxmlTests) @safe /+pure+/ unittest
{
    import dxml.internal : TestAttrOR;
    auto writer = xmlWriter(TestAttrOR.init);
    writer.writeTaggedText("root", "text");
}


private:

void checkName(R)(R range)
{
    import std.format : format;
    import std.range : takeExactly;
    import std.utf : byCodeUnit, decodeFront, UseReplacementDchar;
    import dxml.internal : isNameStartChar, isNameChar;

    auto text = range.byCodeUnit();

    size_t takeLen;
    {
        immutable decodedC = text.decodeFront!(UseReplacementDchar.yes)(takeLen);
        if(!isNameStartChar(decodedC))
            throw new XMLWritingException(format!"Name contains invalid character: 0x%0x"(decodedC));
    }

    while(!text.empty)
    {
        size_t numCodeUnits;
        immutable decodedC = text.decodeFront!(UseReplacementDchar.yes)(numCodeUnits);
        if(!isNameChar(decodedC))
            throw new XMLWritingException(format!"Name contains invalid character: 0x%0x"(decodedC));
    }
}

version(dxmlTests) @safe pure unittest
{
    import std.exception : assertNotThrown, assertThrown;
    import std.range : only;
    import dxml.internal : testRangeFuncs;

    static foreach(func; testRangeFuncs)
    {
        foreach(str; only("hello", "プログラミング", "h_:llo-.42", "_.", "_-", "_42", "プログラミング"))
            assertNotThrown!XMLWritingException(checkName(func(str)));

        foreach(str; only(".", ".foo", "-foo", "&foo;", "foo\vbar"))
            assertThrown!XMLWritingException(checkName(func(str)));
    }
}


enum CheckText
{
    attValueApos,
    attValueQuot,
    cdata,
    comment,
    pi,
    text
}

void checkText(CheckText ct, R)(R range)
{
    import std.format : format;
    import std.utf : byCodeUnit, decodeFront, UseReplacementDchar;

    auto text = range.byCodeUnit();

    loop: while(!text.empty)
    {
        switch(text.front)
        {
            static if(ct == CheckText.attValueApos || ct == CheckText.attValueQuot || ct == CheckText.text)
            {
                import std.algorithm.searching : startsWith;

                case '&':
                {
                    import dxml.util : parseCharRef;

                    {
                        auto temp = text.save;
                        auto charRef = parseCharRef(temp);
                        if(!charRef.isNull)
                        {
                            static if(hasLength!(typeof(text)))
                                text = temp;
                            else
                            {
                                while(text.front != ';')
                                    text.popFront();
                                text.popFront();
                            }
                            continue;
                        }
                    }

                    text.popFront();

                    static foreach(entRef; ["amp;", "apos;", "quot;", "lt;", "gt;"])
                    {
                        if(text.save.startsWith(entRef))
                        {
                            text.popFrontN(entRef.length);
                            continue loop;
                        }
                    }

                    throw new XMLWritingException("& is only legal in an EntitType.text entity as part of a " ~
                                                  "reference, and this dxml only supports entity references if " ~
                                                  "they're predefined by the spec. This is not a valid character " ~
                                                  "reference or one of the predefined entity references.");
                }
                case '<': throw new XMLWritingException("< is not legal in EntityType.text");
            }
            static if(ct == CheckText.comment)
            {
                case '-':
                {
                    text.popFront();
                    if(text.empty)
                        throw new XMLWritingException("- is not legal at the end of an EntityType.comment");
                    if(text.front == '-')
                        throw new XMLWritingException("-- is not legal in EntityType.comment");
                    break;
                }
            }
            else static if(ct == CheckText.cdata || ct == CheckText.text)
            {
                case ']':
                {
                    import std.algorithm.searching : startsWith;
                    text.popFront();
                    if(text.save.startsWith("]>"))
                    {
                        static if(ct == CheckText.cdata)
                            throw new XMLWritingException("]]> is not legal in EntityType.cdata");
                        else
                            throw new XMLWritingException("]]> is not legal in EntityType.text");
                    }
                    break;
                }
            }
            else static if(ct == CheckText.attValueApos)
            {
                case '\'':
                {
                    throw new XMLWritingException("If a single quote is the attrbute value's delimiter, then it's " ~
                                                  "illegal for the attribute value to contain a single quote. Either " ~
                                                  "instantiate writeAttr with a double quote instead or use " ~
                                                  "&apos; in the attribute value instead of a single quote.");
                }
            }
            else static if(ct == CheckText.attValueQuot)
            {
                case '"':
                {
                    throw new XMLWritingException("If a double quote is the attrbute value's delimiter, then it's " ~
                                                  "illegal for the attribute value to contain a double quote. Either " ~
                                                  "instantiate writeAttr with a single quote instead or use " ~
                                                  "&quot; in the attribute value instead of a double quote.");
                }
            }
            case '\n':
            {
                text.popFront();
                break;
            }
            default:
            {
                import std.ascii : isASCII;
                import dxml.internal : isXMLChar;
                immutable c = text.front;
                if(isASCII(c))
                {
                    if(!isXMLChar(c))
                        throw new XMLWritingException(format!"Character is not legal in an XML File: 0x%0x"(c));
                    text.popFront();
                }
                else
                {
                    import std.utf : UTFException;
                    // Annoyngly, letting decodeFront throw is the easier way to handle this, since the
                    // replacement character is considered valid XML, and if we decoded using it, then
                    // all of the invalid Unicode characters would come out as the replacement character
                    // and then be treated as valid instead of being caught, which we could do, but then
                    // the resulting XML document would contain the replacement character without the
                    // caller knowing it, which almost certainly means that a bug would go unnoticed.
                    try
                    {
                        size_t numCodeUnits;
                        immutable decodedC = text.decodeFront!(UseReplacementDchar.no)(numCodeUnits);
                        if(!isXMLChar(decodedC))
                        {
                            enum fmt = "Character is not legal in an XML File: 0x%0x";
                            throw new XMLWritingException(format!fmt(decodedC));
                        }
                    }
                    catch(UTFException)
                        throw new XMLWritingException("Text contains invalid Unicode character");
                }
                break;
            }
        }
    }
}

version(dxmlTests) unittest
{
    import std.exception : assertNotThrown, assertThrown;
    import dxml.internal : testRangeFuncs;

    static void test(alias func, CheckText ct)(string text, size_t line = __LINE__)
    {
        assertNotThrown(checkText!ct(func(text)), "unittest failure", __FILE__, line);
    }

    static void testFail(alias func, CheckText ct)(string text, size_t line = __LINE__)
    {
        assertThrown!XMLWritingException(checkText!ct(func(text)), "unittest failure", __FILE__, line);
    }

    static foreach(func; testRangeFuncs)
    {
        static foreach(ct; EnumMembers!CheckText)
        {
            test!(func, ct)("");
            test!(func, ct)("J",);
            test!(func, ct)("foo");
            test!(func, ct)("プログラミング");

            test!(func, ct)("&amp;&gt;&lt;");
            test!(func, ct)("hello&amp;&gt;&lt;world");
            test!(func, ct)(".....&apos;&quot;&amp;.....");
            test!(func, ct)("&#12487;&#12451;&#12521;&#12531;");
            test!(func, ct)("-hello&#xAF;&#42;&quot;-world");

            test!(func, ct)("]]");
            test!(func, ct)("]>");
            test!(func, ct)("foo]]bar");
            test!(func, ct)("foo]>bar");
            test!(func, ct)("]] >");

            testFail!(func, ct)("\v");
            testFail!(func, ct)("\uFFFE");
            testFail!(func, ct)("hello\vworld");
            testFail!(func, ct)("he\nllo\vwo\nrld");
        }

        static foreach(ct; [CheckText.attValueApos, CheckText.attValueQuot, CheckText.text])
        {
            testFail!(func, ct)("<");
            testFail!(func, ct)("&");
            testFail!(func, ct)("&");
            testFail!(func, ct)("&x");
            testFail!(func, ct)("&&;");
            testFail!(func, ct)("&a");
            testFail!(func, ct)("&a;");
            testFail!(func, ct)(`&am;`);
            testFail!(func, ct)(`&ampe;`);
            testFail!(func, ct)(`&l;`);
            testFail!(func, ct)(`&lte;`);
            testFail!(func, ct)(`&g;`);
            testFail!(func, ct)(`&gte;`);
            testFail!(func, ct)(`&apo;`);
            testFail!(func, ct)(`&aposs;`);
            testFail!(func, ct)(`&quo;`);
            testFail!(func, ct)(`&quote;`);
            testFail!(func, ct)("hello&;");
            testFail!(func, ct)("hello&;world");
            testFail!(func, ct)("hello&<;world");
            testFail!(func, ct)("hello&world");
            testFail!(func, ct)("hello world&");
            testFail!(func, ct)("hello world&;");
            testFail!(func, ct)("hello world&foo");
            testFail!(func, ct)("&#;");
            testFail!(func, ct)("&#x;");
            testFail!(func, ct)("&#AF;");
            testFail!(func, ct)("&#x");
            testFail!(func, ct)("&#42");
            testFail!(func, ct)("&#x42");
            testFail!(func, ct)("&#12;");
            testFail!(func, ct)("&#x12;");
            testFail!(func, ct)("&#42;foo\nbar&#;");
            testFail!(func, ct)("&#42;foo\nbar&#x;");
            testFail!(func, ct)("&#42;foo\nbar&#AF;");
            testFail!(func, ct)("&#42;foo\nbar&#x");
            testFail!(func, ct)("&#42;foo\nbar&#42");
            testFail!(func, ct)("&#42;foo\nbar&#x42");
            testFail!(func, ct)("プログラミング&");
        }

        static foreach(ct; EnumMembers!CheckText)
        {
            static if(ct == CheckText.attValueApos)
                testFail!(func, ct)(`foo'bar`);
            else
                test!(func, ct)(`foo'bar`);

            static if(ct == CheckText.attValueQuot)
                testFail!(func, ct)(`foo"bar`);
            else
                test!(func, ct)(`foo"bar`);

            static if(ct == CheckText.comment)
            {
                testFail!(func, ct)("-");
                testFail!(func, ct)("--");
                testFail!(func, ct)("--*");
            }
            else
            {
                test!(func, ct)("-");
                test!(func, ct)("--");
                test!(func, ct)("--*");
            }
        }

        static foreach(ct; [CheckText.attValueApos, CheckText.attValueQuot, CheckText.pi])
        {
            test!(func, ct)("]]>");
            test!(func, ct)("foo]]>bar");
        }
        static foreach(ct; [CheckText.cdata, CheckText.text])
        {
            testFail!(func, ct)("]]>");
            testFail!(func, ct)("foo]]>bar");
        }

        static foreach(ct; [CheckText.cdata, CheckText.comment, CheckText.pi])
        {
            test!(func, ct)("<");
            test!(func, ct)("&");
            test!(func, ct)("&x");
            test!(func, ct)("&&;");
            test!(func, ct)("&a");
            test!(func, ct)("&a;");
            test!(func, ct)(`&am;`);
            test!(func, ct)(`&ampe;`);
            test!(func, ct)(`&l;`);
            test!(func, ct)(`&lte;`);
            test!(func, ct)(`&g;`);
            test!(func, ct)(`&gte;`);
            test!(func, ct)(`&apo;`);
            test!(func, ct)(`&aposs;`);
            test!(func, ct)(`&quo;`);
            test!(func, ct)(`&quote;`);
            test!(func, ct)("hello&;");
            test!(func, ct)("hello&;world");
            test!(func, ct)("hello&<;world");
            test!(func, ct)("hello&world");
            test!(func, ct)("hello world&");
            test!(func, ct)("hello world&;");
            test!(func, ct)("hello world&foo");
            test!(func, ct)("&#;");
            test!(func, ct)("&#x;");
            test!(func, ct)("&#AF;");
            test!(func, ct)("&#x");
            test!(func, ct)("&#42");
            test!(func, ct)("&#x42");
            test!(func, ct)("&#12;");
            test!(func, ct)("&#x12;");
            test!(func, ct)("&#42;foo\nbar&#;");
            test!(func, ct)("&#42;foo\nbar&#x;");
            test!(func, ct)("&#42;foo\nbar&#AF;");
            test!(func, ct)("&#42;foo\nbar&#x");
            test!(func, ct)("&#42;foo\nbar&#42");
            test!(func, ct)("&#42;foo\nbar&#x42");
            test!(func, ct)("プログラミング&");
        }
    }

    // These can't be tested with testFail, because attempting to convert
    // invalid Unicode results in UnicodeExceptions before checkText even
    // gets called.
    import std.meta : AliasSeq;
    static foreach(str; AliasSeq!(cast(string)[255], cast(wstring)[0xD800], cast(dstring)[0xD800]))
    {
        static foreach(ct; EnumMembers!CheckText)
        {
            assertThrown!XMLWritingException(checkText!ct(str));
            assertThrown!XMLWritingException(checkText!ct(str));
        }
    }
}

version(dxmlTests) @safe pure unittest
{
    static foreach(ct; EnumMembers!CheckText)
        checkText!ct("foo");
}
