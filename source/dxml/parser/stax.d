// Written in the D programming language

/++
    When adding DOM support, it became clear that it made more sense to treat
    the StAX parser as the only parser, since the DOM is really just a way to
    hold the result of the parsed data and not really a parser. So,
    dxml.parser.stax has been renamed to dxml.parser, which also has the
    benefit of reducing the length of the import path.

    For dxml 0.2.0, the contents of dxml/parser/stax.d have been moved to
    dxml/parser/package.d, and dxml/parser/stax.d now publicly imports
    dxml.parser but is marked as deprecated. In dxml 0.3.0, dxml/parser/stax.d
    will be removed, and dxml/parser/package.d will be moved to dxml/parser.d.
    So, there's one release with a deprecation warning for those folks who
    grabbed the first release.
  +/
deprecated("dxml.parser.stax has been moved to dxml.parser. dxml.parser.stax publicly imports it for now but will be removed in 0.3.0.")
module dxml.parser.stax;

public import dxml.parser;
