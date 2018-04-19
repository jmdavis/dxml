# dxml

dxml is a library written in the D programming language for parsing XML 1.0.

dxml's parser is a range-based [StAX parser](https://en.wikipedia.org/wiki/StAX),
but dxml also has support for generating a DOM using the parser. dxml also
provides support for writing XML.

The parser should be complete as-is, but more helper functions probably should
and will be added in order to make common idioms using the parser less verbose.
However, unless a significant problem is found with the current API, it is
expected that the API of the parser itself will remain essentially unchanged.

The basic writer functionality should also be essentially complete as-is,
though there are plans to add writer support to the DOM functionality at some
point in the future, and more helper functions may be added as well.

For the sake of simplicitly, sanity, and efficiency, dxml does not support the
DTD section of XML beyond what is required to correctly parse past it. Both the
XML declaration and the DOCTYPE definition are skipped if they are encountered
at the start of the document. So, while dxml should work wonderfully for a lot
of XML documents, it's not a good solution for programs that require full DTD
support.

By default, any entity references other than the five defined in the XML
standard are considered invalid XML (since dxml cannot properly replace them
with whatever XML they represent), but the parser can be configured to treat
entity references as normal text (in which case, it validates their syntax but
otherwise treats them as normal text). See the documentation for details.

The documentation can be found [here](http://jmdavisprog.com/projects.html)
rendered as html, or it can be read from the source code.

Currently, the oldest supported version of dmd is 2.078.0. dxml will not
compile with older versions.
