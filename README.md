# dxml

dxml is a library written in the D programming language for parsing XML 1.0.

dxml's parser is a range-based [StAX parser](https://en.wikipedia.org/wiki/StAX),
but dxml also has support for generating a DOM using the parser.

dxml does not currently have any XML writers, but writer support is planned.

The parser should be complete as-is, but more helper functions probably should
and will be added in order to make common idioms using the parser less verbose.
However, unless a significant problem is found with the current API, it is
expected that the API of the parser itself will remain essentially unchanged.

For the sake of simplicitly, sanity, and efficiency, dxml does not support the
DTD section of XML beyond what is required to correctly parse past it. Both the
XML declaration and the DOCTYPE definition are skipped if they are encountered
at the start of the document. So, while dxml should work wonderfully for a lot
of XML documents, if the DTD section is required to correctly parse the
document (i.e. the document contains entity references that the DTD section
defines), then dxml will consider those documents to be invalid XML, and dxml
will not be a good solution for parsing those documents. See the documentation
for details.

The documentation can be found [here](http://jmdavisprog.com/projects.html)
rendered as html, or it can be read from the source code.

Currently, the oldest supported version of dmd is 2.078.0. dxml will not
compile with older versions.
