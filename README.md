# dxml

dxml is a library written in the D programming language for parsing XML 1.0.

Currently, it contains only a range-based
[StAX parser](https://en.wikipedia.org/wiki/StAX) and related helper functions,
but the plan is to add a DOM parser as well as two XML writers - one which is
essentially the writer version of a StAX parser, and one which is DOM-based.

However, the StAX parser should be complete as-is. More helper functions
probably should and will be added in order to make common idioms using the StAX
parser less verbose, but unless a significant problem is found with the current
API, it is expected that the API of the parser itself will remain essentially
unchanged.

For the sake of simplicitly, sanity, and efficiency, dxml does not support the
DTD section of XML. Both the XML declaration and the DOCTYPE definition are
skipped if they are encountered at the start of the document. So, while dxml
should work wonderfully for a lot of XML documents, if the DTD section is
required to correctly parse the document (i.e. the document contains entity
references that the DTD section defines), then dxml will consider those
documents to be invalid XML, and dxml will not be a good solution for parsing
those documents. See the documentation for details.

The documentation can be found [here](http://jmdavisprog.com/projects.html)
rendered as html, or it can be read from the source code.
