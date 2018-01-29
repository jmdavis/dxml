// Written in the D programming language

/++
    This runs some of the spec conformance tests found at
    $(LINK http://www.w3.org/XML/Test/) on dxml.

    Copyright: Copyright 2018
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   Jonathan M Davis

    See_Also: $(LINK2 http://www.w3.org/TR/REC-xml/, Official Specification for XML 1.0)
  +/
module main;

import std.range.primitives;
import dxml.parser.stax;

enum testsDir = "xmlconf";

void main()
{
    xmlTest();
    japanese();
}

void xmlTest()
{
    // Unfortunately, because we ignore the DTD section and only validate it
    // enough to parse past it, we fail a number of the tests, because they
    // test that the parser reports an error for invalid DTD stuff, which we
    // don't care about. We also fail several tests, because they use an entity
    // reference without declaring it first, and we don't care about that,
    // because detecting that requires processing the DTD section. We also fail
    // some tests related to the XML declaration at the top of the file, since
    // we skip that too.
    auto ignoreList =
    [
        "xmlconf/xmltest/not-wf/sa/054.xml", "xmlconf/xmltest/not-wf/sa/056.xml", "xmlconf/xmltest/not-wf/sa/057.xml",
        "xmlconf/xmltest/not-wf/sa/058.xml", "xmlconf/xmltest/not-wf/sa/059.xml", "xmlconf/xmltest/not-wf/sa/060.xml",
        "xmlconf/xmltest/not-wf/sa/061.xml", "xmlconf/xmltest/not-wf/sa/062.xml", "xmlconf/xmltest/not-wf/sa/064.xml",
        "xmlconf/xmltest/not-wf/sa/065.xml", "xmlconf/xmltest/not-wf/sa/066.xml", "xmlconf/xmltest/not-wf/sa/067.xml",
        "xmlconf/xmltest/not-wf/sa/068.xml", "xmlconf/xmltest/not-wf/sa/069.xml", "xmlconf/xmltest/not-wf/sa/071.xml",
        "xmlconf/xmltest/not-wf/sa/072.xml", "xmlconf/xmltest/not-wf/sa/073.xml", "xmlconf/xmltest/not-wf/sa/074.xml",
        "xmlconf/xmltest/not-wf/sa/075.xml", "xmlconf/xmltest/not-wf/sa/076.xml", "xmlconf/xmltest/not-wf/sa/077.xml",
        "xmlconf/xmltest/not-wf/sa/078.xml", "xmlconf/xmltest/not-wf/sa/079.xml", "xmlconf/xmltest/not-wf/sa/080.xml",
        "xmlconf/xmltest/not-wf/sa/081.xml", "xmlconf/xmltest/not-wf/sa/082.xml", "xmlconf/xmltest/not-wf/sa/083.xml",
        "xmlconf/xmltest/not-wf/sa/084.xml", "xmlconf/xmltest/not-wf/sa/085.xml", "xmlconf/xmltest/not-wf/sa/086.xml",
        "xmlconf/xmltest/not-wf/sa/087.xml", "xmlconf/xmltest/not-wf/sa/089.xml", "xmlconf/xmltest/not-wf/sa/090.xml",
        "xmlconf/xmltest/not-wf/sa/091.xml", "xmlconf/xmltest/not-wf/sa/092.xml", "xmlconf/xmltest/not-wf/sa/094.xml",
        "xmlconf/xmltest/not-wf/sa/095.xml", "xmlconf/xmltest/not-wf/sa/096.xml", "xmlconf/xmltest/not-wf/sa/097.xml",
        "xmlconf/xmltest/not-wf/sa/098.xml", "xmlconf/xmltest/not-wf/sa/099.xml", "xmlconf/xmltest/not-wf/sa/100.xml",
        "xmlconf/xmltest/not-wf/sa/101.xml", "xmlconf/xmltest/not-wf/sa/102.xml", "xmlconf/xmltest/not-wf/sa/103.xml",
        "xmlconf/xmltest/not-wf/sa/113.xml", "xmlconf/xmltest/not-wf/sa/114.xml", "xmlconf/xmltest/not-wf/sa/115.xml",
        "xmlconf/xmltest/not-wf/sa/116.xml", "xmlconf/xmltest/not-wf/sa/117.xml", "xmlconf/xmltest/not-wf/sa/119.xml",
        "xmlconf/xmltest/not-wf/sa/120.xml", "xmlconf/xmltest/not-wf/sa/121.xml", "xmlconf/xmltest/not-wf/sa/121.xml",
        "xmlconf/xmltest/not-wf/sa/122.xml", "xmlconf/xmltest/not-wf/sa/123.xml", "xmlconf/xmltest/not-wf/sa/124.xml",
        "xmlconf/xmltest/not-wf/sa/125.xml", "xmlconf/xmltest/not-wf/sa/126.xml", "xmlconf/xmltest/not-wf/sa/127.xml",
        "xmlconf/xmltest/not-wf/sa/128.xml", "xmlconf/xmltest/not-wf/sa/129.xml", "xmlconf/xmltest/not-wf/sa/130.xml",
        "xmlconf/xmltest/not-wf/sa/131.xml", "xmlconf/xmltest/not-wf/sa/132.xml", "xmlconf/xmltest/not-wf/sa/133.xml",
        "xmlconf/xmltest/not-wf/sa/134.xml", "xmlconf/xmltest/not-wf/sa/135.xml", "xmlconf/xmltest/not-wf/sa/136.xml",
        "xmlconf/xmltest/not-wf/sa/137.xml", "xmlconf/xmltest/not-wf/sa/138.xml", "xmlconf/xmltest/not-wf/sa/139.xml",
        "xmlconf/xmltest/not-wf/sa/140.xml", "xmlconf/xmltest/not-wf/sa/141.xml", "xmlconf/xmltest/not-wf/sa/149.xml",
        "xmlconf/xmltest/not-wf/sa/152.xml", "xmlconf/xmltest/not-wf/sa/153.xml", "xmlconf/xmltest/not-wf/sa/158.xml",
        "xmlconf/xmltest/not-wf/sa/159.xml", "xmlconf/xmltest/not-wf/sa/160.xml", "xmlconf/xmltest/not-wf/sa/161.xml",
        "xmlconf/xmltest/not-wf/sa/162.xml", "xmlconf/xmltest/not-wf/sa/165.xml", "xmlconf/xmltest/not-wf/sa/175.xml",
        "xmlconf/xmltest/not-wf/sa/180.xml", "xmlconf/xmltest/not-wf/sa/182.xml", "xmlconf/xmltest/not-wf/sa/183.xml",
        "xmlconf/xmltest/not-wf/sa/184.xml", "xmlconf/xmltest/not-wf/sa/185.xml",
        "xmlconf/xmltest/not-wf/not-sa/001.xml", "xmlconf/xmltest/not-wf/not-sa/002.xml",
        "xmlconf/xmltest/not-wf/not-sa/003.xml", "xmlconf/xmltest/not-wf/not-sa/004.xml",
        "xmlconf/xmltest/not-wf/not-sa/005.xml", "xmlconf/xmltest/not-wf/not-sa/006.xml",
        "xmlconf/xmltest/not-wf/not-sa/007.xml", "xmlconf/xmltest/not-wf/not-sa/008.xml",
        "xmlconf/xmltest/not-wf/not-sa/009.xml",
        "xmlconf/xmltest/not-wf/ext-sa/001.xml", "xmlconf/xmltest/not-wf/ext-sa/002.xml",
        "xmlconf/xmltest/not-wf/ext-sa/003.xml",
        "xmlconf/xmltest/invalid/002.xml", "xmlconf/xmltest/invalid/005.xml", "xmlconf/xmltest/invalid/006.xml",
        "xmlconf/xmltest/invalid/not-sa/022.xml",
    ];

    import std.path : buildPath;
    validateTests(buildPath(testsDir, "xmltest", "xmltest.xml"), ignoreList);
}

void japanese()
{
    auto ignoreList =
    [
        "xmlconf/japanese/pr-xml-utf-16.xml", // FIXME big-endian
        "xmlconf/japanese/weekly-utf-16.xml", // FIXME big-endian
    ];

    import std.path : buildPath;
    validateTests(buildPath(testsDir, "japanese", "japanese.xml"), ignoreList);
}

void validateTests(string mainFile, string[] ignoreList)
{
    import std.algorithm : canFind, filter;
    import std.exception : enforce;
    import std.file : readText;
    import std.format : format;
    import std.path : buildPath, dirName;
    import std.typecons : Nullable, nullable;

    foreach(entity; parseXML!simpleXML(readText(mainFile)).
                    filter!(a => a.type == EntityType.elementStart && a.name == "TEST")())
    {
        Nullable!bool tt;
        string uri;
        foreach(attr; entity.attributes)
        {
            if(attr.name == "TYPE")
            {
                switch(attr.value)
                {
                    case "valid": tt = nullable(true); break;
                    case "invalid":
                    case "not-wf":
                    case "error": tt = nullable(false); break;
                    default: throw new Exception(format("Unexpected test type: %s", attr.value));
                }
            }
            else if(attr.name == "URI")
                uri = attr.value;
        }
        enforce(!tt.isNull, format("In %s, %s: TEST tag missing TYPE attribute", mainFile, entity.pos));
        enforce(!uri.empty, format("In %s, %s: TEST tag missing URI attribute", mainFile, entity.pos));

        auto file = buildPath(mainFile.dirName, uri);
        if(ignoreList.canFind(file))
            continue;
        if(tt.get)
            parseExpectSuccess(file);
        else
            parseExpectFailure(file);
    }
}

void parseExpectSuccess(string file)
{
    import std.encoding : BOM;
    import std.exception : assertNotThrown, enforce;
    import std.format : format;
    import std.file : exists, readBOM, readText;
    import std.utf : UTFException;

    enforce(file.exists, format("%s does not exist", file));
    immutable bom = readBOM(file).schema;
    // FIXME This only works properly with little-endian machines right now, and 
    // we need a way to process big endian encodings.
    switch(bom)
    {
        case BOM.none:
        case BOM.utf8:
        {
            auto text = file.readText();
            assertNotThrown!XMLParsingException(parseEverything(text), format("%s failed", file));
            break;
        }
        case BOM.utf16le:
        {
            auto text = file.readText!wstring();
            assertNotThrown!XMLParsingException(parseEverything(text), format("%s failed", file));
            break;
        }
        default: throw new Exception(format("Unsupported encoding: %s, file: %s", bom, file));
    }
}

void parseExpectFailure(string file)
{
    import std.exception : assertThrown, enforce;
    import std.format : format;
    import std.file : exists;
    enforce(file.exists, format("%s does not exist", file));
    auto text = file.unvalidatedReadText();
    assertThrown!XMLParsingException(parseEverything(text), format("%s failed", file));
}

void parseEverything(S)(S xml)
{
    with(EntityType) foreach(entity; parseXML(xml))
    {
        final switch(entity.type)
        {
            case cdata: break;
            case comment: break;
            case elementStart: auto name = entity.name; break;
            case elementEnd: goto case elementStart;
            case elementEmpty: goto case elementStart;
            case pi: goto case elementStart;
            case text: break;
        }

        final switch(entity.type)
        {
            case cdata: auto text = entity.text; break;
            case comment: goto case cdata;
            case elementStart:
            {
                foreach(attr; entity.attributes)
                {
                    auto name = attr.name;
                    auto value = attr.value;
                }
                break;
            }
            case elementEnd: break;
            case elementEmpty: goto case elementStart;
            case pi: goto case cdata;
            case text: goto case cdata;
        }
    }
}

// We should probably consider making dxml work with ranges of ubyte, ushort,
// and uint, since char, wchar, and dchar are supposed to be valid Unicode, but
// for now it doesn't. In any case, we need this function, because readText
// validates the characters, and at least one of the tests has invalid Unicode
// in it - and the parser is set up such that it should be catching such a
// character if it's given it, so I'd just as soon test it this way rather than
// skipping the test on the theory that any string would have already been
// validated as far as Unicode goes.
string unvalidatedReadText(string file)
{
    import std.file : read;
    return cast(string)read(file);
}
