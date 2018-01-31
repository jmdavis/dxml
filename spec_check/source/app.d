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

void main()
{
    xmltest();
    japanese();
    ibm();
}

void xmltest()
{
    // Unfortunately, because we skip passed the XML declaration and the DTD
    // section, we fail many tests - either because they validate those sections
    // or because they include entity references in the main body of the XML
    // which are not predefined entities (we consider them to be invalid XML,
    // because we can't process them properly without processing the DTD
    // section).
    auto ignoreList =
    [
        "xmlconf/xmltest/not-wf/sa/054.xml", "xmlconf/xmltest/not-wf/sa/056.xml", "xmlconf/xmltest/not-wf/sa/057.xml",
        "xmlconf/xmltest/not-wf/sa/058.xml", "xmlconf/xmltest/not-wf/sa/059.xml", "xmlconf/xmltest/not-wf/sa/060.xml",
        "xmlconf/xmltest/not-wf/sa/061.xml", "xmlconf/xmltest/not-wf/sa/062.xml", "xmlconf/xmltest/not-wf/sa/064.xml",
        "xmlconf/xmltest/not-wf/sa/065.xml", "xmlconf/xmltest/not-wf/sa/066.xml", "xmlconf/xmltest/not-wf/sa/067.xml",
        "xmlconf/xmltest/not-wf/sa/068.xml", "xmlconf/xmltest/not-wf/sa/069.xml", "xmlconf/xmltest/not-wf/sa/078.xml",
        "xmlconf/xmltest/not-wf/sa/079.xml", "xmlconf/xmltest/not-wf/sa/080.xml", "xmlconf/xmltest/not-wf/sa/082.xml",
        "xmlconf/xmltest/not-wf/sa/084.xml", "xmlconf/xmltest/not-wf/sa/085.xml", "xmlconf/xmltest/not-wf/sa/086.xml",
        "xmlconf/xmltest/not-wf/sa/087.xml", "xmlconf/xmltest/not-wf/sa/089.xml", "xmlconf/xmltest/not-wf/sa/091.xml",
        "xmlconf/xmltest/not-wf/sa/094.xml", "xmlconf/xmltest/not-wf/sa/095.xml", "xmlconf/xmltest/not-wf/sa/096.xml",
        "xmlconf/xmltest/not-wf/sa/097.xml", "xmlconf/xmltest/not-wf/sa/098.xml", "xmlconf/xmltest/not-wf/sa/099.xml",
        "xmlconf/xmltest/not-wf/sa/100.xml", "xmlconf/xmltest/not-wf/sa/101.xml", "xmlconf/xmltest/not-wf/sa/102.xml",
        "xmlconf/xmltest/not-wf/sa/113.xml", "xmlconf/xmltest/not-wf/sa/114.xml", "xmlconf/xmltest/not-wf/sa/121.xml",
        "xmlconf/xmltest/not-wf/sa/121.xml", "xmlconf/xmltest/not-wf/sa/122.xml", "xmlconf/xmltest/not-wf/sa/123.xml",
        "xmlconf/xmltest/not-wf/sa/124.xml", "xmlconf/xmltest/not-wf/sa/125.xml", "xmlconf/xmltest/not-wf/sa/126.xml",
        "xmlconf/xmltest/not-wf/sa/127.xml", "xmlconf/xmltest/not-wf/sa/128.xml", "xmlconf/xmltest/not-wf/sa/129.xml",
        "xmlconf/xmltest/not-wf/sa/130.xml", "xmlconf/xmltest/not-wf/sa/131.xml", "xmlconf/xmltest/not-wf/sa/132.xml",
        "xmlconf/xmltest/not-wf/sa/133.xml", "xmlconf/xmltest/not-wf/sa/134.xml", "xmlconf/xmltest/not-wf/sa/135.xml",
        "xmlconf/xmltest/not-wf/sa/136.xml", "xmlconf/xmltest/not-wf/sa/137.xml", "xmlconf/xmltest/not-wf/sa/138.xml",
        "xmlconf/xmltest/not-wf/sa/139.xml", "xmlconf/xmltest/not-wf/sa/149.xml", "xmlconf/xmltest/not-wf/sa/152.xml",
        "xmlconf/xmltest/not-wf/sa/158.xml", "xmlconf/xmltest/not-wf/sa/160.xml", "xmlconf/xmltest/not-wf/sa/161.xml",
        "xmlconf/xmltest/not-wf/sa/162.xml", "xmlconf/xmltest/not-wf/sa/165.xml", "xmlconf/xmltest/not-wf/sa/175.xml",
        "xmlconf/xmltest/not-wf/sa/180.xml", "xmlconf/xmltest/not-wf/sa/183.xml", "xmlconf/xmltest/not-wf/sa/184.xml",
        "xmlconf/xmltest/not-wf/not-sa/001.xml", "xmlconf/xmltest/not-wf/not-sa/002.xml",
        "xmlconf/xmltest/not-wf/not-sa/003.xml", "xmlconf/xmltest/not-wf/not-sa/004.xml",
        "xmlconf/xmltest/not-wf/not-sa/005.xml", "xmlconf/xmltest/not-wf/not-sa/006.xml",
        "xmlconf/xmltest/not-wf/not-sa/007.xml", "xmlconf/xmltest/not-wf/not-sa/008.xml",
        "xmlconf/xmltest/not-wf/not-sa/009.xml", "xmlconf/xmltest/invalid/002.xml", "xmlconf/xmltest/invalid/005.xml",
        "xmlconf/xmltest/invalid/006.xml", "xmlconf/xmltest/invalid/not-sa/022.xml",
        "xmlconf/xmltest/valid/sa/023.xml", "xmlconf/xmltest/valid/sa/024.xml", "xmlconf/xmltest/valid/sa/053.xml",
        "xmlconf/xmltest/valid/sa/066.xml", "xmlconf/xmltest/valid/sa/068.xml", "xmlconf/xmltest/valid/sa/085.xml",
        "xmlconf/xmltest/valid/sa/086.xml", "xmlconf/xmltest/valid/sa/087.xml", "xmlconf/xmltest/valid/sa/088.xml",
        "xmlconf/xmltest/valid/sa/089.xml", "xmlconf/xmltest/valid/sa/108.xml", "xmlconf/xmltest/valid/sa/110.xml",
        "xmlconf/xmltest/valid/sa/114.xml", "xmlconf/xmltest/valid/sa/115.xml", "xmlconf/xmltest/valid/sa/117.xml",
        "xmlconf/xmltest/valid/sa/118.xml", "xmlconf/xmltest/valid/not-sa/031.xml",
        "xmlconf/xmltest/valid/ext-sa/001.xml", "xmlconf/xmltest/valid/ext-sa/002.xml",
        "xmlconf/xmltest/valid/ext-sa/003.xml", "xmlconf/xmltest/valid/ext-sa/004.xml",
        "xmlconf/xmltest/valid/ext-sa/005.xml", "xmlconf/xmltest/valid/ext-sa/006.xml",
        "xmlconf/xmltest/valid/ext-sa/007.xml", "xmlconf/xmltest/valid/ext-sa/008.xml",
        "xmlconf/xmltest/valid/ext-sa/009.xml", "xmlconf/xmltest/valid/ext-sa/010.xml",
        "xmlconf/xmltest/valid/ext-sa/011.xml", "xmlconf/xmltest/valid/ext-sa/012.xml",
        "xmlconf/xmltest/valid/ext-sa/013.xml", "xmlconf/xmltest/valid/ext-sa/014.xml",
    ];

    validateTests("xmlconf/xmltest/xmltest.xml", ignoreList);
}

void japanese()
{
    auto ignoreList =
    [
        "xmlconf/japanese/pr-xml-little-endian.xml",
        "xmlconf/japanese/pr-xml-utf-16.xml",
        "xmlconf/japanese/pr-xml-utf-8.xml",
        "xmlconf/japanese/weekly-utf-16.xml", // FIXME big-endian
    ];

    validateTests("xmlconf/japanese/japanese.xml", ignoreList);
}

void ibm()
{
    // Unfortunately, because we skip passed the XML declaration and the DTD
    // section, we fail many tests - either because they validate those sections
    // or because they include entity references in the main body of the XML
    // which are not predefined entities (we consider them to be invalid XML,
    // because we can't process them properly without processing the DTD
    // section).
    auto ignoreList =
    [
        "xmlconf/ibm/invalid/P28/ibm28i01.xml", "xmlconf/ibm/invalid/P32/ibm32i01.xml",
        "xmlconf/ibm/invalid/P32/ibm32i03.xml", "xmlconf/ibm/invalid/P32/ibm32i04.xml",
        "xmlconf/ibm/invalid/P39/ibm39i01.xml", "xmlconf/ibm/invalid/P39/ibm39i02.xml",
        "xmlconf/ibm/invalid/P39/ibm39i03.xml", "xmlconf/ibm/invalid/P39/ibm39i04.xml",
        "xmlconf/ibm/invalid/P41/ibm41i01.xml", "xmlconf/ibm/invalid/P41/ibm41i02.xml",
        "xmlconf/ibm/invalid/P45/ibm45i01.xml", "xmlconf/ibm/invalid/P49/ibm49i01.xml",
        "xmlconf/ibm/invalid/P50/ibm50i01.xml", "xmlconf/ibm/invalid/P51/ibm51i01.xml",
        "xmlconf/ibm/invalid/P51/ibm51i03.xml", "xmlconf/ibm/invalid/P56/ibm56i01.xml",
        "xmlconf/ibm/invalid/P56/ibm56i02.xml", "xmlconf/ibm/invalid/P56/ibm56i03.xml",
        "xmlconf/ibm/invalid/P56/ibm56i05.xml", "xmlconf/ibm/invalid/P56/ibm56i06.xml",
        "xmlconf/ibm/invalid/P56/ibm56i07.xml", "xmlconf/ibm/invalid/P56/ibm56i08.xml",
        "xmlconf/ibm/invalid/P56/ibm56i09.xml", "xmlconf/ibm/invalid/P56/ibm56i10.xml",
        "xmlconf/ibm/invalid/P56/ibm56i11.xml", "xmlconf/ibm/invalid/P56/ibm56i12.xml",
        "xmlconf/ibm/invalid/P56/ibm56i13.xml", "xmlconf/ibm/invalid/P56/ibm56i14.xml",
        "xmlconf/ibm/invalid/P56/ibm56i15.xml", "xmlconf/ibm/invalid/P56/ibm56i16.xml",
        "xmlconf/ibm/invalid/P56/ibm56i17.xml", "xmlconf/ibm/invalid/P56/ibm56i18.xml",
        "xmlconf/ibm/invalid/P58/ibm58i01.xml", "xmlconf/ibm/invalid/P58/ibm58i02.xml",
        "xmlconf/ibm/invalid/P59/ibm59i01.xml", "xmlconf/ibm/invalid/P60/ibm60i01.xml",
        "xmlconf/ibm/invalid/P60/ibm60i02.xml", "xmlconf/ibm/invalid/P60/ibm60i03.xml",
        "xmlconf/ibm/invalid/P60/ibm60i04.xml", "xmlconf/ibm/invalid/P68/ibm68i01.xml",
        "xmlconf/ibm/invalid/P68/ibm68i02.xml", "xmlconf/ibm/invalid/P68/ibm68i03.xml",
        "xmlconf/ibm/invalid/P68/ibm68i04.xml", "xmlconf/ibm/invalid/P69/ibm69i01.xml",
        "xmlconf/ibm/invalid/P69/ibm69i02.xml", "xmlconf/ibm/invalid/P69/ibm69i03.xml",
        "xmlconf/ibm/invalid/P69/ibm69i04.xml", "xmlconf/ibm/invalid/P76/ibm76i01.xml",
        "xmlconf/ibm/not-wf/P23/ibm23n01.xml", "xmlconf/ibm/not-wf/P23/ibm23n02.xml",
        "xmlconf/ibm/not-wf/P23/ibm23n03.xml", "xmlconf/ibm/not-wf/P24/ibm24n01.xml",
        "xmlconf/ibm/not-wf/P24/ibm24n03.xml", "xmlconf/ibm/not-wf/P24/ibm24n04.xml",
        "xmlconf/ibm/not-wf/P24/ibm24n05.xml", "xmlconf/ibm/not-wf/P24/ibm24n06.xml",
        "xmlconf/ibm/not-wf/P24/ibm24n07.xml", "xmlconf/ibm/not-wf/P24/ibm24n08.xml",
        "xmlconf/ibm/not-wf/P24/ibm24n09.xml", "xmlconf/ibm/not-wf/P25/ibm25n01.xml",
        "xmlconf/ibm/not-wf/P25/ibm25n02.xml", "xmlconf/ibm/not-wf/P26/ibm26n01.xml",
        "xmlconf/ibm/not-wf/P28/ibm28n01.xml", "xmlconf/ibm/not-wf/P28/ibm28n03.xml",
        "xmlconf/ibm/not-wf/P29/ibm29n01.xml", "xmlconf/ibm/not-wf/P29/ibm29n02.xml",
        "xmlconf/ibm/not-wf/P29/ibm29n03.xml", "xmlconf/ibm/not-wf/P29/ibm29n05.xml",
        "xmlconf/ibm/not-wf/P29/ibm29n06.xml", "xmlconf/ibm/not-wf/P29/ibm29n07.xml",
        "xmlconf/ibm/not-wf/P30/ibm30n01.xml", "xmlconf/ibm/not-wf/P31/ibm31n01.xml",
        "xmlconf/ibm/not-wf/P32/ibm32n01.xml", "xmlconf/ibm/not-wf/P32/ibm32n02.xml",
        "xmlconf/ibm/not-wf/P32/ibm32n03.xml", "xmlconf/ibm/not-wf/P32/ibm32n04.xml",
        "xmlconf/ibm/not-wf/P32/ibm32n05.xml", "xmlconf/ibm/not-wf/P32/ibm32n06.xml",
        "xmlconf/ibm/not-wf/P32/ibm32n07.xml", "xmlconf/ibm/not-wf/P32/ibm32n08.xml",
        "xmlconf/ibm/not-wf/P45/ibm45n01.xml",
    ];

    validateTests("xmlconf/ibm/ibm_oasis_invalid.xml", ignoreList);
    validateTests("xmlconf/ibm/ibm_oasis_not-wf.xml", ignoreList);
    validateTests("xmlconf/ibm/ibm_oasis_valid.xml", ignoreList);
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
