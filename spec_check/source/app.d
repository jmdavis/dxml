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
import std.traits;
import dxml.parser;

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
        "xmlconf/xmltest/not-wf/sa/122.xml", "xmlconf/xmltest/not-wf/sa/123.xml", "xmlconf/xmltest/not-wf/sa/124.xml",
        "xmlconf/xmltest/not-wf/sa/125.xml", "xmlconf/xmltest/not-wf/sa/126.xml", "xmlconf/xmltest/not-wf/sa/127.xml",
        "xmlconf/xmltest/not-wf/sa/128.xml", "xmlconf/xmltest/not-wf/sa/129.xml", "xmlconf/xmltest/not-wf/sa/130.xml",
        "xmlconf/xmltest/not-wf/sa/131.xml", "xmlconf/xmltest/not-wf/sa/132.xml", "xmlconf/xmltest/not-wf/sa/133.xml",
        "xmlconf/xmltest/not-wf/sa/134.xml", "xmlconf/xmltest/not-wf/sa/135.xml", "xmlconf/xmltest/not-wf/sa/136.xml",
        "xmlconf/xmltest/not-wf/sa/137.xml", "xmlconf/xmltest/not-wf/sa/138.xml", "xmlconf/xmltest/not-wf/sa/139.xml",
        "xmlconf/xmltest/not-wf/sa/149.xml", "xmlconf/xmltest/not-wf/sa/152.xml", "xmlconf/xmltest/not-wf/sa/158.xml",
        "xmlconf/xmltest/not-wf/sa/160.xml", "xmlconf/xmltest/not-wf/sa/161.xml", "xmlconf/xmltest/not-wf/sa/162.xml",
        "xmlconf/xmltest/not-wf/sa/165.xml", "xmlconf/xmltest/not-wf/sa/175.xml", "xmlconf/xmltest/not-wf/sa/180.xml",
        "xmlconf/xmltest/not-wf/sa/183.xml", "xmlconf/xmltest/not-wf/sa/184.xml",
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
        "xmlconf/ibm/not-wf/P45/ibm45n01.xml", "xmlconf/ibm/not-wf/P45/ibm45n02.xml",
        "xmlconf/ibm/not-wf/P45/ibm45n03.xml", "xmlconf/ibm/not-wf/P45/ibm45n04.xml",
        "xmlconf/ibm/not-wf/P45/ibm45n05.xml", "xmlconf/ibm/not-wf/P45/ibm45n06.xml",
        "xmlconf/ibm/not-wf/P45/ibm45n07.xml", "xmlconf/ibm/not-wf/P45/ibm45n08.xml",
        "xmlconf/ibm/not-wf/P45/ibm45n09.xml", "xmlconf/ibm/not-wf/P46/ibm46n01.xml",
        "xmlconf/ibm/not-wf/P46/ibm46n02.xml", "xmlconf/ibm/not-wf/P46/ibm46n03.xml",
        "xmlconf/ibm/not-wf/P46/ibm46n04.xml", "xmlconf/ibm/not-wf/P46/ibm46n05.xml",
        "xmlconf/ibm/not-wf/P47/ibm47n01.xml", "xmlconf/ibm/not-wf/P47/ibm47n02.xml",
        "xmlconf/ibm/not-wf/P47/ibm47n03.xml", "xmlconf/ibm/not-wf/P47/ibm47n04.xml",
        "xmlconf/ibm/not-wf/P47/ibm47n05.xml", "xmlconf/ibm/not-wf/P47/ibm47n06.xml",
        "xmlconf/ibm/not-wf/P48/ibm48n01.xml", "xmlconf/ibm/not-wf/P48/ibm48n02.xml",
        "xmlconf/ibm/not-wf/P48/ibm48n03.xml", "xmlconf/ibm/not-wf/P48/ibm48n04.xml",
        "xmlconf/ibm/not-wf/P48/ibm48n05.xml", "xmlconf/ibm/not-wf/P48/ibm48n06.xml",
        "xmlconf/ibm/not-wf/P48/ibm48n07.xml", "xmlconf/ibm/not-wf/P49/ibm49n01.xml",
        "xmlconf/ibm/not-wf/P49/ibm49n02.xml", "xmlconf/ibm/not-wf/P49/ibm49n03.xml",
        "xmlconf/ibm/not-wf/P49/ibm49n04.xml", "xmlconf/ibm/not-wf/P49/ibm49n05.xml",
        "xmlconf/ibm/not-wf/P49/ibm49n06.xml", "xmlconf/ibm/not-wf/P50/ibm50n01.xml",
        "xmlconf/ibm/not-wf/P50/ibm50n02.xml", "xmlconf/ibm/not-wf/P50/ibm50n03.xml",
        "xmlconf/ibm/not-wf/P50/ibm50n04.xml", "xmlconf/ibm/not-wf/P50/ibm50n05.xml",
        "xmlconf/ibm/not-wf/P50/ibm50n06.xml", "xmlconf/ibm/not-wf/P51/ibm51n01.xml",
        "xmlconf/ibm/not-wf/P51/ibm51n02.xml", "xmlconf/ibm/not-wf/P51/ibm51n03.xml",
        "xmlconf/ibm/not-wf/P51/ibm51n04.xml", "xmlconf/ibm/not-wf/P51/ibm51n07.xml",
        "xmlconf/ibm/not-wf/P52/ibm52n01.xml", "xmlconf/ibm/not-wf/P52/ibm52n02.xml",
        "xmlconf/ibm/not-wf/P52/ibm52n03.xml", "xmlconf/ibm/not-wf/P52/ibm52n04.xml",
        "xmlconf/ibm/not-wf/P52/ibm52n05.xml", "xmlconf/ibm/not-wf/P52/ibm52n06.xml",
        "xmlconf/ibm/not-wf/P53/ibm53n01.xml", "xmlconf/ibm/not-wf/P53/ibm53n02.xml",
        "xmlconf/ibm/not-wf/P53/ibm53n03.xml", "xmlconf/ibm/not-wf/P53/ibm53n04.xml",
        "xmlconf/ibm/not-wf/P53/ibm53n05.xml", "xmlconf/ibm/not-wf/P53/ibm53n06.xml",
        "xmlconf/ibm/not-wf/P53/ibm53n07.xml", "xmlconf/ibm/not-wf/P53/ibm53n08.xml",
        "xmlconf/ibm/not-wf/P54/ibm54n01.xml", "xmlconf/ibm/not-wf/P54/ibm54n02.xml",
        "xmlconf/ibm/not-wf/P55/ibm55n01.xml", "xmlconf/ibm/not-wf/P55/ibm55n02.xml",
        "xmlconf/ibm/not-wf/P55/ibm55n03.xml", "xmlconf/ibm/not-wf/P56/ibm56n01.xml",
        "xmlconf/ibm/not-wf/P56/ibm56n02.xml", "xmlconf/ibm/not-wf/P56/ibm56n03.xml",
        "xmlconf/ibm/not-wf/P56/ibm56n04.xml", "xmlconf/ibm/not-wf/P56/ibm56n05.xml",
        "xmlconf/ibm/not-wf/P56/ibm56n06.xml", "xmlconf/ibm/not-wf/P56/ibm56n07.xml",
        "xmlconf/ibm/not-wf/P57/ibm57n01.xml", "xmlconf/ibm/not-wf/P58/ibm58n01.xml",
        "xmlconf/ibm/not-wf/P58/ibm58n02.xml", "xmlconf/ibm/not-wf/P58/ibm58n03.xml",
        "xmlconf/ibm/not-wf/P58/ibm58n04.xml", "xmlconf/ibm/not-wf/P58/ibm58n05.xml",
        "xmlconf/ibm/not-wf/P58/ibm58n06.xml", "xmlconf/ibm/not-wf/P58/ibm58n07.xml",
        "xmlconf/ibm/not-wf/P58/ibm58n08.xml", "xmlconf/ibm/not-wf/P59/ibm59n01.xml",
        "xmlconf/ibm/not-wf/P59/ibm59n02.xml", "xmlconf/ibm/not-wf/P59/ibm59n03.xml",
        "xmlconf/ibm/not-wf/P59/ibm59n04.xml", "xmlconf/ibm/not-wf/P59/ibm59n05.xml",
        "xmlconf/ibm/not-wf/P59/ibm59n06.xml", "xmlconf/ibm/not-wf/P60/ibm60n01.xml",
        "xmlconf/ibm/not-wf/P60/ibm60n02.xml", "xmlconf/ibm/not-wf/P60/ibm60n03.xml",
        "xmlconf/ibm/not-wf/P60/ibm60n04.xml", "xmlconf/ibm/not-wf/P60/ibm60n05.xml",
        "xmlconf/ibm/not-wf/P60/ibm60n06.xml", "xmlconf/ibm/not-wf/P60/ibm60n08.xml",
        "xmlconf/ibm/not-wf/P61/ibm61n01.xml", "xmlconf/ibm/not-wf/P62/ibm62n01.xml",
        "xmlconf/ibm/not-wf/P62/ibm62n02.xml", "xmlconf/ibm/not-wf/P62/ibm62n03.xml",
        "xmlconf/ibm/not-wf/P62/ibm62n04.xml", "xmlconf/ibm/not-wf/P62/ibm62n05.xml",
        "xmlconf/ibm/not-wf/P62/ibm62n06.xml", "xmlconf/ibm/not-wf/P62/ibm62n07.xml",
        "xmlconf/ibm/not-wf/P62/ibm62n08.xml", "xmlconf/ibm/not-wf/P63/ibm63n01.xml",
        "xmlconf/ibm/not-wf/P63/ibm63n02.xml", "xmlconf/ibm/not-wf/P63/ibm63n03.xml",
        "xmlconf/ibm/not-wf/P63/ibm63n04.xml", "xmlconf/ibm/not-wf/P63/ibm63n05.xml",
        "xmlconf/ibm/not-wf/P63/ibm63n06.xml", "xmlconf/ibm/not-wf/P63/ibm63n07.xml",
        "xmlconf/ibm/not-wf/P64/ibm64n01.xml", "xmlconf/ibm/not-wf/P64/ibm64n02.xml",
        "xmlconf/ibm/not-wf/P64/ibm64n03.xml", "xmlconf/ibm/not-wf/P65/ibm65n01.xml",
        "xmlconf/ibm/not-wf/P65/ibm65n02.xml", "xmlconf/ibm/not-wf/P66/ibm66n01.xml",
        "xmlconf/ibm/not-wf/P66/ibm66n03.xml", "xmlconf/ibm/not-wf/P66/ibm66n05.xml",
        "xmlconf/ibm/not-wf/P66/ibm66n07.xml", "xmlconf/ibm/not-wf/P66/ibm66n09.xml",
        "xmlconf/ibm/not-wf/P66/ibm66n11.xml", "xmlconf/ibm/not-wf/P68/ibm68n07.xml",
        "xmlconf/ibm/not-wf/P69/ibm69n01.xml", "xmlconf/ibm/not-wf/P69/ibm69n02.xml",
        "xmlconf/ibm/not-wf/P69/ibm69n03.xml", "xmlconf/ibm/not-wf/P69/ibm69n04.xml",
        "xmlconf/ibm/not-wf/P69/ibm69n05.xml", "xmlconf/ibm/not-wf/P69/ibm69n06.xml",
        "xmlconf/ibm/not-wf/P69/ibm69n07.xml", "xmlconf/ibm/not-wf/P71/ibm70n01.xml",
        "xmlconf/ibm/not-wf/P72/ibm72n01.xml", "xmlconf/ibm/not-wf/P72/ibm72n02.xml",
        "xmlconf/ibm/not-wf/P72/ibm72n03.xml", "xmlconf/ibm/not-wf/P72/ibm72n04.xml",
        "xmlconf/ibm/not-wf/P72/ibm72n05.xml", "xmlconf/ibm/not-wf/P72/ibm72n06.xml",
        "xmlconf/ibm/not-wf/P72/ibm72n07.xml", "xmlconf/ibm/not-wf/P72/ibm72n08.xml",
        "xmlconf/ibm/not-wf/P72/ibm72n09.xml", "xmlconf/ibm/not-wf/P73/ibm73n01.xml",
        "xmlconf/ibm/not-wf/P73/ibm73n03.xml", "xmlconf/ibm/not-wf/P74/ibm74n01.xml",
        "xmlconf/ibm/not-wf/P75/ibm75n01.xml", "xmlconf/ibm/not-wf/P75/ibm75n02.xml",
        "xmlconf/ibm/not-wf/P75/ibm75n03.xml", "xmlconf/ibm/not-wf/P75/ibm75n04.xml",
        "xmlconf/ibm/not-wf/P75/ibm75n05.xml", "xmlconf/ibm/not-wf/P75/ibm75n06.xml",
        "xmlconf/ibm/not-wf/P75/ibm75n07.xml", "xmlconf/ibm/not-wf/P75/ibm75n08.xml",
        "xmlconf/ibm/not-wf/P75/ibm75n09.xml", "xmlconf/ibm/not-wf/P75/ibm75n10.xml",
        "xmlconf/ibm/not-wf/P75/ibm75n11.xml", "xmlconf/ibm/not-wf/P75/ibm75n12.xml",
        "xmlconf/ibm/not-wf/P75/ibm75n13.xml", "xmlconf/ibm/not-wf/P76/ibm76n01.xml",
        "xmlconf/ibm/not-wf/P76/ibm76n02.xml", "xmlconf/ibm/not-wf/P76/ibm76n03.xml",
        "xmlconf/ibm/not-wf/P76/ibm76n04.xml", "xmlconf/ibm/not-wf/P76/ibm76n05.xml",
        "xmlconf/ibm/not-wf/P76/ibm76n06.xml", "xmlconf/ibm/not-wf/P76/ibm76n07.xml",
        "xmlconf/ibm/not-wf/P77/ibm77n03.xml", "xmlconf/ibm/not-wf/P77/ibm77n04.xml",
        "xmlconf/ibm/not-wf/P79/ibm79n01.xml", "xmlconf/ibm/not-wf/P79/ibm79n02.xml",
        "xmlconf/ibm/not-wf/P80/ibm80n01.xml", "xmlconf/ibm/not-wf/P80/ibm80n02.xml",
        "xmlconf/ibm/not-wf/P80/ibm80n03.xml", "xmlconf/ibm/not-wf/P80/ibm80n04.xml",
        "xmlconf/ibm/not-wf/P80/ibm80n05.xml", "xmlconf/ibm/not-wf/P80/ibm80n06.xml",
        "xmlconf/ibm/not-wf/P81/ibm81n01.xml", "xmlconf/ibm/not-wf/P81/ibm81n02.xml",
        "xmlconf/ibm/not-wf/P81/ibm81n03.xml", "xmlconf/ibm/not-wf/P81/ibm81n04.xml",
        "xmlconf/ibm/not-wf/P81/ibm81n05.xml", "xmlconf/ibm/not-wf/P81/ibm81n06.xml",
        "xmlconf/ibm/not-wf/P81/ibm81n07.xml", "xmlconf/ibm/not-wf/P81/ibm81n08.xml",
        "xmlconf/ibm/not-wf/P81/ibm81n09.xml", "xmlconf/ibm/not-wf/P82/ibm82n01.xml",
        "xmlconf/ibm/not-wf/P82/ibm82n02.xml", "xmlconf/ibm/not-wf/P82/ibm82n03.xml",
        "xmlconf/ibm/not-wf/P82/ibm82n04.xml", "xmlconf/ibm/not-wf/P82/ibm82n05.xml",
        "xmlconf/ibm/not-wf/P82/ibm82n06.xml", "xmlconf/ibm/not-wf/P82/ibm82n07.xml",
        "xmlconf/ibm/not-wf/P82/ibm82n08.xml", "xmlconf/ibm/not-wf/P83/ibm83n01.xml",
        "xmlconf/ibm/not-wf/P83/ibm83n03.xml", "xmlconf/ibm/not-wf/P83/ibm83n04.xml",
        "xmlconf/ibm/not-wf/P83/ibm83n05.xml", "xmlconf/ibm/not-wf/P83/ibm83n06.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n01.xml", "xmlconf/ibm/not-wf/P85/ibm85n02.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n03.xml", "xmlconf/ibm/not-wf/P85/ibm85n04.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n05.xml", "xmlconf/ibm/not-wf/P85/ibm85n06.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n07.xml", "xmlconf/ibm/not-wf/P85/ibm85n08.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n09.xml", "xmlconf/ibm/not-wf/P85/ibm85n10.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n100.xml", "xmlconf/ibm/not-wf/P85/ibm85n101.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n102.xml", "xmlconf/ibm/not-wf/P85/ibm85n103.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n104.xml", "xmlconf/ibm/not-wf/P85/ibm85n105.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n106.xml", "xmlconf/ibm/not-wf/P85/ibm85n107.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n108.xml", "xmlconf/ibm/not-wf/P85/ibm85n109.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n11.xml", "xmlconf/ibm/not-wf/P85/ibm85n110.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n111.xml", "xmlconf/ibm/not-wf/P85/ibm85n112.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n113.xml", "xmlconf/ibm/not-wf/P85/ibm85n114.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n115.xml", "xmlconf/ibm/not-wf/P85/ibm85n116.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n117.xml", "xmlconf/ibm/not-wf/P85/ibm85n118.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n119.xml", "xmlconf/ibm/not-wf/P85/ibm85n12.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n120.xml", "xmlconf/ibm/not-wf/P85/ibm85n121.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n122.xml", "xmlconf/ibm/not-wf/P85/ibm85n123.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n124.xml", "xmlconf/ibm/not-wf/P85/ibm85n125.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n126.xml", "xmlconf/ibm/not-wf/P85/ibm85n127.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n128.xml", "xmlconf/ibm/not-wf/P85/ibm85n129.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n13.xml", "xmlconf/ibm/not-wf/P85/ibm85n130.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n131.xml", "xmlconf/ibm/not-wf/P85/ibm85n132.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n133.xml", "xmlconf/ibm/not-wf/P85/ibm85n134.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n135.xml", "xmlconf/ibm/not-wf/P85/ibm85n136.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n137.xml", "xmlconf/ibm/not-wf/P85/ibm85n138.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n139.xml", "xmlconf/ibm/not-wf/P85/ibm85n14.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n140.xml", "xmlconf/ibm/not-wf/P85/ibm85n141.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n142.xml", "xmlconf/ibm/not-wf/P85/ibm85n143.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n144.xml", "xmlconf/ibm/not-wf/P85/ibm85n145.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n146.xml", "xmlconf/ibm/not-wf/P85/ibm85n147.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n148.xml", "xmlconf/ibm/not-wf/P85/ibm85n149.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n15.xml", "xmlconf/ibm/not-wf/P85/ibm85n150.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n151.xml", "xmlconf/ibm/not-wf/P85/ibm85n152.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n153.xml", "xmlconf/ibm/not-wf/P85/ibm85n154.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n155.xml", "xmlconf/ibm/not-wf/P85/ibm85n156.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n157.xml", "xmlconf/ibm/not-wf/P85/ibm85n158.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n159.xml", "xmlconf/ibm/not-wf/P85/ibm85n16.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n160.xml", "xmlconf/ibm/not-wf/P85/ibm85n161.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n162.xml", "xmlconf/ibm/not-wf/P85/ibm85n163.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n164.xml", "xmlconf/ibm/not-wf/P85/ibm85n165.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n166.xml", "xmlconf/ibm/not-wf/P85/ibm85n167.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n168.xml", "xmlconf/ibm/not-wf/P85/ibm85n169.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n17.xml", "xmlconf/ibm/not-wf/P85/ibm85n170.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n171.xml", "xmlconf/ibm/not-wf/P85/ibm85n172.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n173.xml", "xmlconf/ibm/not-wf/P85/ibm85n174.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n175.xml", "xmlconf/ibm/not-wf/P85/ibm85n176.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n177.xml", "xmlconf/ibm/not-wf/P85/ibm85n178.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n179.xml", "xmlconf/ibm/not-wf/P85/ibm85n18.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n180.xml", "xmlconf/ibm/not-wf/P85/ibm85n181.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n182.xml", "xmlconf/ibm/not-wf/P85/ibm85n183.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n184.xml", "xmlconf/ibm/not-wf/P85/ibm85n185.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n186.xml", "xmlconf/ibm/not-wf/P85/ibm85n187.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n188.xml", "xmlconf/ibm/not-wf/P85/ibm85n189.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n19.xml", "xmlconf/ibm/not-wf/P85/ibm85n190.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n191.xml", "xmlconf/ibm/not-wf/P85/ibm85n192.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n193.xml", "xmlconf/ibm/not-wf/P85/ibm85n194.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n195.xml", "xmlconf/ibm/not-wf/P85/ibm85n196.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n197.xml", "xmlconf/ibm/not-wf/P85/ibm85n198.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n20.xml", "xmlconf/ibm/not-wf/P85/ibm85n21.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n22.xml", "xmlconf/ibm/not-wf/P85/ibm85n23.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n24.xml", "xmlconf/ibm/not-wf/P85/ibm85n25.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n26.xml", "xmlconf/ibm/not-wf/P85/ibm85n27.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n28.xml", "xmlconf/ibm/not-wf/P85/ibm85n29.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n30.xml", "xmlconf/ibm/not-wf/P85/ibm85n31.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n32.xml", "xmlconf/ibm/not-wf/P85/ibm85n33.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n34.xml", "xmlconf/ibm/not-wf/P85/ibm85n35.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n36.xml", "xmlconf/ibm/not-wf/P85/ibm85n37.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n38.xml", "xmlconf/ibm/not-wf/P85/ibm85n39.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n40.xml", "xmlconf/ibm/not-wf/P85/ibm85n41.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n42.xml", "xmlconf/ibm/not-wf/P85/ibm85n43.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n44.xml", "xmlconf/ibm/not-wf/P85/ibm85n45.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n46.xml", "xmlconf/ibm/not-wf/P85/ibm85n47.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n48.xml", "xmlconf/ibm/not-wf/P85/ibm85n49.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n50.xml", "xmlconf/ibm/not-wf/P85/ibm85n51.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n52.xml", "xmlconf/ibm/not-wf/P85/ibm85n53.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n54.xml", "xmlconf/ibm/not-wf/P85/ibm85n55.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n56.xml", "xmlconf/ibm/not-wf/P85/ibm85n57.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n58.xml", "xmlconf/ibm/not-wf/P85/ibm85n59.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n60.xml", "xmlconf/ibm/not-wf/P85/ibm85n61.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n62.xml", "xmlconf/ibm/not-wf/P85/ibm85n63.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n64.xml", "xmlconf/ibm/not-wf/P85/ibm85n65.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n66.xml", "xmlconf/ibm/not-wf/P85/ibm85n67.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n68.xml", "xmlconf/ibm/not-wf/P85/ibm85n69.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n70.xml", "xmlconf/ibm/not-wf/P85/ibm85n71.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n72.xml", "xmlconf/ibm/not-wf/P85/ibm85n73.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n74.xml", "xmlconf/ibm/not-wf/P85/ibm85n75.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n76.xml", "xmlconf/ibm/not-wf/P85/ibm85n77.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n78.xml", "xmlconf/ibm/not-wf/P85/ibm85n79.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n80.xml", "xmlconf/ibm/not-wf/P85/ibm85n81.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n82.xml", "xmlconf/ibm/not-wf/P85/ibm85n83.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n84.xml", "xmlconf/ibm/not-wf/P85/ibm85n85.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n86.xml", "xmlconf/ibm/not-wf/P85/ibm85n87.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n88.xml", "xmlconf/ibm/not-wf/P85/ibm85n89.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n90.xml", "xmlconf/ibm/not-wf/P85/ibm85n91.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n92.xml", "xmlconf/ibm/not-wf/P85/ibm85n93.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n94.xml", "xmlconf/ibm/not-wf/P85/ibm85n95.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n96.xml", "xmlconf/ibm/not-wf/P85/ibm85n97.xml",
        "xmlconf/ibm/not-wf/P85/ibm85n98.xml", "xmlconf/ibm/not-wf/P85/ibm85n99.xml",
        "xmlconf/ibm/not-wf/P86/ibm86n01.xml", "xmlconf/ibm/not-wf/P86/ibm86n02.xml",
        "xmlconf/ibm/not-wf/P86/ibm86n03.xml", "xmlconf/ibm/not-wf/P86/ibm86n04.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n01.xml", "xmlconf/ibm/not-wf/P87/ibm87n02.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n03.xml", "xmlconf/ibm/not-wf/P87/ibm87n04.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n05.xml", "xmlconf/ibm/not-wf/P87/ibm87n06.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n07.xml", "xmlconf/ibm/not-wf/P87/ibm87n08.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n09.xml", "xmlconf/ibm/not-wf/P87/ibm87n10.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n11.xml", "xmlconf/ibm/not-wf/P87/ibm87n12.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n13.xml", "xmlconf/ibm/not-wf/P87/ibm87n14.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n15.xml", "xmlconf/ibm/not-wf/P87/ibm87n16.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n17.xml", "xmlconf/ibm/not-wf/P87/ibm87n18.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n19.xml", "xmlconf/ibm/not-wf/P87/ibm87n20.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n21.xml", "xmlconf/ibm/not-wf/P87/ibm87n22.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n23.xml", "xmlconf/ibm/not-wf/P87/ibm87n24.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n25.xml", "xmlconf/ibm/not-wf/P87/ibm87n26.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n27.xml", "xmlconf/ibm/not-wf/P87/ibm87n28.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n29.xml", "xmlconf/ibm/not-wf/P87/ibm87n30.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n31.xml", "xmlconf/ibm/not-wf/P87/ibm87n32.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n33.xml", "xmlconf/ibm/not-wf/P87/ibm87n34.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n35.xml", "xmlconf/ibm/not-wf/P87/ibm87n36.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n37.xml", "xmlconf/ibm/not-wf/P87/ibm87n38.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n39.xml", "xmlconf/ibm/not-wf/P87/ibm87n40.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n41.xml", "xmlconf/ibm/not-wf/P87/ibm87n42.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n43.xml", "xmlconf/ibm/not-wf/P87/ibm87n44.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n45.xml", "xmlconf/ibm/not-wf/P87/ibm87n46.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n47.xml", "xmlconf/ibm/not-wf/P87/ibm87n48.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n49.xml", "xmlconf/ibm/not-wf/P87/ibm87n50.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n51.xml", "xmlconf/ibm/not-wf/P87/ibm87n52.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n53.xml", "xmlconf/ibm/not-wf/P87/ibm87n54.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n55.xml", "xmlconf/ibm/not-wf/P87/ibm87n56.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n57.xml", "xmlconf/ibm/not-wf/P87/ibm87n58.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n59.xml", "xmlconf/ibm/not-wf/P87/ibm87n60.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n61.xml", "xmlconf/ibm/not-wf/P87/ibm87n62.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n63.xml", "xmlconf/ibm/not-wf/P87/ibm87n64.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n66.xml", "xmlconf/ibm/not-wf/P87/ibm87n67.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n68.xml", "xmlconf/ibm/not-wf/P87/ibm87n69.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n70.xml", "xmlconf/ibm/not-wf/P87/ibm87n71.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n72.xml", "xmlconf/ibm/not-wf/P87/ibm87n73.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n74.xml", "xmlconf/ibm/not-wf/P87/ibm87n75.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n76.xml", "xmlconf/ibm/not-wf/P87/ibm87n77.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n78.xml", "xmlconf/ibm/not-wf/P87/ibm87n79.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n80.xml", "xmlconf/ibm/not-wf/P87/ibm87n81.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n82.xml", "xmlconf/ibm/not-wf/P87/ibm87n83.xml",
        "xmlconf/ibm/not-wf/P87/ibm87n84.xml", "xmlconf/ibm/not-wf/P87/ibm87n85.xml",
        "xmlconf/ibm/not-wf/P88/ibm88n01.xml", "xmlconf/ibm/not-wf/P88/ibm88n02.xml",
        "xmlconf/ibm/not-wf/P88/ibm88n03.xml", "xmlconf/ibm/not-wf/P88/ibm88n04.xml",
        "xmlconf/ibm/not-wf/P88/ibm88n05.xml", "xmlconf/ibm/not-wf/P88/ibm88n06.xml",
        "xmlconf/ibm/not-wf/P88/ibm88n08.xml", "xmlconf/ibm/not-wf/P88/ibm88n09.xml",
        "xmlconf/ibm/not-wf/P88/ibm88n10.xml", "xmlconf/ibm/not-wf/P88/ibm88n11.xml",
        "xmlconf/ibm/not-wf/P88/ibm88n12.xml", "xmlconf/ibm/not-wf/P88/ibm88n13.xml",
        "xmlconf/ibm/not-wf/P88/ibm88n14.xml", "xmlconf/ibm/not-wf/P88/ibm88n15.xml",
        "xmlconf/ibm/not-wf/P88/ibm88n16.xml", "xmlconf/ibm/not-wf/P89/ibm89n01.xml",
        "xmlconf/ibm/not-wf/P89/ibm89n02.xml", "xmlconf/ibm/not-wf/P89/ibm89n03.xml",
        "xmlconf/ibm/not-wf/P89/ibm89n04.xml", "xmlconf/ibm/not-wf/P89/ibm89n05.xml",
        "xmlconf/ibm/valid/P09/ibm09v01.xml", "xmlconf/ibm/valid/P09/ibm09v02.xml",
        "xmlconf/ibm/valid/P09/ibm09v03.xml", "xmlconf/ibm/valid/P09/ibm09v04.xml",
        "xmlconf/ibm/valid/P09/ibm09v05.xml", "xmlconf/ibm/valid/P10/ibm10v01.xml",
        "xmlconf/ibm/valid/P10/ibm10v02.xml", "xmlconf/ibm/valid/P10/ibm10v03.xml",
        "xmlconf/ibm/valid/P10/ibm10v04.xml", "xmlconf/ibm/valid/P10/ibm10v05.xml",
        "xmlconf/ibm/valid/P10/ibm10v06.xml", "xmlconf/ibm/valid/P10/ibm10v07.xml",
        "xmlconf/ibm/valid/P10/ibm10v08.xml", "xmlconf/ibm/valid/P28/ibm28v02.xml",
        "xmlconf/ibm/valid/P29/ibm29v01.xml", "xmlconf/ibm/valid/P29/ibm29v02.xml",
        "xmlconf/ibm/valid/P32/ibm32v02.xml", "xmlconf/ibm/valid/P43/ibm43v01.xml",
        "xmlconf/ibm/valid/P67/ibm67v01.xml", "xmlconf/ibm/valid/P78/ibm78v01.xml",
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
    import std.encoding : BOM, getBOM;
    import std.exception : assertNotThrown, enforce;
    import std.format : format;
    import std.file : exists, read, readText;
    import std.utf : UTFException;

    enforce(file.exists, format("%s does not exist", file));
    immutable bom = getBOM(cast(ubyte[])read(file)).schema;
    // FIXME This only works properly with little-endian machines right now, and 
    // we need a way to process big endian encodings.
    switch(bom)
    {
        case BOM.none:
        case BOM.utf8:
        {
            auto text = stripBOM(file.readText());
            assertNotThrown!XMLParsingException(parseEverything(text), format("%s failed", file));
            break;
        }
        case BOM.utf16le:
        {
            auto text = stripBOM(file.readText!wstring());
            assertNotThrown!XMLParsingException(parseEverything(text), format("%s failed", file));
            break;
        }
        default: throw new Exception(format("Unsupported encoding: %s, file: %s", bom, file));
    }
}

void parseExpectFailure(string file)
{
    import std.encoding : BOM, getBOM;
    import std.exception : assertThrown, enforce;
    import std.format : format;
    import std.file : exists, read;
    enforce(file.exists, format("%s does not exist", file));

    immutable bom = getBOM(cast(ubyte[])read(file)).schema;
    // FIXME This only works properly with little-endian machines right now, and 
    // we need a way to process big endian encodings.
    switch(bom)
    {
        case BOM.none:
        case BOM.utf8:
        {
            auto text = stripBOM(cast(string)file.read());
            assertThrown!XMLParsingException(parseEverything(text), format("%s failed", file));
            break;
        }
        case BOM.utf16le:
        {
            auto text = stripBOM(cast(wstring)file.read());
            assertThrown!XMLParsingException(parseEverything(text), format("%s failed", file));
            break;
        }
        default: throw new Exception(format("Unsupported encoding: %s, file: %s", bom, file));
    }
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


// To be replaced by std.utf.stripBOM once it's available.
R stripBOM(R)(R range)
if (isForwardRange!R && isSomeChar!(ElementType!R))
{
    import std.utf : decodeFront, UseReplacementDchar;
    if (range.empty)
        return range;
    auto orig = range.save;
    immutable c = range.decodeFront!(UseReplacementDchar.yes)();
    return c == '\uFEFF' ? range : orig;
}
