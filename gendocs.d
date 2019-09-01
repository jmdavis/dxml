#!/usr/bin/env rdmd
// Written in the D programming language

/++
    This is a program for simplifying ddoc generation.

    It ensures that the names of the generated .html files include the full
    module path (with underscores instead of dots) rather than simply being
    named after the modules (since just using the module names results in
    connflicts if any packages have modules with the same name).

    It also provides an easy way to exclude files from ddoc generation. Any
    modules or packages with the name internal are excluded as well as any
    files that are passed on the command line. And package.d files have their
    corresponding .html files renamed to match the package name.

    Also, the program generates a .ddoc file intended for use in a navigation
    bar on the side of the documentation (similar to what dlang.org has) uses
    it in the ddoc generation (it's deleted afterwards). The navigation bar
    contains the full module hierarchy to allow for easy navigation among the
    modules in the project. Of course, the other .ddoc files have to actually
    use the MODULE_MENU macro in the generated .ddoc file, or the documentation
    won't end up with a navigation bar.

    The program assumes a layout similar to dub in that the source files are
    expected to be in a directory called "source", and the generated
    documentation goes in the "docs" directory (which is deleted before
    documentation generation to ensure a clean build).

    It's expected that any .ddoc files being used will be in the "ddoc"
    directory, which isn't a dub thing, but they have to go somewhere.

    In addition, the program expects there to be a "source_docs" directory. Any
    .dd files that are there will have corresponding .html files generated for
    them (e.g. for generating index.html), and any other files or directories
    (e.g. a "css" or "js" folder) will be copied over to the "docs" folder.

    Note that this program does assume that all module names match their file
    names and that all package names match their folder names.

    Copyright: Copyright 2017 - 2019
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Author:   Jonathan M Davis
  +/
module gendocs;

import std.algorithm : canFind;
import std.array : appender, array, replace;
import std.file : dirEntries, mkdir, SpanMode;
import std.format : format;
import std.path : baseName, buildPath, dirName, extension, setExtension, stripExtension;
import std.range.primitives;

enum sourceDir = "source";
enum docsDir = "docs";
enum ddocDir = "ddoc";
enum sourceDocsDir = "source_docs";

int main(string[] args)
{
    import std.exception : enforce;
    import std.file : exists, remove, rmdirRecurse;

    try
    {
        auto excludes = args[1 .. $];

        enforce(ddocDir.exists, "ddoc directory is missing");
        enforce(sourceDocsDir.exists, "source_docs directory is missing");

        foreach(exclude; excludes)
        {
            auto file = buildPath(sourceDir, exclude);
            enforce(file.exists, format("%s does not exist", file));
        }

        if(docsDir.exists)
            rmdirRecurse(docsDir);
        mkdir(docsDir);

        auto moduleListDdoc = genModuleListDdoc(excludes);
        scope(exit) remove(moduleListDdoc);

        auto ddocFiles = getDdocFiles();
        processSourceDocsDir(sourceDocsDir, docsDir, ddocFiles);
        processSourceDir(sourceDir, docsDir, excludes, ddocFiles);
    }
    catch(Exception e)
    {
        import std.stdio : stderr, writeln;
        stderr.writeln(e.msg);
        return -1;
    }

    return 0;
}

void processSourceDocsDir(string sourceDir, string targetDir, string[] ddocFiles)
{
    import std.file : copy;

    foreach(de; dirEntries(sourceDir, SpanMode.shallow))
    {
        auto target = buildPath(targetDir, de.baseName);
        if(de.isDir)
        {
            mkdir(target);
            processSourceDocsDir(de.name, target, ddocFiles);
        }
        else if(de.isFile)
        {
            if(de.name.extension == ".dd")
                genDdoc(de.name, target.setExtension(".html"), ddocFiles);
            else
                copy(de.name, target);
        }
    }
}

void processSourceDir(string sourceDir, string target, string[] excludes, string[] ddocFiles, int depth = 0)
{
    import std.algorithm : endsWith;

    if(depth == 0 && !target.endsWith("/"))
        target ~= "/";

    foreach(de; dirEntries(sourceDir, SpanMode.shallow))
    {
        auto name = de.baseName;
        if(name.stripExtension == "internal" || excludes.canFind(de.baseName))
            continue;
        auto nextTarget = name == "package.d" ? target : format("%s%s%s", target, depth == 0 ? "" : "_", name);
        if(de.isDir)
            processSourceDir(de.name, nextTarget, excludes, ddocFiles, depth + 1);
        else if(de.isFile)
            genDdoc(de.name, nextTarget.setExtension(".html"), ddocFiles);
    }
}

void genDdoc(string sourceFile, string htmlFile, string[] ddocFiles)
{
    import std.process : execute;
    auto result = execute(["dmd", "-o-", "-Isource/", "-Df" ~ htmlFile, sourceFile] ~ ddocFiles);
    if(result.status != 0)
        throw new Exception("dmd failed:\n" ~ result.output);
}

string[] getDdocFiles()
{
    import std.algorithm : map;
    return dirEntries("ddoc", SpanMode.shallow).map!(a => a.name)().array();
}

string genModuleListDdoc(string[] excludes)
{
    import std.array : join;
    import std.file : write;

    auto lines = appender!(string[])();
    put(lines, "MODULE_MENU=");

    auto modules = getModules(sourceDir, excludes);
    genModuleMenu(lines, modules);
    put(lines, "_=");

    put(lines, "");
    put(lines, `MENU_MODULE=$(A $2$(UNDERSCORE_PREFIXED_SKIP $+).html, $(SPAN, $1))`);
    put(lines, "MENU_PKG=$(LIC expand-container open, $(AC expand-toggle, #, $(SPAN, $1))$(ITEMIZE $+))");
    put(lines, "_=");

    put(lines, "");
    put(lines, "MODULE_INDEX=");
    genModuleIndex(lines, modules);
    put(lines, "_=");

    put(lines, "");
    put(lines, `INDEX_MODULE=$(A $1$(UNDERSCORE_PREFIXED $+).html, ` ~
               `$(SPANC module_index, $1$(DOT_PREFIXED $+)))$(DDOC_BLANKLINE)`);
    put(lines, "_=");

    auto moduleListDDoc = buildPath(ddocDir, "module_list.ddoc");
    write(moduleListDDoc, lines.data.join("\n"));
    return moduleListDDoc;
}

void genModuleMenu(OR)(ref OR lines, Package* pkg, int depth = 0)
{
    import std.array : replicate;

    auto outerIndent = "    ".replicate(depth == 0 ? 0 : depth - 1);
    auto innerIndent = "    ".replicate(depth == 0 ? 0 : depth);

    if(depth != 0)
        put(lines, format("%s$(MENU_PKG %s,", outerIndent, pkg.path.baseName));

    foreach(modPath; pkg.modules)
    {
        auto modName = modPath.baseName.stripExtension();
        if(modName == "package")
            modPath = modPath.dirName;
        auto modPieces = modPath.replace("/", ", ").stripExtension();
        put(lines, format("%s$(MENU_MODULE %s, %s)", innerIndent, modName, modPieces));
    }

    foreach(subPkg; pkg.packages)
        genModuleMenu(lines, subPkg, depth + 1);

    if(depth != 0)
        put(lines, format("%s)", outerIndent));
}

void genModuleIndex(OR)(ref OR lines, Package* pkg, int depth = 0)
{
    import std.algorithm : filter;
    import std.array : replicate;

    static string genListModule(string modPath)
    {
        auto modName = modPath.baseName.stripExtension();
        auto modPieces = modPath.replace("/", ", ").stripExtension();
        return format("$(INDEX_MODULE %s)", modPieces);
    }

    if(depth != 0 && pkg.hasPackageD)
        put(lines, genListModule(pkg.path));

    foreach(mod; pkg.modules.filter!(a => a.baseName != "package.d")())
        put(lines, genListModule(mod));

    foreach(subPkg; pkg.packages)
        genModuleIndex(lines, subPkg, depth + 1);
}

struct Package
{
    string path;
    bool hasPackageD;
    string[] modules;
    Package*[] packages;
}

Package* getModules(string dir, string[] excludes, int depth = 0)
{
    import std.algorithm : sort;

    string path;
    bool hasPackageD;
    auto modules = appender!(string[])();
    auto packages = appender!(Package*[])();

    if(depth != 0)
        path = stripSourceDir(dir);

    foreach(de; dirEntries(dir, SpanMode.shallow))
    {
        auto stripped = stripSourceDir(de.name);
        if(excludes.canFind(stripped))
            continue;
        if(de.isDir)
        {
            if(stripped.baseName == "internal")
                continue;
            if(auto subPackage = getModules(de.name, excludes, depth + 1))
                put(packages, subPackage);
        }
        else if(de.isFile)
        {
            if(stripped.baseName == "internal.d")
                continue;
            if(stripped.baseName == "package.d")
                hasPackageD = true;
            modules.put(stripped);
        }
    }

    if(modules.data.empty && packages.data.empty)
        return null;

    auto pkg = new Package(path, hasPackageD, modules.data, packages.data);
    sort(pkg.modules);
    sort!((a, b) => a.path < b.path)(pkg.packages);

    return pkg;
}

string stripSourceDir(string path)
{
    import std.algorithm : startsWith;
    assert(path.startsWith(sourceDir));
    version(Posix)
        return path[sourceDir.length + 1 .. $];
    else version(Windows)
        return path[sourceDir.length + 1 .. $].replace("\\", "/");
    else
        static assert(0, "Unsupported platform");
}
