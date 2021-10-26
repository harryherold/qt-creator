import qbs 1.0

QtcPlugin {
    name: "GitScrollBarHighlighter"

    Depends { name: "Core" }
    Depends { name: "ProjectExplorer" }
    Depends { name: "libgit2" }
    Depends { name: "Qt"; submodules: ["widgets", "network"] }

    files: [
        "gitscrollbarhighlighterplugin.cpp",
        "gitscrollbarhighlighterplugin.h"
    ]
    condition: libgit2.found
}

