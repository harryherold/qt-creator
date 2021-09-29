find_path(LIBGIT2_INCLUDE_PATH git2.h)
find_library(LIBGIT2_LIBRARY_PATH git2)

if (LIBGIT2_LIBRARY_PATH AND LIBGIT2_INCLUDE_PATH)
    add_qtc_plugin(GitScrollBarHighlighter
      INCLUDES ${LIBGIT2_INCLUDE_PATH}
      PUBLIC_DEPENDS ${LIBGIT2_LIBRARY_PATH}
      PLUGIN_DEPENDS Core DiffEditor TextEditor VcsBase
      SOURCES
      gitscrollbarhighlighterplugin.cpp gitscrollbarhighlighterplugin.h constants.h)
    target_compile_definitions(Git PUBLIC LIBGIT2_FOUND)
endif()
