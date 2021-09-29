include(../../qtcreatorplugin.pri)

DEFINES += LIBGIT2_FOUND
HEADERS += gitscrollbarhighlighterplugin.h
SOURCES += gitscrollbarhighlighterplugin.cpp
LIBS += -lgit2
