/****************************************************************************
**
** Copyright (C) 2016 The Qt Company Ltd.
** Contact: https://www.qt.io/licensing/
**
** This file is part of Qt Creator.
**
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and The Qt Company. For licensing terms
** and conditions see https://www.qt.io/terms-conditions. For further
** information use the contact form at https://www.qt.io/contact-us.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 3 as published by the Free Software
** Foundation with exceptions as appearing in the file LICENSE.GPL3-EXCEPT
** included in the packaging of this file. Please review the following
** information to ensure the GNU General Public License requirements will
** be met: https://www.gnu.org/licenses/gpl-3.0.html.
**
****************************************************************************/

#include "gitscrollbarhighlighterplugin.h"
#include "constants.h"

#include <coreplugin/actionmanager/actioncontainer.h>
#include <coreplugin/actionmanager/actionmanager.h>
#include <coreplugin/coreconstants.h>
#include <coreplugin/icore.h>
#include <coreplugin/imode.h>
#include <coreplugin/modemanager.h>
#include <coreplugin/iversioncontrol.h>
#include <coreplugin/vcsmanager.h>
#include <coreplugin/editormanager/editormanager.h>
#include <coreplugin/find/highlightscrollbarcontroller.h>

#include <projectexplorer/session.h>
#include <projectexplorer/project.h>
#include <projectexplorer/projecttree.h>

#include <texteditor/texteditor.h>
#include <texteditor/textdocument.h>
#include <texteditor/textdocumentlayout.h>
#include <texteditor/texteditorconstants.h>

#include <utils/theme/theme.h>

#include <QApplication>
#include <QAction>
#include <QtConcurrent>
#include <QDebug>
#include <QObject>
#include <QScrollBar>

#include <type_traits>
#include <optional>

extern "C" {
    #include <git2.h>
}


namespace GitScrollBarHighlighter {
namespace Internal {

using namespace ProjectExplorer;
using namespace Core;

enum class ChangeClass {
    Invalid = 0,
    Added = 1,
    Deleted = 2,
};

using LineNumberClassMap =QMap<int, ChangeClass>;
using ScrollbarSegment = Highlight::ScrollbarSegment;

ChangeClass operator|( ChangeClass a, ChangeClass b)
{
    auto tmp = static_cast<std::underlying_type<ChangeClass>::type>(a)
            | static_cast<std::underlying_type<ChangeClass>::type>(b);
    return static_cast< ChangeClass>(tmp);
}

ChangeClass& operator |=( ChangeClass & a,  ChangeClass b)
{
    a = a | b;
    return a;
}

ChangeClass operator&(ChangeClass a, ChangeClass b)
{
    auto tmp = static_cast<std::underlying_type<ChangeClass>::type>(a)
    & static_cast<std::underlying_type<ChangeClass>::type>(b);
    return static_cast<ChangeClass>(tmp);
}

class GitRepo {
public:

    using GitRepositoryDeleter = std::function<void(git_repository* repo)>;
    using GitRepositoryPtr = std::unique_ptr<git_repository, GitRepositoryDeleter>;

    using GitFileDiffMap = QHash<Utils::FilePath,LineNumberClassMap>;

    using GitDiffDeleter = std::function<void(git_diff* diff)>;
    using GitDiffPtr = std::unique_ptr<git_diff, GitDiffDeleter>;

    GitRepo(const Utils::FilePath & path)
        :m_path(path), m_repository(nullptr, git_repository_free)
    {
        git_diff_options_init(&m_gitDiffOptions, GIT_DIFF_OPTIONS_VERSION);
        m_gitDiffOptions.context_lines = 0;

        git_repository* repository = nullptr;
        int error = git_repository_open(&repository, m_path.toString().toLatin1().constData());
        if (error != 0) {
            qDebug() << "Error while opening repo occured\n";
            return;
        }
        m_repository.reset(repository);
        qDebug() << "Repo successful loaded";
    }

    GitRepo(const GitRepo &) = delete;

    GitRepo& operator=(const GitRepo &) = delete;

    GitRepo& operator=(GitRepo && other)
    {
        if (&other != this) {
            this->m_path = other.m_path;
            this->m_repository = std::move(other.m_repository);
            this->m_changedFiles = std::move(other.m_changedFiles);
        }
        return *this;
    }

    GitRepo(GitRepo&& other)
        : m_path(other.m_path),
        m_repository(std::move(other.m_repository)),
        m_gitDiffOptions(other.m_gitDiffOptions),
        m_changedFiles(std::move(other.m_changedFiles))
    {}

    void removeFileFromCache(const Utils::FilePath & filePath)
    {
        m_changedFiles.remove(filePath);
    }

    LineNumberClassMap getChangedLinesOfFile(const Utils::FilePath & filePath) const
    {
        return m_changedFiles.value(filePath);
    }

    bool hasFile(const Utils::FilePath & filePath) const
    {
        return m_changedFiles.contains(filePath);
    }

    void updateChangedLinesOfFile(const Utils::FilePath & filePath)
    {
        char * paths[1];
        auto pathString = filePath.relativeChildPath(m_path).toString().toStdString();
        paths[0] = pathString.data();
        m_gitDiffOptions.pathspec.count = 1;
        m_gitDiffOptions.pathspec.strings = paths;

        git_diff* tmpDiff = NULL;
        int error = git_diff_index_to_workdir(&tmpDiff, m_repository.get(), NULL, &m_gitDiffOptions);
        if (error != 0) {
            qDebug() << "Git diff failed\n";
            return;
        }
        auto diff = GitDiffPtr{tmpDiff, git_diff_free};
        m_changedFiles.insert(filePath, extractLineNumbersOfDiff(diff.get()));
    }

private:
   LineNumberClassMap extractLineNumbersOfDiff(git_diff* diff) const
    {
       LineNumberClassMap lineNumbers;

        int error = git_diff_foreach(
            diff,
            nullptr,
            nullptr,
            [](const git_diff_delta* delta, const git_diff_hunk* hunk, void* payload) -> int {
               LineNumberClassMap * p = (LineNumberClassMap *) payload;
                auto addLines = [&] (int start, int lines, auto changeClass) -> void {
                    for (int iter = start, end = start + lines; iter < end; ++iter) {
                        if (p->contains(iter)) {
                            (*p)[iter] |= changeClass;
                        }
                        else {
                            p->insert(iter, changeClass);
                        }
                    }
                };
                if (delta != nullptr && p != nullptr) {
                    addLines(hunk->new_start, hunk->new_lines, ChangeClass::Added);
                    addLines(hunk->old_start, hunk->old_lines, ChangeClass::Deleted);
                }
                return 0;
            },
            nullptr,
            (void *) &lineNumbers);
        if (error != 0) {
            qDebug() << "Some error occured while traversing over diff.";
        }
        return lineNumbers;
    }

private:
    Utils::FilePath m_path;
    GitRepositoryPtr m_repository;
    git_diff_options m_gitDiffOptions;
    GitFileDiffMap m_changedFiles;
};

class GitScrollBarHighlighterPrivate : public QObject
{
    using GitFileDiffWatcher = QFutureWatcher<void>;
    using GitRepoMap = std::unordered_map<Utils::FilePath, GitRepo>;

public:
    GitScrollBarHighlighterPrivate();
    ~GitScrollBarHighlighterPrivate();

private:
    void updateStatusOfCurrentFile();
    GitRepoMap::iterator findGitRepo(const Utils::FilePath & filePath);
    GitRepoMap::const_iterator cfindGitRepo(const Utils::FilePath & filePath) const;
    void setupDocumentSignals(TextEditor::TextDocument *textDocument);
    void scheduleHighlightUpdate();

private slots:
    void updateHighlightOfCurrentDocument();
    void setCurrentDocument(Core::IEditor *editor);
    void handleClosedDocument(Core::IDocument *document);

private:
    static GitScrollBarHighlighterPrivate * m_instance;
    Core::IDocument* m_currentDocument = nullptr;
    GitRepoMap m_repoMap;
    GitFileDiffWatcher m_updateWatcher;
    bool m_updateScheduled;
};

GitScrollBarHighlighterPrivate::GitScrollBarHighlighterPrivate()
{
    QObject::connect(SessionManager::instance(), &SessionManager::projectAdded,
            this, [&] (ProjectExplorer::Project *project) {
                auto projectPath = project->rootProjectDirectory();
                auto * vcs = Core::VcsManager::findVersionControlForDirectory(projectPath);
                if (m_updateWatcher.isRunning()) {
                    m_updateWatcher.waitForFinished();
                }
                if (m_repoMap.count(projectPath) == 0 && vcs != nullptr) {
                    qDebug() << "Register project" << projectPath;
                    m_repoMap.insert({projectPath, GitRepo(projectPath)});
                }
            });
    QObject::connect(SessionManager::instance(), &SessionManager::projectRemoved,
            this, [&] (ProjectExplorer::Project *project){
                if (m_updateWatcher.isRunning()) {
                    m_updateWatcher.waitForFinished();
                }
                auto iter = m_repoMap.find(project->rootProjectDirectory());
                if (iter != m_repoMap.end()) {
                    qDebug() << "Deregister project" << project->rootProjectDirectory();
                    iter = m_repoMap.erase(iter);
                }
            });
    QObject::connect(Core::EditorManager::instance(), &Core::EditorManager::currentEditorChanged,
            this, &GitScrollBarHighlighterPrivate::setCurrentDocument);
    QObject::connect(Core::EditorManager::instance(), &Core::EditorManager::documentClosed,
            this, &GitScrollBarHighlighterPrivate::handleClosedDocument);
    QObject::connect(Core::EditorManager::instance(), &Core::EditorManager::saved,
            this, &GitScrollBarHighlighterPrivate::updateStatusOfCurrentFile);
    QObject::connect(qApp, &QApplication::applicationStateChanged, this, [=] (Qt::ApplicationState state) {
        if (state == Qt::ApplicationState::ApplicationActive) {
            updateStatusOfCurrentFile();
        }
    });
    QObject::connect(&m_updateWatcher, &GitFileDiffWatcher::finished,
            this, &GitScrollBarHighlighterPrivate::updateHighlightOfCurrentDocument);
}

void GitScrollBarHighlighterPrivate::updateStatusOfCurrentFile()
{
    if (m_updateWatcher.isRunning() || m_currentDocument == nullptr) {
        return;
    }
    auto result = findGitRepo(m_currentDocument->filePath());
    if (result != m_repoMap.end()) {
        auto future = QtConcurrent::run([] (GitRepo * repo, const Utils::FilePath& filePath) {
            return repo->updateChangedLinesOfFile(filePath);
        }, &result->second, m_currentDocument->filePath());
        m_updateWatcher.setFuture(future);
    }
}

void GitScrollBarHighlighterPrivate::setCurrentDocument(Core::IEditor *editor)
{
    if (editor != nullptr && editor->document() != nullptr) {
        auto repoIterator = findGitRepo(editor->document()->filePath());
        if (repoIterator == m_repoMap.end()) {
            return;
        }
        if (m_updateWatcher.isRunning()) {
            m_updateWatcher.cancel();
            m_updateWatcher.waitForFinished();
        }
        m_currentDocument = editor->document();
        QObject::connect(m_currentDocument, &QObject::destroyed, this, [=] () {
            m_currentDocument = nullptr;
        });
        if (! repoIterator->second.hasFile(m_currentDocument->filePath())) {
            // NOTE in this case the file is opened the first time
            setupDocumentSignals(qobject_cast<TextEditor::TextDocument *>(editor->document()));
            updateStatusOfCurrentFile();
        }
        else {
            updateHighlightOfCurrentDocument();
        }
    }
}

void GitScrollBarHighlighterPrivate::handleClosedDocument(Core::IDocument *document)
{
    if (document == nullptr) {
        return;
    }
    auto repoIterator = findGitRepo(document->filePath());
    if (repoIterator != m_repoMap.end()) {
        if (m_updateWatcher.isRunning()) {
            m_updateWatcher.waitForFinished();
        }
        repoIterator->second.removeFileFromCache(document->filePath());
    }
}

GitScrollBarHighlighterPrivate::GitRepoMap::iterator GitScrollBarHighlighterPrivate::findGitRepo(const Utils::FilePath & filePath)
{
    auto result = std::find_if(m_repoMap.begin(), m_repoMap.end(), [=] (const GitRepoMap::value_type & item) -> bool {
        return filePath.isChildOf(item.first);
    });
    return result;
}

GitScrollBarHighlighterPrivate::GitRepoMap::const_iterator GitScrollBarHighlighterPrivate::cfindGitRepo(const Utils::FilePath & filePath) const
{
    auto result = std::find_if(m_repoMap.cbegin(), m_repoMap.cend(), [=] (const GitRepoMap::value_type & item) -> bool {
        return filePath.isChildOf(item.first);
    });
    return result;
}

GitScrollBarHighlighterPrivate::~GitScrollBarHighlighterPrivate()
{
}

void GitScrollBarHighlighterPrivate::setupDocumentSignals(TextEditor::TextDocument *textDocument)
{
    if (textDocument == nullptr) {
        return;
    }
    QTextDocument *doc = textDocument->document();
    if (auto * documentLayout = qobject_cast<TextEditor::TextDocumentLayout*>(doc->documentLayout())) {
        QObject::connect(documentLayout, &TextEditor::TextDocumentLayout::updateExtraArea,
                         this, &GitScrollBarHighlighterPrivate::scheduleHighlightUpdate);

        QObject::connect(documentLayout, &QAbstractTextDocumentLayout::documentSizeChanged,
                         this, &GitScrollBarHighlighterPrivate::scheduleHighlightUpdate);

        QObject::connect(documentLayout, &QAbstractTextDocumentLayout::update,
                         this, &GitScrollBarHighlighterPrivate::scheduleHighlightUpdate);
    }
}

void GitScrollBarHighlighterPrivate::scheduleHighlightUpdate()
{
    if (m_updateScheduled)
        return;

    m_updateScheduled = true;
    QMetaObject::invokeMethod(this, &GitScrollBarHighlighterPrivate::updateHighlightOfCurrentDocument,
                              Qt::QueuedConnection);
}

void GitScrollBarHighlighterPrivate::updateHighlightOfCurrentDocument()
{
    m_updateScheduled = false;
    auto result = cfindGitRepo(m_currentDocument->filePath());
    if (result != m_repoMap.cend()) {
        auto * editorManager = Core::EditorManager::instance();
        if (editorManager != nullptr && editorManager->currentEditor() != nullptr) {
            auto * currentEditor = editorManager->currentEditor();
            if (auto * textEditorWidget = qobject_cast<TextEditor::TextEditorWidget *>(currentEditor->widget())) {
                auto * highlighterController = textEditorWidget->highlightScrollBarController();
                if (highlighterController != nullptr) {
                    highlighterController->removeHighlights(TextEditor::Constants::SCROLL_BAR_CHANGED_LINES);

                    auto changedLines = result->second.getChangedLinesOfFile(m_currentDocument->filePath());
                    auto * textDocument = qobject_cast<TextEditor::TextDocument *>(m_currentDocument);
                    auto lineCount = textDocument->document()->lineCount();

                    for (auto iter = changedLines.cbegin(); iter != changedLines.cend(); ++iter) {
                        auto wasAdded = (iter.value() & ChangeClass::Added) == ChangeClass::Added;
                        auto wasDeleted = (iter.value() & ChangeClass::Deleted) == ChangeClass::Deleted;

                        if (iter.key() > lineCount) {
                            continue;
                        }
                        if (wasAdded && wasDeleted) {
                            highlighterController->addHighlight(
                                {TextEditor::Constants::SCROLL_BAR_CHANGED_LINES, iter.key() - 1,
                                 Utils::Theme::PaletteHighlight, Highlight::HighPriority, ScrollbarSegment::Left});
                        }
                        else if(wasAdded) {
                            highlighterController->addHighlight(
                                {TextEditor::Constants::SCROLL_BAR_CHANGED_LINES, iter.key() - 1,
                                 Utils::Theme::ProgressBarColorFinished, Highlight::HighPriority, ScrollbarSegment::Left});
                        }
                        else {
                            highlighterController->addHighlight(
                                {TextEditor::Constants::SCROLL_BAR_CHANGED_LINES, iter.key() - 1,
                                 Utils::Theme::ProgressBarColorError, Highlight::HighPriority, ScrollbarSegment::Left});
                        }
                    }
                }
            }
        }
    }
}


GitScrollBarHighlighterPlugin::GitScrollBarHighlighterPlugin()
{
}

GitScrollBarHighlighterPlugin::~GitScrollBarHighlighterPlugin()
{
    git_libgit2_shutdown();
}

bool GitScrollBarHighlighterPlugin::initialize(const QStringList &arguments, QString *errorMessage)
{
    Q_UNUSED(arguments)
    Q_UNUSED(errorMessage)
    git_libgit2_init();
    d = std::make_unique<GitScrollBarHighlighterPrivate>();
    return true;
}

void GitScrollBarHighlighterPlugin::extensionsInitialized()
{
}

} // namespace Internal
} // namespace GitScrollBarHighlighter
