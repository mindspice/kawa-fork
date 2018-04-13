/*
Copyright (c) 2016 George Fraser [ from vscode-javac]
Copyright (c) 2018 Per Bothner

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

package kawa.langserver;

//import gnu.expr.ModuleContext;
import gnu.expr.AccessExp;
import gnu.expr.Compilation;
import gnu.expr.Declaration;
import gnu.expr.ExpExpVisitor;
import gnu.expr.Expression;
import gnu.expr.Language;
import gnu.expr.ModuleExp;
import gnu.expr.ModuleInfo;
import gnu.expr.ModuleManager;
import gnu.expr.ReferenceExp;
import gnu.expr.SetExp;
import gnu.kawa.io.CharArrayInPort;
import gnu.kawa.io.Path;
import gnu.text.SourceError;
import gnu.text.SourceLocator;
import gnu.text.SourceMessages;

import org.eclipse.lsp4j.CodeActionParams;
import org.eclipse.lsp4j.CodeLens;
import org.eclipse.lsp4j.CodeLensOptions;
import org.eclipse.lsp4j.CodeLensParams;
import org.eclipse.lsp4j.Command;
import org.eclipse.lsp4j.CompletionItem;
import org.eclipse.lsp4j.CompletionList;
import org.eclipse.lsp4j.CompletionOptions;
import org.eclipse.lsp4j.DidChangeConfigurationParams;
import org.eclipse.lsp4j.DidChangeTextDocumentParams;
import org.eclipse.lsp4j.DidChangeWatchedFilesParams;
import org.eclipse.lsp4j.DidCloseTextDocumentParams;
import org.eclipse.lsp4j.DidOpenTextDocumentParams;
import org.eclipse.lsp4j.DidSaveTextDocumentParams;
import org.eclipse.lsp4j.DocumentFormattingParams;
import org.eclipse.lsp4j.DocumentHighlight;
import org.eclipse.lsp4j.DocumentOnTypeFormattingParams;
import org.eclipse.lsp4j.DocumentRangeFormattingParams;
import org.eclipse.lsp4j.DocumentSymbolParams;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.DiagnosticSeverity;
import org.eclipse.lsp4j.ExecuteCommandOptions;
import org.eclipse.lsp4j.ExecuteCommandParams;
import org.eclipse.lsp4j.Hover;
import org.eclipse.lsp4j.InitializeParams;
import org.eclipse.lsp4j.InitializeResult;
import org.eclipse.lsp4j.InitializedParams;
import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.PublishDiagnosticsParams;
import org.eclipse.lsp4j.Range;
import org.eclipse.lsp4j.ReferenceParams;
import org.eclipse.lsp4j.Registration;
import org.eclipse.lsp4j.RegistrationParams;
import org.eclipse.lsp4j.RenameParams;
import org.eclipse.lsp4j.ServerCapabilities;
import org.eclipse.lsp4j.SignatureHelp;
import org.eclipse.lsp4j.SymbolInformation;
import org.eclipse.lsp4j.TextDocumentContentChangeEvent;
import org.eclipse.lsp4j.TextDocumentIdentifier;
import org.eclipse.lsp4j.TextDocumentItem;
import org.eclipse.lsp4j.TextDocumentPositionParams;
import org.eclipse.lsp4j.TextDocumentSyncKind;
import org.eclipse.lsp4j.TextEdit;
import org.eclipse.lsp4j.Unregistration;
import org.eclipse.lsp4j.UnregistrationParams;
import org.eclipse.lsp4j.VersionedTextDocumentIdentifier;
import org.eclipse.lsp4j.WorkspaceEdit;
import org.eclipse.lsp4j.WorkspaceSymbolParams;
import org.eclipse.lsp4j.jsonrpc.CancelChecker;
import org.eclipse.lsp4j.jsonrpc.Launcher;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.eclipse.lsp4j.launch.LSPLauncher;
import org.eclipse.lsp4j.services.LanguageClient;
import org.eclipse.lsp4j.services.LanguageClientAware;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.lsp4j.services.TextDocumentService;
import org.eclipse.lsp4j.services.WorkspaceService;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static org.eclipse.lsp4j.jsonrpc.CompletableFutures.computeAsync;

public class KawaLanguageServer implements LanguageServer, LanguageClientAware,
                                           TextDocumentService, WorkspaceService {
    /**
     * Exit code returned when JDTLanguageServer is forced to exit.
     */
    private static final int FORCED_EXIT_CODE = 1;
    private LanguageClient client;
    private final Map<URI, VersionedContent> activeDocuments = new HashMap<>();
    private Set<String> registeredCapabilities = new HashSet<>(3);

    ModuleManager mmanager = ModuleManager.getInstance();

    public KawaLanguageServer()
    {
    }

    @Override
    public CompletableFuture<InitializeResult> initialize(InitializeParams params) {
        logInfo(">> initialize");
        ServerCapabilities c = new ServerCapabilities();
        c.setTextDocumentSync(TextDocumentSyncKind.Incremental);
        c.setDefinitionProvider(true);
        c.setHoverProvider(true);
        c.setWorkspaceSymbolProvider(true);
        c.setReferencesProvider(true);
        c.setDocumentSymbolProvider(true);
        /*
        c.setCompletionProvider(new CompletionOptions(true, List.of(".")));
        c.setCodeActionProvider(true);
        c.setExecuteCommandProvider(
            new ExecuteCommandOptions(List.of("Java.importClass")));
        c.setSignatureHelpProvider(new SignatureHelpOptions(List.of("(", ",")));
        */
        InitializeResult initializeResult = new InitializeResult(c);
        return CompletableFuture.completedFuture(initializeResult);
    }

    @Override
    public void initialized(InitializedParams params) {
    }

    @Override
    public CompletableFuture<Object> shutdown() {
        logInfo(">> shutdown");
        return computeAsync((cc) -> {
                // FIXME
                return new Object();
            });
    }

    @Override
    public void exit() {
        logInfo(">> exit");
        Executors.newSingleThreadScheduledExecutor().schedule(() -> {
                logInfo("Forcing exit after 1 min.");
                System.exit(FORCED_EXIT_CODE);
            }, 1, TimeUnit.MINUTES);
    }

    @Override
    public TextDocumentService getTextDocumentService() {
        return this;
    }

    @Override
    public WorkspaceService getWorkspaceService() {
        return this;
    }

    /** Text of file, if it is in the active set */
    Optional<String> activeContent(URI file) {
        return Optional.ofNullable(activeDocuments.get(file)).map(doc -> doc.content);
    }

    void doLint(Collection<URI> paths) {
        Set<String> spaths = new HashSet<String>();
        SourceMessages messages = new SourceMessages();
        logInfo("Lint " + paths);
        for (URI path : paths) {
            String contents = activeDocuments.get(path).content;
            CharArrayInPort inport = CharArrayInPort.make(contents);
            inport.setPath(Path.valueOf(path));
            String spath = path.toString();
            ModuleInfo minfo = mmanager.findWithSourcePath(spath);
            Language language = Language.getInstanceFromFilenameExtension(spath);
            spaths.add(spath);
            if (language == null) {
                // language = Language.detect(inport)l
                // language = Language.getDefaultLanguage();
                language = Language.getInstance(null);
            }
            minfo.clearClass();
            int options = Language.PARSE_PROLOG|Language.PARSE_EXPLICIT|Language.PARSE_FOR_LINT;
            try {
                Compilation comp = language.parse(inport, messages, options, minfo);
                comp.process(Compilation.WALKED);
            } catch (IOException ex) {
                logWarning("caught "+ex); // FIXME
            }
        }
        publishDiagnostics(spaths, messages);
    }

    private void publishDiagnostics(
            Set<String> touched,
            SourceMessages diagnostics) {
        Map<String, List<Diagnostic>> allDiagnostics =
	    new HashMap<String, List<Diagnostic>>();
        for (String spath : touched) {
            allDiagnostics.put(spath, new ArrayList<Diagnostic>());
        }

	for (SourceError err = diagnostics.getErrors();
	     err != null;  err = err.next) {
	    String fname = err.getFileName();
            String spath = fname; //gnu.kawa.io.Path.toURL(fname).toString();
	    List<Diagnostic> list = allDiagnostics.get(spath);
            logInfo("publishD fn:"+fname+" sp:"+spath+" d:"+ err+" end:"+err.getEndLine()+":"+err.getEndColumn());
	    if (list == null)
                continue;
            Diagnostic d = asDiagnostic(err);
	    list.add(d);
	}
	for (Map.Entry<String, List<Diagnostic>> entry
		 : allDiagnostics.entrySet()) {
	    client.publishDiagnostics(new PublishDiagnosticsParams(entry.getKey(),
						    entry.getValue()));
	}
    }

    @Override
    public CompletableFuture<List<? extends SymbolInformation>> symbol(WorkspaceSymbolParams params) {
        logInfo(">> workspace/symbol");
        throw new UnsupportedOperationException();
    }

    @Override
    public void didChangeConfiguration(DidChangeConfigurationParams params) {
        logInfo(">> workspace/didChangeConfiguration");
        Object settings = params.getSettings();
    }

    @Override
    public void didChangeWatchedFiles(DidChangeWatchedFilesParams params) {
        logInfo(">> workspace/didChangeWatchedFiles");
        throw new UnsupportedOperationException();
    }

    @Override
    public CompletableFuture<Object> executeCommand(ExecuteCommandParams params) {
        logInfo(">> workspace/executeCommand");
        throw new UnsupportedOperationException();
    }

    @Override
    public CompletableFuture<Either<List<CompletionItem>, CompletionList>> completion(TextDocumentPositionParams position) {
        logInfo(">> document/completion");
        throw new UnsupportedOperationException();
    }

    @Override
    public CompletableFuture<CompletionItem> resolveCompletionItem(CompletionItem unresolved) {
        logInfo(">> document/resolveCompletionItem");
        throw new UnsupportedOperationException();
    }

    @Override
    public CompletableFuture<Hover> hover(TextDocumentPositionParams position) {
        String uri = position.getTextDocument().getUri();
        int line = position.getPosition().getLine() + 1;
        int character = position.getPosition().getCharacter() + 1;
        ModuleInfo minfo = mmanager.findWithSourcePath(uri);

        logInfo(String.format("hover at %s %d:%d", uri, line, character));
        Hover hover = emptyHover();
        return CompletableFuture.completedFuture(hover);
    }

    private Hover emptyHover() {
        return new Hover(Collections.emptyList(), null);
    }

    @Override
    public CompletableFuture<SignatureHelp> signatureHelp(TextDocumentPositionParams position) {
        logInfo(">> document/signatureHelp");
        throw new UnsupportedOperationException();
    }

    @Override
    public CompletableFuture<List<? extends Location>> definition(TextDocumentPositionParams position) {
        String uri = position.getTextDocument().getUri();
        final int line = position.getPosition().getLine() + 1;
        final int character = position.getPosition().getCharacter() + 1;
        ArrayList<Location> locations = new ArrayList<Location>();
        ModuleInfo minfo = mmanager.findWithSourcePath(uri);

        logInfo(">> document/definition "+uri+":"+line+":"+character);
        if (minfo != null && minfo.getModuleExpRaw() != null) {
            ModuleExp mexp = minfo.getModuleExpRaw();
            logInfo(">> - mexp:"+mexp);
            class FindDecl extends ExpExpVisitor<Void> {
                @Override
                protected Expression visitReferenceExp(ReferenceExp exp, Void ignored) {
                    visitAccessExp(exp);
                    return exp;
                }
                @Override
                protected Expression visitSetExp(SetExp exp, Void ignored) {
                    //FIXME "source-location" of SetExp is enture SetExp
                    // - we need location of just the LHS
                    //visitAccessExp(exp);
                    return super.visitSetExp(exp, ignored);
                }
                protected void visitAccessExp(AccessExp exp) {
                    int startLine = exp.getStartLine();
                    int startColumn = exp.getStartColumn();
                    int endLine = exp.getEndLine();
                    int endColumn = exp.getEndColumn();
                    String name = exp.getName();
                    if (name != null
                        && line >= startLine
                        && line <= (endLine > 0 ? endLine : startLine)
                        && character >= startColumn
                        && (endColumn > 0 ? character < endColumn
                            : character < startColumn + name.length())) {
                        Declaration decl = exp.getBinding();
                        System.err.println("check "+exp+":"+startLine+":"+startColumn+"-"+endLine+":"+endColumn+" d:"+decl);
                        this.exitValue = decl;
                    }
                }
            }
            FindDecl fd  = new FindDecl();
            fd.visit(mexp, null);
            Declaration decl = (Declaration) fd.getExitValue();
            if (decl != null)
                locations.add(asLocation(decl));
        }
        return CompletableFuture.completedFuture(locations);
    }

    @Override
    public CompletableFuture<List<? extends Location>> references(ReferenceParams params) {
        logInfo(">> document/references");
        throw new UnsupportedOperationException();
    }

    @Override
    public CompletableFuture<List<? extends DocumentHighlight>> documentHighlight(TextDocumentPositionParams position) {
        logInfo(">> document/documentHighlight");
        throw new UnsupportedOperationException();
    }

    @Override
    public CompletableFuture<List<? extends SymbolInformation>> documentSymbol(DocumentSymbolParams params) {
        logInfo(">> document/documentSymbol");
        // FIXME
        List<SymbolInformation> symbols = new ArrayList<SymbolInformation>();
        return CompletableFuture.completedFuture(symbols);
    }

    @Override
    public CompletableFuture<List<? extends Command>> codeAction(CodeActionParams params) {
        logInfo(">> document/codeAction");
        throw new UnsupportedOperationException();
    }

    @Override
    public CompletableFuture<List<? extends CodeLens>> codeLens(CodeLensParams params) {
        logInfo(">> document/codeLens");
        throw new UnsupportedOperationException();
    }

    @Override
    public CompletableFuture<CodeLens> resolveCodeLens(CodeLens unresolved) {
        logInfo(">> codeLens/resolve");
        throw new UnsupportedOperationException();
    }

    @Override
    public CompletableFuture<List<? extends TextEdit>> formatting(DocumentFormattingParams params) {
        logInfo(">> document/formatting");
        throw new UnsupportedOperationException();
    }

    @Override
    public CompletableFuture<List<? extends TextEdit>> rangeFormatting(DocumentRangeFormattingParams params) {
        logInfo(">> document/rangeFormatting");
        throw new UnsupportedOperationException();
    }

    @Override
    public CompletableFuture<List<? extends TextEdit>> onTypeFormatting(DocumentOnTypeFormattingParams params) {
        logInfo(">> document/onTypeFormatting");
        throw new UnsupportedOperationException();
    }

    @Override
    public CompletableFuture<WorkspaceEdit> rename(RenameParams params) {
        logInfo(">> document/rename");
        throw new UnsupportedOperationException();
    }

    @Override
    public void didOpen(DidOpenTextDocumentParams params) {
        TextDocumentItem document = params.getTextDocument();
        String u = document.getUri();
        logInfo(">> document/didOpen "+u);
        URI uri = URI.create(u);

        activeDocuments.put(uri, new VersionedContent(document.getText(), document.getVersion()));

        doLint(Collections.singleton(uri));
    }

    @Override
    public void didChange(DidChangeTextDocumentParams params) {
        VersionedTextDocumentIdentifier document = params.getTextDocument();
        URI uri = URI.create(document.getUri());
        VersionedContent existing = activeDocuments.get(uri);
        String newText = existing.content;

        if (document.getVersion() > existing.version) {
            for (TextDocumentContentChangeEvent change : params.getContentChanges()) {
                if (change.getRange() == null)
                    activeDocuments.put(
                                        uri, new VersionedContent(change.getText(), document.getVersion()));
                else newText = patch(newText, change);
            }

            activeDocuments.put(uri, new VersionedContent(newText, document.getVersion()));
            doLint(Collections.singleton(uri));
        } else
            logWarning(
                        "Ignored change with version "
                        + document.getVersion()
                        + " <= "
                        + existing.version);
        logInfo(">> document/didChange "+params.getTextDocument().getUri());
    }

   private String patch(String sourceText, TextDocumentContentChangeEvent change) {
        try {
            Range range = change.getRange();
            BufferedReader reader = new BufferedReader(new StringReader(sourceText));
            StringWriter writer = new StringWriter();

            // Skip unchanged lines
            int line = 0;

            while (line < range.getStart().getLine()) {
                writer.write(reader.readLine() + '\n');
                line++;
            }

            // Skip unchanged chars
            for (int character = 0; character < range.getStart().getCharacter(); character++)
                writer.write(reader.read());

            // Write replacement text
            writer.write(change.getText());

            // Skip replaced text
            reader.skip(change.getRangeLength());

            // Write remaining text
            while (true) {
                int next = reader.read();

                if (next == -1) return writer.toString();
                else writer.write(next);
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void didClose(DidCloseTextDocumentParams params) {
        logInfo(">> document/didClose");
    }

    @Override
    public void didSave(DidSaveTextDocumentParams params) {
        logInfo(">> document/didSave");
    }

    void logInfo(String message) {
        //LOG.info(message);
        System.err.println(message);
    }

    void logWarning(String message) {
        // LOG.warning(message);
        System.err.println("warning: "+message);
    }

    public static void main(String[] args) {
        LanguageServer server = new KawaLanguageServer();
        Launcher<LanguageClient> launcher = 
            LSPLauncher.createServerLauncher(server,
                                     System.in, 
                                     System.out);
        if (server instanceof LanguageClientAware) {
            LanguageClient client = launcher.getRemoteProxy();
            ((LanguageClientAware) server).connect(client);
        }
        launcher.startListening();
    }

    public static Position UNDEFINED_POSITION = new Position(-1, -1);
    public static Range UNDEFINED_RANGE =
	new Range(UNDEFINED_POSITION, UNDEFINED_POSITION);

    public static Range asRange(SourceLocator sloc) {
	int startLine = sloc.getStartLine();
	if (startLine <= 0)
	   return UNDEFINED_RANGE;
	else {
	    int endLine = sloc.getEndLine();
	    int startColumn = sloc.getStartColumn();
	    int endColumn = sloc.getEndColumn();
	    // adjust - LSP Positions are 0-based
	    startLine = startLine-1;
	    endLine = endLine <= 0 ? startLine : endLine-1;
	    // FIXME is incorrect for non-BMP-character
	    startColumn = startColumn <= 0 ? 0 : startColumn-1;
	    endColumn = endColumn <= 0 ? startColumn : endColumn-1;
	    return new Range(new Position(startLine, startColumn),
                             new Position(endLine, endColumn));
	}
    }
    public static Location asLocation(SourceLocator sloc) {
        String uri = sloc.getFileName();
        return new Location(uri, asRange(sloc));
    }

    public static Diagnostic asDiagnostic(SourceError error)
    {
        Range range = asRange(error);
        DiagnosticSeverity severity;
	switch (error.severity) {
	case 'i':
	    severity = DiagnosticSeverity.Information;
	    break;
	case 'w':
	    severity = DiagnosticSeverity.Warning;
	    break;
	default: // case 'e': case 'f':
	    severity = DiagnosticSeverity.Error;
	    break;
	}
	return new Diagnostic(range, error.message, severity, "kawa");
    }

    public static class VersionedContent {
        final String content;
        final int version;

        VersionedContent(String content, int version) {
            this.content = content;
            this.version = version;
        }
    }

    @Override
    public void connect(LanguageClient client) {
        this.client = client;
    }
}
