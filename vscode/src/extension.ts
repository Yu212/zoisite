import * as path from "path";
import { ExtensionContext, workspace } from "vscode";
import {LanguageClient, LanguageClientOptions, ServerOptions} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    const serverModule = context.asAbsolutePath(path.join("server", "zoisite-lsp"));

    const serverOptions: ServerOptions = {
        run: { command: serverModule },
        debug: { command: serverModule },
    };
    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: "file", language: "zoisite" },
            { scheme: "untitled", language: "zoisite" },
        ],
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
        },
    };
    client = new LanguageClient("zoisite-lsp", "Zoisite Language Server", serverOptions, clientOptions);
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    return client?.stop();
}
