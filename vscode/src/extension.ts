import * as path from "path";
import {ExtensionContext, workspace} from "vscode";
import {Executable, LanguageClient, LanguageClientOptions, ServerOptions} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    const run: Executable = {
        command: context.asAbsolutePath(path.join("..", "target", "debug", "zoisite-lsp")),
        options: {
            env: {
                ...process.env,
                RUST_LOG: "DEBUG",
            }
        }
    }
    const serverOptions: ServerOptions = {
        run,
        debug: run,
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
