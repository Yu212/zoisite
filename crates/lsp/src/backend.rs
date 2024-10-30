use crate::semantic_token::{get_semantic_tokens, LEGEND_TYPE};
use dashmap::DashMap;
use ropey::Rope;
use text_size::{TextRange, TextSize};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use zoisite::hir;

pub struct Backend {
    client: Client,
    hir_map: DashMap<Url, hir::Root>,
    document_map: DashMap<Url, Rope>,
    semantic_token_map: DashMap<Url, Vec<SemanticToken>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Backend {
            client,
            hir_map: DashMap::new(),
            document_map: DashMap::new(),
            semantic_token_map: DashMap::new(),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                    legend: SemanticTokensLegend {
                        token_types: LEGEND_TYPE.into(),
                        token_modifiers: vec![],
                    },
                    range: None,
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                })),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "server initialized!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "did_open").await;
        self.on_change(params.text_document.uri, &params.text_document.text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client.log_message(MessageType::INFO, format!("did_change {}", params.content_changes.len())).await;
        self.on_change(params.text_document.uri, &params.content_changes[0].text).await;
    }

    async fn hover(&self, _: HoverParams) -> Result<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("hello!".to_string())),
            range: None,
        }))
    }

    async fn semantic_tokens_full(&self, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
        let semantic_tokens = self.semantic_token_map.get(&params.text_document.uri).unwrap().clone();
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        })))
    }
}

impl Backend {
    async fn on_change(&self, uri: Url, text: &str) {
        let rope = Rope::from_str(text);
        let (semantic_tokens, hir, diagnostics) = parse(text, &rope);
        self.hir_map.insert(uri.clone(), hir);
        self.document_map.insert(uri.clone(), rope);
        self.semantic_token_map.insert(uri.clone(), semantic_tokens);
        self.client.publish_diagnostics(uri, diagnostics, None).await
    }
}

fn parse(text: &str, rope: &Rope) -> (Vec<SemanticToken>, hir::Root, Vec<Diagnostic>) {
    let (tokens, hir, diagnostics) = zoisite::parse(text);
    let semantic_tokens = get_semantic_tokens(tokens, rope);
    let diagnostics = diagnostics.iter().map(|diag| Diagnostic {
        range: text_range_to_range(diag.range, rope),
        message: format!("{:?}", diag.kind),
        ..Default::default()
    }).collect();
    (semantic_tokens, hir, diagnostics)
}

pub fn text_range_to_range(text_range: TextRange, rope: &Rope) -> Range {
    Range::new(text_size_to_position(text_range.start(), &rope), text_size_to_position(text_range.end(), &rope))
}

pub fn text_size_to_position(text_size: TextSize, rope: &Rope) -> Position {
    let offset = text_size.into();
    let line = rope.byte_to_line(offset);
    let line_start = rope.line_to_byte(line);
    Position::new(line as u32, (offset - line_start) as u32)
}
