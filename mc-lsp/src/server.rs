use std::collections::HashMap;

use lsp_server::{Message, Notification};
use lsp_types::{
    Diagnostic, PublishDiagnosticsParams, Uri,
    notification::{Notification as _, PublishDiagnostics},
};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    HoverProviderCapability, OneOf, Position, Range, ServerCapabilities, TextDocumentItem,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
};
use serde::Serialize;

#[derive(Default)]
pub struct ServerState {
    open_documents: HashMap<Uri, DocumentEntry>,
    server_driven_messages: Vec<Message>,
}

struct DocumentEntry {
    contents: String, // source: SourceFile,
                      // parsed: ParsedFile,
}

impl ServerState {
    pub fn new() -> ServerState {
        ServerState {
            open_documents: HashMap::new(),
            server_driven_messages: Vec::new(),
        }
    }

    pub fn capabilities() -> ServerCapabilities {
        ServerCapabilities {
            definition_provider: Some(OneOf::Left(true)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            text_document_sync: Some(TextDocumentSyncCapability::Options(
                TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::FULL),
                    will_save: None,
                    will_save_wait_until: None,
                    save: None,
                },
            )),
            ..Default::default()
        }
    }

    pub fn compile_for_errors(&mut self, uri: &Uri) -> anyhow::Result<()> {
        let Some(document) = self.open_documents.get(uri) else {
            return Ok(());
        };
        let tokens = mc::tokenizer::tokenize(&document.contents);
        let errors = if !tokens.errors.is_empty() {
            Some(tokens.errors)
        } else {
            let parsed_unit = mc::parser::parse(&tokens);
            if !parsed_unit.errors.is_empty() {
                Some(parsed_unit.errors)
            } else {
                let compiled_unit = mc::compiler::compile_to_assembly(&parsed_unit);
                if !compiled_unit.errors.is_empty() {
                    Some(compiled_unit.errors)
                } else {
                    None
                }
            }
        };

        let diagnostics: Vec<Diagnostic> = errors.map_or(Vec::new(), |errors| {
            errors
                .iter()
                .map(|error| Diagnostic {
                    range: Range {
                        start: Position {
                            line: error.pos.line as u32,
                            character: error.pos.column as u32,
                        },
                        end: Position {
                            line: error.pos.line as u32,
                            character: error.pos.column as u32 + 1,
                        },
                    },
                    message: error.msg.clone(),
                    ..Default::default()
                })
                .collect()
        });

        self.push_notification(
            PublishDiagnostics::METHOD,
            PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics,
                version: None,
            },
        )?;

        Ok(())
    }

    pub fn did_open_text_document(
        &mut self,
        DidOpenTextDocumentParams { text_document }: DidOpenTextDocumentParams,
    ) -> anyhow::Result<()> {
        let TextDocumentItem {
            uri,
            language_id,
            version: _,
            text: contents,
        } = text_document;
        eprintln!("{language_id}");
        self.open_documents
            .insert(uri.clone(), DocumentEntry { contents });
        self.compile_for_errors(&uri)?;
        Ok(())
    }

    pub fn did_change_text_document(
        &mut self,
        DidChangeTextDocumentParams {
            text_document,
            content_changes,
        }: DidChangeTextDocumentParams,
    ) -> anyhow::Result<()> {
        if let Some(document) = self.open_documents.get_mut(&text_document.uri) {
            for change in content_changes {
                if let Some(range) = change.range {
                    let start = position_to_idx(document.contents.as_str(), range.start) as usize;
                    document
                        .contents
                        .replace_range(start..(start + change.text.len()), change.text.as_str());
                } else {
                    document.contents = change.text;
                }
            }
        }

        self.compile_for_errors(&text_document.uri)?;
        Ok(())
    }

    pub fn did_close_text_document(
        &mut self,
        DidCloseTextDocumentParams { text_document }: DidCloseTextDocumentParams,
    ) {
        self.open_documents.remove(&text_document.uri);
    }

    pub fn server_messages(&mut self) -> impl Iterator<Item = Message> + '_ {
        self.server_driven_messages.drain(..)
    }

    fn push_notification(&mut self, method: &str, params: impl Serialize) -> anyhow::Result<()> {
        self.server_driven_messages
            .push(Message::Notification(Notification {
                method: method.to_string(),
                params: serde_json::to_value(params)?,
            }));

        Ok(())
    }
}

fn position_to_idx(source: &str, position: Position) -> u32 {
    let mut lines = 0;
    let mut idx = 0;
    for char in source.chars() {
        idx += 1;
        if char == '\n' {
            lines += 1;
            if lines >= position.line {
                break;
            }
        }
    }

    // TODO: https://github.com/rust-lang/rust-analyzer/issues/202
    idx + position.character
}
