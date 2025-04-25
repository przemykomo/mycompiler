use std::{cmp::Ordering, collections::HashMap};

use lsp_server::{Message, Notification};
use lsp_types::{
    Diagnostic, PublishDiagnosticsParams, SemanticToken, SemanticTokenModifier, SemanticTokenType,
    SemanticTokens, SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
    SemanticTokensResult, SemanticTokensServerCapabilities, Uri, WorkDoneProgressOptions,
    notification::{Notification as _, PublishDiagnostics},
};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    HoverProviderCapability, OneOf, Position, Range, ServerCapabilities, TextDocumentItem,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
};
use mc::{
    compiler::{CompiledUnit, Highlight, HighlightKind},
    tokenizer::{Token, TokenSpanned},
};
use serde::Serialize;

const TOKEN_TYPES: [SemanticTokenType; 23] = [
    SemanticTokenType::NAMESPACE,      //0
    SemanticTokenType::TYPE,           //1
    SemanticTokenType::CLASS,          //2
    SemanticTokenType::ENUM,           //3
    SemanticTokenType::INTERFACE,      //4
    SemanticTokenType::STRUCT,         //5
    SemanticTokenType::TYPE_PARAMETER, //6
    SemanticTokenType::PARAMETER,      //7
    SemanticTokenType::VARIABLE,       //8
    SemanticTokenType::PROPERTY,       //9
    SemanticTokenType::ENUM_MEMBER,    //10
    SemanticTokenType::EVENT,          //11
    SemanticTokenType::FUNCTION,       //12
    SemanticTokenType::METHOD,         //13
    SemanticTokenType::MACRO,          //14
    SemanticTokenType::KEYWORD,        //15
    SemanticTokenType::MODIFIER,       //16
    SemanticTokenType::COMMENT,        //17
    SemanticTokenType::STRING,         //18
    SemanticTokenType::NUMBER,         //19
    SemanticTokenType::REGEXP,         //20
    SemanticTokenType::OPERATOR,       //21
    SemanticTokenType::DECORATOR,      //22
];

#[derive(Default)]
pub struct ServerState {
    open_documents: HashMap<Uri, DocumentEntry>,
    server_driven_messages: Vec<Message>,
}

struct DocumentEntry {
    contents: String,
    tokens: Vec<TokenSpanned>,
    compiled_unit: Option<CompiledUnit>,
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
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                    legend: SemanticTokensLegend {
                        token_types: TOKEN_TYPES.to_vec(),
                        token_modifiers: vec![
                            SemanticTokenModifier::DECLARATION,
                            SemanticTokenModifier::DEFINITION,
                            SemanticTokenModifier::READONLY,
                            SemanticTokenModifier::STATIC,
                            SemanticTokenModifier::DEPRECATED,
                            SemanticTokenModifier::ABSTRACT,
                            SemanticTokenModifier::ASYNC,
                            SemanticTokenModifier::MODIFICATION,
                            SemanticTokenModifier::DOCUMENTATION,
                            SemanticTokenModifier::DEFINITION,
                        ],
                    },
                    range: None,
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                }),
            ),
            ..Default::default()
        }
    }

    pub fn compile_for_errors(&mut self, uri: &Uri) -> anyhow::Result<()> {
        let Some(document) = self.open_documents.get_mut(uri) else {
            return Ok(());
        };

        let tokens = mc::tokenizer::tokenize(&document.contents);
        let errors = if !tokens.errors.is_empty() {
            Some(tokens.errors)
        } else {
            let mut parsed_unit = mc::parser::parse(&tokens);
            let errors = parsed_unit.errors;
            parsed_unit.errors = Vec::new();
            if !errors.is_empty() {
                Some(errors)
            } else {
                let mut compiled_unit = mc::compiler::compile_to_assembly(&parsed_unit);
                let errors = compiled_unit.errors;
                compiled_unit.errors = Vec::new();
                compiled_unit.highlighs.sort_by(|a, b| {
                    if a.span.line < b.span.line
                        || (a.span.line == b.span.line && a.span.column < b.span.column)
                    {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                });
                document.compiled_unit = Some(compiled_unit);
                if !errors.is_empty() {
                    Some(errors)
                } else {
                    None
                }
            }
        };

        document.tokens = tokens.tokens;

        let diagnostics: Vec<Diagnostic> = errors.map_or(Vec::new(), |errors| {
            errors
                .iter()
                .map(|error| Diagnostic {
                    range: Range {
                        start: Position {
                            line: error.span.line as u32,
                            character: error.span.column as u32,
                        },
                        end: Position {
                            line: error.span.endline as u32,
                            character: error.span.endcolumn as u32 + 1,
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
            language_id: _,
            version: _,
            text: contents,
        } = text_document;
        self.open_documents.insert(
            uri.clone(),
            DocumentEntry {
                contents,
                tokens: Vec::new(),
                compiled_unit: None,
            },
        );
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

    pub fn get_tokens(&self, params: lsp_types::SemanticTokensParams) -> SemanticTokensResult {
        let Some(doc) = self.open_documents.get(&params.text_document.uri) else {
            return SemanticTokens {
                ..Default::default()
            }
            .into();
        };

        let mut data = Vec::new();
        let mut prev_line = 0;
        let mut prev_column = 0;

        let tokens = &mut doc.tokens.iter();

        let mut highlights = doc
            .compiled_unit
            .as_ref()
            .map_or([].iter(), |u| u.highlighs.iter());
        let mut token_opt = tokens.next();
        let mut highlight_opt = highlights.next();

        loop {
            match (token_opt, highlight_opt) {
                (None, None) => break,
                (None, Some(highlight)) => {
                    add_semantic_token_from_highlight(
                        &mut data,
                        &mut prev_line,
                        &mut prev_column,
                        highlight,
                    );
                    highlight_opt = highlights.next();
                }
                (Some(token), None) => {
                    add_semantic_token(&mut data, &mut prev_line, &mut prev_column, token);
                    token_opt = tokens.next();
                }
                (Some(token), Some(highlight)) => {
                    if token.span.line < highlight.span.line
                        || (token.span.line == highlight.span.line
                            && token.span.column < highlight.span.column)
                    {
                        add_semantic_token(&mut data, &mut prev_line, &mut prev_column, token);
                        token_opt = tokens.next();
                    } else {
                        add_semantic_token_from_highlight(
                            &mut data,
                            &mut prev_line,
                            &mut prev_column,
                            highlight,
                        );
                        highlight_opt = highlights.next();
                    }
                }
            }
        }

        SemanticTokens {
            result_id: None,
            data,
        }
        .into()
    }
}

fn add_semantic_token(
    data: &mut Vec<SemanticToken>,
    prev_line: &mut u32,
    prev_column: &mut u32,
    token: &TokenSpanned,
) {
    let token_type = match &token.token {
        Token::IntLiteral(_) => SemanticTokenType::NUMBER,
        Token::CharacterLiteral(_) => SemanticTokenType::NUMBER, //?
        Token::BoolLiteral(_) => SemanticTokenType::NUMBER,
        Token::FloatLiteral(_) => SemanticTokenType::NUMBER,
        Token::Semicolon => return,
        Token::DataType(_) => SemanticTokenType::TYPE,
        Token::Identifier(_) => return, // Identifiers are highlighted separately
        Token::EqualSign => return,
        Token::CompareEqual => SemanticTokenType::OPERATOR,
        Token::PlusSign => SemanticTokenType::OPERATOR,
        Token::MultiplySign => SemanticTokenType::OPERATOR,
        Token::MinusSign => SemanticTokenType::OPERATOR,
        Token::DivisionSign => SemanticTokenType::OPERATOR,
        Token::ParenthesisOpen => return,
        Token::ParenthesisClose => return,
        Token::CurlyBracketOpen => return,
        Token::CurlyBracketClose => return,
        Token::Public => SemanticTokenType::KEYWORD,
        Token::String => SemanticTokenType::TYPE,
        Token::StringLiteral(_) => SemanticTokenType::STRING,
        Token::Extern => SemanticTokenType::KEYWORD,
        Token::Ampersand => SemanticTokenType::OPERATOR,
        Token::SquareParenthesisOpen => return,
        Token::SquareParenthesisClose => return,
        Token::LargerThan => SemanticTokenType::OPERATOR,
        Token::SmallerThan => SemanticTokenType::OPERATOR,
        Token::If => SemanticTokenType::KEYWORD,
        Token::Else => SemanticTokenType::KEYWORD,
        Token::Coma => return,
        Token::Colon => return,
        Token::Return => SemanticTokenType::KEYWORD,
        Token::Struct => SemanticTokenType::KEYWORD,
        Token::Period => return,
        Token::While => SemanticTokenType::KEYWORD,
        Token::For => SemanticTokenType::KEYWORD,
    };
    let (delta_line, delta_start) = if *prev_line == token.span.line as u32 {
        (0, token.span.column as u32 - *prev_column)
    } else {
        (
            token.span.line as u32 - *prev_line,
            token.span.column as u32,
        )
    };
    let sem = SemanticToken {
        delta_line,
        delta_start,
        length: (token.span.endcolumn - token.span.column + 1) as u32,
        token_type: TOKEN_TYPES.iter().position(|t| *t == token_type).unwrap() as u32,
        token_modifiers_bitset: 0,
    };
    *prev_line = token.span.line as u32;
    *prev_column = token.span.column as u32;
    data.push(sem);
}

fn add_semantic_token_from_highlight(
    data: &mut Vec<SemanticToken>,
    prev_line: &mut u32,
    prev_column: &mut u32,
    highlight: &Highlight,
) {
    let token_type = match highlight.kind {
        HighlightKind::Parameter => SemanticTokenType::PARAMETER,
        HighlightKind::Property => SemanticTokenType::PROPERTY,
        HighlightKind::Variable => SemanticTokenType::VARIABLE,
        HighlightKind::Struct => SemanticTokenType::STRUCT,
        HighlightKind::Function => SemanticTokenType::FUNCTION,
    };
    let (delta_line, delta_start) = if *prev_line == highlight.span.line as u32 {
        (0, highlight.span.column as u32 - *prev_column)
    } else {
        (
            highlight.span.line as u32 - *prev_line,
            highlight.span.column as u32,
        )
    };
    let sem = SemanticToken {
        delta_line,
        delta_start,
        length: (highlight.span.endcolumn - highlight.span.column + 1) as u32,
        token_type: TOKEN_TYPES.iter().position(|t| *t == token_type).unwrap() as u32,
        token_modifiers_bitset: 0,
    };
    *prev_line = highlight.span.line as u32;
    *prev_column = highlight.span.column as u32;
    data.push(sem);
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
