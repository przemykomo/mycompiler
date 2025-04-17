use std::env;

use lsp_types::SemanticTokensParams;
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification as _,
};
use lsp_types::request::{Request as _, SemanticTokensFullRequest};

use lsp_server::{Connection, ExtractError, Message, Notification, Request, Response};
use server::ServerState;
use tracing::info;

mod server;

fn main() -> anyhow::Result<()> {
    let appender = tracing_appender::rolling::never("/tmp/", "mc-lsp.log");
    let (non_blocking_appender, _guard) = tracing_appender::non_blocking(appender);
    tracing_subscriber::fmt()
        .with_target(false)
        .with_level(false)
        .with_thread_names(false)
        .without_time()
        .with_ansi(false)
        .with_writer(non_blocking_appender)
        .init();
    unsafe { env::set_var("RUST_BACKTRACE", "1") };
    std::panic::set_hook(Box::new(tracing_panic::panic_hook));

    info!("mc-lsp booting up");
    let (connection, io_threads) = Connection::stdio();
    let server_capabilities = serde_json::to_value(ServerState::capabilities())?;
    let initialization_params = match connection.initialize(server_capabilities) {
        Ok(it) => it,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }
            return Err(e.into());
        }
    };
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    info!("shutting down server");
    Ok(())
}

fn main_loop(connection: Connection, _params: serde_json::Value) -> anyhow::Result<()> {
    let mut server = ServerState::new();
    // let params: InitializeParams = serde_json::from_value(params)?;
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                match handle_request(&mut server, req) {
                    Ok(Some(resp)) => {
                        connection.sender.send(Message::Response(resp))?;
                    }
                    Ok(None) => {}
                    Err(err) => {
                        info!("{}", &err);
                        // let message = ShowMessageParams {
                        //     typ: MessageType::ERROR,
                        //     message: err.to_string(),
                        // };
                        // let params = serde_json::to_value(&message)?;
                        // let response = Request {
                        //     // TODO: super illegal
                        //     id: 0.into(),
                        //     method: ShowMessageRequest::METHOD.to_string(),
                        //     params,
                        // };
                        // connection.sender.send(Message::Request(response))?;
                    }
                }
            }
            Message::Response(_resp) => {}
            Message::Notification(not) => {
                handle_notification(&mut server, not)?;
            }
        }
        for msg in server.server_messages() {
            connection.sender.send(msg)?;
        }
    }
    Ok(())
}

fn handle_request(server: &mut ServerState, req: Request) -> anyhow::Result<Option<Response>> {
    Ok(match req.method.as_str() {
        // GotoDefinition::METHOD => match req.extract(GotoDefinition::METHOD) {
        //     Ok((id, params)) => {
        //         eprintln!("got gotoDefinition request #{id}: {params:?}\n");
        //         let result = server.goto_definition(params)?;
        //         let result = serde_json::to_value(result)?;
        //         Some(Response {
        //             id,
        //             result: Some(result),
        //             error: None,
        //         })
        //     }
        //     Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
        //     Err(ExtractError::MethodMismatch(_)) => unreachable!(),
        // },
        // HoverRequest::METHOD => match req.extract(HoverRequest::METHOD) {
        //     Ok((id, params)) => {
        //         eprintln!("got gotoDefinition request #{id}: {params:?}\n");
        //         let result = server.hover(params)?;
        //         let result = serde_json::to_value(result)?;
        //         Some(Response {
        //             id,
        //             result: Some(result),
        //             error: None,
        //         })
        //     }
        //     Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
        //     Err(ExtractError::MethodMismatch(_)) => unreachable!(),
        // },
        SemanticTokensFullRequest::METHOD => {
            match req.extract::<SemanticTokensParams>(SemanticTokensFullRequest::METHOD) {
                Ok((id, params)) => {
                    let result = server.get_tokens(params);
                    let result = serde_json::to_value(result)?;
                    let response = Response {
                        id,
                        result: Some(result),
                        error: None,
                    };
                    Some(response)
                }
                Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                Err(ExtractError::MethodMismatch(_)) => unreachable!(),
            }
        }
        _ => None,
    })
}

fn handle_notification(server: &mut ServerState, notif: Notification) -> anyhow::Result<()> {
    match notif.method.as_str() {
        DidOpenTextDocument::METHOD => match notif.extract(DidOpenTextDocument::METHOD) {
            Ok(params) => {
                server.did_open_text_document(params)?;
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(_)) => unreachable!(),
        },
        DidChangeTextDocument::METHOD => match notif.extract(DidChangeTextDocument::METHOD) {
            Ok(params) => {
                server.did_change_text_document(params)?;
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(_)) => unreachable!(),
        },
        DidCloseTextDocument::METHOD => match notif.extract(DidCloseTextDocument::METHOD) {
            Ok(params) => {
                server.did_close_text_document(params);
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(_)) => unreachable!(),
        },
        _ => {}
    }
    Ok(())
}
