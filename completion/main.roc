app "lang-server"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br" }
    imports [
        pf.Stdout,
        pf.Stdin,
        pf.Task,
        Decode.{ DecodeResult },
        Core,
        Types.Union2.{ u1, u2 },
        Types.Option.{ some, none },
        LspTypes.{ RequestMessage, HoverMessage, HoverRequest, toResponse,  LspMessage, notificationMessage },
        JsonRpc,
        Handlers.{ handleRequest, handleNotification },
        ServerMessages.{ notificationEncode, makeLog, messageType },
        Wrap.{from,to}
    ]
    provides [main] to pf

runRpc =
    messageBytes <- JsonRpc.messageLoop
    msg <- messageBytes |> Decode.fromBytes Core.json |> Task.fromResult |> Task.await
    a : LspMessage
    a = msg
    res =
        when Types.Union2.get msg is
            U1 reqOpaque ->
                req = from reqOpaque
                responseMsg =
                    when handleRequest req.params is
                        Ok response ->
                            { id: some req.id, result: some response, error: none {} }

                        Err err ->
                            { id: some req.id, result: none {}, error: some err }
                {} <- responseMsg |>Encode.toBytes Core.json|> JsonRpc.sendBytes|> Task.await
                Task.ok Continue

            U2 notifOpaque ->
                notif = notificationMessage notifOpaque
                when handleNotification notif.params is
                    Ok {} -> Task.ok Continue
                    Err err ->

                        errMsg <-
                            err
                            |> Encode.toBytes Core.json
                            |> Str.fromUtf8
                            |> Task.fromResult
                            |> Task.await
                        log = (makeLog (messageType Error) errMsg) |> notificationEncode Core.json
                        {} <- log |> JsonRpc.sendBytes |> Task.await

                        Task.ok Continue

    res

main = Task.ok {}

