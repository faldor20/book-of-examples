app "lang-server"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br" }
    imports [pf.Stdout,
     pf.Stdin,
     pf.Task,
     Decode.{ DecodeResult },
     Union.{ Union2, Option }, Core,
     Types.Option,
     LspTypes.{ RequestMessage, ResponseMessage,HoverMessage,HoverRequest },
     JsonRpc
    ]
    provides [main] to pf

handleHoverMessage: HoverRequest->_
handleHoverMessage=\ hover->
    hover

main =
    messageBytes<-JsonRpc.messageLoop





