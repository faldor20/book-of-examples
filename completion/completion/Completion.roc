## Implements basic Lsp Types for
## Init, Hover, DidOpen, DidChange, Completion
app "completion"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.9.0/oKWkaruh2zXxin_xfsYsCJobH1tO8_JvNkFzDwwzNUQ.tar.br", lsp: "../lsp/package.roc" }
    imports [
        pf.Stdout,
        ParserWithLoc.{ Paragraph },
        lsp.LspTypes.{ CompletionItem, CompletionResponse },
        lsp.CompletionItemKind,
        lsp.Types.Option.{ Option, some, none },
    ]
    provides [main] to pf

## checks if pos is within the range, inclusive of start and end
isInRange = \range, pos ->
    pos >= range.start && pos <= range.end

# TODO: completionsFromParagraph: Paragraph->CompletionItem
completionsFromParagraph : _ -> List CompletionItem
completionsFromParagraph = \paragraph ->
    word <- paragraph.val |> List.map
    wordStr = word.val
    # TODO: use actual LSP types
    { label: wordStr, kind: (CompletionItemKind.from Text |> some), detail: (wordStr |> some), documentation: none {} }

# getCompletion:_->Result (List CompletionItem) _
getCompletion = \position, document ->
    ## This should be a binary search or we could store ranges in a dict
    paragraph <-
        document
        |> List.findFirst \p -> p.region |> isInRange position
        |> Result.mapErr NoPara
        |> Result.map

    paragraph |> completionsFromParagraph


handleCompletion : _, _ -> Result CompletionResponse _
handleCompletion = \state, completionRequest ->
    completions =
        state.docs
        |> Dict.get completionRequest.textDocument.uri
        |> Result.mapErr NoDoc
        |> Result.try \doc ->
            getCompletion completionRequest.position doc
    completions

handleDidChange: _, Document -> _
handleDocumentUpdate = \state, docPath, documentBytes ->
    parsedDoc = documentBytes |> ParserWithLoc.parse
    docs = state.docs |> Dict.insert docPath parsedDoc
    { state & docs }

