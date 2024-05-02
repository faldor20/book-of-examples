## Implements basic Lsp Types for
## Init, Hover, DidOpen, DidChange, Completion
app "completion"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.9.0/oKWkaruh2zXxin_xfsYsCJobH1tO8_JvNkFzDwwzNUQ.tar.br" }
    imports [
        pf.Stdout,
        ParserWithLoc.{Paragraph}
    ]
    provides [main] to pf

## checks if pos is within the range, inclusive of start and end
isInRange= \range, pos->
    pos>=range.start&&pos<=range.end

# TODO: completionsFromParagraph: Paragraph->CompletionItem
completionsFromParagraph= \paragraph->
    word<-paragraph.val|>List.map 
    wordStr=word.val
    #TODO: use actual LSP types
    {label:wordStr,kind:(Some Text)}

getCompletion = \position,document->
    ## This should be a binary search or we could store ranges in a dict
    paragraph<-
        document
        |>List.findFirst \p->p.region|>isInRange position 
        |>Result.mapErr NoPara
        |>Result.map

    paragraph|>completionsFromParagraph
handleCompletion =\state,completionRequest->
    state

handleDocumentUpdate= \state,docPath,documentBytes->
    parsedDoc=documentBytes|>ParserWithLoc.parse
    docs=state.docs|>Dict.insert docPath parsedDoc
    {state & docs}


main=\a->Stdout.line
