app "error"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.9.0/oKWkaruh2zXxin_xfsYsCJobH1tO8_JvNkFzDwwzNUQ.tar.br" }
    imports [
        pf.Stdout,
    ]
    provides [main] to pf
main = Stdout.line "bad"
isTwo = \bytes ->
    when bytes is
        ['\n', '\n', .. as rest] -> Bool.true
        [a, .. as rest] if a == 'h' -> isTwo rest
        [_, .. as rest] -> isTwo rest
        [] -> Bool.false
expect isTwo ['\n'] |> Bool.not
