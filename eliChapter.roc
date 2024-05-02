
app "lang-server"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.9.0/oKWkaruh2zXxin_xfsYsCJobH1tO8_JvNkFzDwwzNUQ.tar.br" }
    imports [
        # Wrap.{ from, to },
        pf.Task,
        pf.Stdout
   ]
    provides [main] to pf

main =  Stdout.line "ho"
