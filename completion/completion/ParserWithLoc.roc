interface ParserWithLoc
    exposes [
        parse,
        Paragraph
    ]
    imports [
    ]

## This is going to be the standin for our compiler.
## In a usual LS implementation you would call the langauges compiler and have it parse and then typecheck your files. Here We will simulate that for our text format
## The smallest unit of our "language" will be a word
Word : Token Str
## Words will be in paragraphs:
Paragraph : Token (List Word)
## many Paragraphs will be in a document
Document : List Paragraph
## Now lets go about parsing this whole thing.

## Offset into our document
Pos : U64
## Range that a symbol is within
Range : { start : Pos, end : Pos }

Token a : { region : Range, val : a }

# First lets define what a word is, in this case we well say a word is can be letters, numbers or a hyphen or apostrophy
isWordChar = \byte ->
    (byte >= 'a' && byte <= 'z') || (byte >= 'A' && byte <= 'Z') || (byte >= '0' && byte <= '9') || byte == '-' || byte == '\''

parseWord = \bytes, word, start ->
    dbg "parsing word"

    when bytes is
        [first, .. as rest] if isWordChar first ->
            parseWord rest (word |> List.append first) start

        rest ->
            { word: (word |> Str.fromUtf8), rest, region: { start, end: (word |> List.len) + start } }

parseParagraph = \bytesP, startCount ->
    dbg "parsing parar"

    offset = (startCount - (bytesP |> List.len))
    loop = \bytes, paragraph ->
        dbg "parsing parar loop"

        # We have to do this seperately becasue of a compiler bug, once fixed we can just use one when is branch
        when bytes is
            ['\n', '\n', .. as rest] ->
                { paragraph: { val: paragraph, region: { start: offset, end: startCount } }, rest }

            _ ->
                when bytes is
                    [first, .. as wordRest] if isWordChar first ->
                        when parseWord wordRest [first] (startCount - (bytes |> List.len)) is
                            { word: Ok str, rest, region } ->
                                loop rest (paragraph |> List.append { val: str, region })

                            { word: _, rest, region: _ } ->
                                # If the word is invalid we just don't add it. This is actually impossible because we already said words should only contain certain chars
                                loop rest paragraph

                    [_, .. as rest] -> loop rest paragraph
                    [] -> { paragraph: { val: paragraph, region: { start: offset, end: startCount } }, rest: [] }
    loop bytesP []

parseDocument = \bytesD ->
    startCount = (bytesD |> List.len)
    loop = \bytes, document ->
        dbg "parsing doc  loop"

        when bytes |> parseParagraph startCount is
            { paragraph: { val: [] }, rest: [] } -> document
            { paragraph, rest: [] } -> document |> List.append paragraph
            { paragraph: { val: [] }, rest } -> loop rest document
            { paragraph, rest } -> loop rest (document |> List.append paragraph)
    loop bytesD []

parse : List U8 -> Document
parse = \bytes -> parseDocument bytes

expect
    parsed =
        """
        Hi I'm Eli. You are reading my tutorial. 

        This is the second paragraph.
        """
        |> Str.toUtf8
        |> parse

    dbg "parsed"

    expected = [
        {
            region: { end: 72, start: 0 },
            val: [
                { region: { end: 2, start: 0 }, val: "Hi" },
                { region: { end: 6, start: 3 }, val: "I'm" },
                { region: { end: 10, start: 7 }, val: "Eli" },
                { region: { end: 15, start: 12 }, val: "You" },
                { region: { end: 19, start: 16 }, val: "are" },
                { region: { end: 27, start: 20 }, val: "reading" },
                { region: { end: 30, start: 28 }, val: "my" },
                { region: { end: 39, start: 31 }, val: "tutorial" },
            ],
        },
        {
            region: { end: 72, start: 43 },
            val: [
                { region: { end: 47, start: 43 }, val: "This" },
                { region: { end: 50, start: 48 }, val: "is" },
                { region: { end: 54, start: 51 }, val: "the" },
                { region: { end: 61, start: 55 }, val: "second" },
                { region: { end: 71, start: 62 }, val: "paragraph" },
            ],
        },
    ]

    parsed == expected
