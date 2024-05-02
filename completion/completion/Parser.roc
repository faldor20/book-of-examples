interface Parser
    exposes [
        parse,
    ]
    imports [
    ]

## This is going to be the standin for our compiler.
## In a usual LS implementation you would call the langauges compiler and have it parse and then typecheck your files. Here We will simulate that for our text format

## The smallest unit of our "language" will be a word
Word : Str
## Words will be in paragraphs:
Paragraph : List Word
## many Paragraphs will be in a document
Document : List Paragraph
## Now lets go about parsing this whole thing.

# First lets define what a word is, in this case we well say a word is can be letters, numbers or a hyphen or apostrophy
isWordChar = \byte ->
    (byte >= 'a' && byte <= 'z') || (byte >= 'A' && byte <= 'Z') || (byte >= '0' && byte <= '9') || byte == '-' || byte == '\''

parseWord = \bytes, word ->
    when bytes is
        [first, .. as rest] if isWordChar first ->
            parseWord rest (word |> List.append first)

        rest ->
            { word: (word |> Str.fromUtf8), rest }

parseParagraph = \bytes, paragraph ->
    # We have to do this seperately becasue of a compiler bug, once fixed we can just use one when is branch
    when bytes is
        ['\n', '\n', .. as rest] ->
            { paragraph, rest }

        _ ->
            when bytes is
                [first, .. as wordRest] if isWordChar first ->
                    when parseWord wordRest [first] is
                        { word: Ok str, rest } ->
                            parseParagraph rest (paragraph |> List.append str)

                        { word: _, rest } ->
                            # If the word is invalid we just don't add it. This is actually impossible because we already said words should only contain certain chars
                            parseParagraph rest paragraph

                [_, .. as rest] -> parseParagraph rest paragraph
                [] -> { paragraph, rest: [] }

parseDocument = \bytes, document ->
    when bytes |> parseParagraph [] is
        { paragraph: [], rest: [] } -> document
        { paragraph, rest: [] } -> document |> List.append paragraph
        { paragraph: [], rest } -> parseDocument rest document
        { paragraph, rest } -> parseDocument rest (document |> List.append paragraph)

parse : List U8 -> Document
parse = \bytes -> parseDocument bytes []

expect
    parsed =
        """
        Hi I'm Eli. You are reading my tutorial. 

        This is the second paragraph.
        """
        |> Str.toUtf8
        |> parse
    expected =
        [["Hi", "I'm", "Eli", "You", "are", "reading", "my", "tutorial"], ["This", "is", "the", "second", "paragraph"]]
    parsed == expected
