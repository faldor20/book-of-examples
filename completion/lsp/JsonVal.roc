interface JsonVal
    exposes [
        JsonVal,
        to,
        from,
    ]

    imports [
        DecodeUtils,
        Core,
    ]

# ====Utils====
getUnchecked : List a, U64 -> a
getUnchecked = \lst, index ->
    when List.get lst index is
        Ok a -> a
        Err _ ->
            indexS = index |> Num.toStr
            crash "index: $(indexS) out of bounds"
isWhitespace = \b ->
    when b is
        ' ' | '\n' | '\r' | '\t' -> Bool.true
        _ -> Bool.false

eatWhitespace = \bytes ->
    when bytes is
        [a, ..] if isWhitespace a -> eatWhitespace (List.dropFirst bytes 1)
        _ -> bytes

decErr = \rest -> { rest, result: Err TooShort }
decOk = \rest, res -> { rest, result: Ok res }

# ====JsonVal====
# Doesn't support decoding null
JsonVal := [
    Bool Bool,
    Null,
    Num Dec,
    String Str,
    Record (Dict Str JsonVal),
    List (List JsonVal),
]
    implements [
        Decoding {
            decoder: jsonValDecoder,
        },
        Encoding {
            toEncoder: toEncoder,
        },
        Eq,
    ]

to = \@JsonVal v -> v
from = \v -> @JsonVal v

jList = \lst -> @JsonVal (List lst)
jRec = \rec -> @JsonVal (Record rec)
jRecFromList = \rec -> @JsonVal (Record (Dict.fromList rec))
jString = \str -> @JsonVal (String str)
jNum = \num -> @JsonVal (Num num)
jBool = \b -> @JsonVal (Bool b)
jNull = @JsonVal Null

toEncoder = \@JsonVal a ->
    Encode.custom \bytes, fmt ->
        when a is
            Bool b -> bytes |> Encode.append b fmt
            Null -> bytes |> List.concat ['n', 'u', 'l', 'l']
            Num dec -> bytes |> Encode.append dec fmt
            String str -> bytes |> Encode.append str fmt
            List ls -> bytes |> Encode.append ls fmt
            Record rec -> bytes |> encodeRec rec
encodeRec = \bytes, rec ->
    rec
    |> Dict.toList
    |> List.walk (bytes |> List.append '{') \recBytes, (key, val) ->
        recBytes
        |> Encode.append key Core.json
        |> List.append ':'
        |> Encode.append val Core.json
        |> List.append ','

record = \bytes ->
    recordField bytes (Dict.empty {})
recordField = \wBytes, dict ->
    bytes = eatWhitespace wBytes
    when bytes is
        ['}', .. as rest] -> decOk rest dict
        ['"', ..] ->
            when takeStrRaw bytes is
                { rest, result: Ok key } ->
                    when eatWhitespace rest is
                        [':', .. as rest2] ->
                            when rest2 |> Decode.decodeWith valueDecode Core.json is
                                { rest: rest3, result: Ok v } ->
                                    nDict = dict |> Dict.insert key v
                                    when eatWhitespace rest3 is
                                        [',', .. as rest4] -> recordField rest4 (nDict |> Dict.insert key v)
                                        ['}', .. as rest4] -> decOk rest4 (nDict |> Dict.insert key v)
                                        a -> decErr a

                                { result: Err e } -> decErr bytes

                        rest2 -> decErr rest2

                { result: Err e } -> decErr bytes

        a -> decErr a

list = \bytes ->
    when bytes is
        [']', .. as rest] -> decOk rest []
        listBytes ->
            listBody listBytes []

listBody = \bytes, lst ->
    when bytes |> Decode.decodeWith valueDecode Core.json is
        { rest, result: Ok v } ->
            newLst = lst |> List.append v
            when eatWhitespace rest is
                [']', .. as rest2] -> decOk rest2 newLst
                [',', .. as rest2] -> listBody rest2 newLst
                a -> decErr a

        { rest, result: Err e } -> decErr bytes

isNumStart = \char -> char >= '0' && char <= '9' || char == '+' || char == '-' || char == '.' || char == 'e' || char == 'E'
valueDecode =
    ##TODO try removing this
    Decode.custom \wBytes, fmt ->
        out = \t, v -> @JsonVal (t v)
        bytes = eatWhitespace wBytes

        when bytes is
            ['{', .. as rest] ->
                record rest |> Decode.mapResult \rec -> out Record rec

            ['[', .. as rest] ->
                list rest |> Decode.mapResult \lst -> out List lst

            ['"', ..] ->
                takeJStrRaw bytes

            # Decode.fromBytesPartial bytes Core.json |>Decode.mapResult \str-> out String str
            [num, ..] if isNumStart num ->
                Decode.fromBytesPartial bytes Core.json |> Decode.mapResult \n -> out Num n

            ['f', 'a', 'l', 's', 'e', ..] -> decOk (List.dropFirst bytes 5) (out Bool Bool.false)
            ['t', 'r', 'u', 'e', ..] -> decOk (List.dropFirst bytes 4) (out Bool Bool.false)
            ['n', 'u', 'l', 'l', ..] -> decOk (List.dropFirst bytes 4) (@JsonVal Null)
            _ -> decErr bytes

isValidEnd : U8 -> Bool
isValidEnd = \b ->
    when b is
        ']' | ',' | ' ' | '\n' | '\r' | '\t' | '}' -> Bool.true
        _ -> Bool.false
takeStrRaw = \bytes ->
    # walk pairwise along the list
    (_, index) =
        # TODO: this could potentially crash if bytes is 0
        bytes
        |> List.walkWithIndexUntil (bytes |> getUnchecked 1, 0) \(bb, _), b, idx ->
            i = idx + 1
            # TODO this can crash but should be caught by the break below
            next = bytes |> getUnchecked (i + 1)
            when (bb, next) is
                ('\\', '"') -> Continue (' ', i)
                ('"', _) -> Break (next, i)
                (_, '"') -> Break (next, i + 1)
                _ -> Continue (next, i)

    when Str.fromUtf8 (bytes |> List.sublist { start: 1, len: index }) is
        Ok s ->
            decOk (bytes |> List.dropFirst (index + 1)) (s)

        Err a ->
            decErr bytes
takeJStrRaw = \bytes ->
    # walk pairwise along the list
    (_, index) =
        # TODO: this could potentially crash if bytes is 0
        bytes
        |> List.walkWithIndexUntil (bytes |> getUnchecked 1, 0) \(bb, _), b, idx ->
            i = idx + 1
            # TODO this can crash but should be caught by the break below
            next = bytes |> getUnchecked (i + 1)
            when (bb, next) is
                ('\\', '"') -> Continue (' ', i)
                ('"', _) -> Break (next, i)
                (_, '"') -> Break (next, i + 1)
                _ -> Continue (next, i)

    when Str.fromUtf8 (bytes |> List.sublist { start: 1, len: index }) is
        Ok s ->
            decOk (bytes |> List.dropFirst (index + 1)) (jString (s))

        Err a ->
            decErr bytes

expect
    input =
        """
        h\\"\\"":
        """
        |> Str.toUtf8
    dbg input

    res = takeStrRaw input
    expected = "h\\\"\\\""

    dbg expected

    strOk =
        when res.result is
            Ok a ->
                a == expected

            _ -> Bool.false
    res.rest == [':'] && strOk

expect
    input =
        """
        @googuns_prod: 2c26cbe22e000000<br>\\nJanuary 01, 2015 at 04:58PM":
        """
        |> Str.toUtf8
    dbg input

    res = takeStrRaw input
    expected = "@googuns_prod: 2c26cbe22e000000<br>\\nJanuary 01, 2015 at 04:58PM"

    dbg expected

    strOk =
        when res.result is
            Ok a ->
                dbg a

                a == expected

            _ -> Bool.false
    res.rest == [':'] && strOk

jsonValDecoder =
    valueDecode

expect
    bytes =
        # """
        # {"hi":{"num":10,"num2":-10,"null":null,"bool":true,"list":["hi","there"]}}
        # """
        """
        ["hi","there",["list"]]
        """
        |> Str.toUtf8
    res = Decode.fromBytes bytes Core.json |> Result.map to

    expected =
        jList [
            jString "hi",
            jString "there",
            jList [jString "list"],
        ]
        |> to

    res == Ok expected

# expect
#     bytes =
#         # """
#         # {"hi":{"num":10,"num2":-10,"null":null,"bool":true,"list":["hi","there"]}}
#         # """
#         """
#         [89]
#         """
#         |> Str.toUtf8
#     res = Decode.fromBytes bytes Core.json|>Result.map to

#     expected=
#         jList  [
#             jRawNum ['8','9'],
#         ]
#         |>to

expect
    bytes =
        """
        {"hi":{"num":10,"num2":-10,"null":null,"bool":true,"list":["hi","there"]}}
        """
        |> Str.toUtf8
    res = Decode.fromBytes bytes Core.json |> Result.map to
    hiVal = Dict.fromList [
        ("num", jNum 10),
        ("num2", jNum (-10)),
        ("null", jNull),
        ("bool", jBool Bool.true),
        ("list", jList [jString "hi", jString "there"]),
    ]
    expected =
        jRecFromList [
            ("hi", jRec hiVal),
        ]
        |> to

    when res is
        Ok (Record v) ->
            hi = Dict.get v "hi" |> Result.withDefault (jString "no hi")
            when hi is
                @JsonVal (Record hiRec) ->
                    dbg hiVal

                    dbg hiRec

                    hiVal == hiRec

                _ -> Bool.false

        _ -> Bool.false
