interface Types.Union2
    exposes [
        Union2,
        get,
        u1,
        u2,
    ]
    imports [
        Decode,
        TotallyNotJson,
    ]

Union2 u1 u2 := [U1 u1, U2 u2]
    implements [
        Eq {
            isEq: union2Eq,
        },
        Decoding {
            decoder: decodeUnionTwo,
        },
        Encoding {
         toEncoder 
        }
    ]

union2Eq = \@Union2 a, @Union2 b -> a == b
u1 = \item -> @Union2 (U1 item)
u2 = \item -> @Union2 (U2 item)
get = \@Union2 union -> union
decodeUnionTwo = Decode.custom \bytes, fmt ->
    when bytes |> Decode.decodeWith (Decode.decoder) fmt is
        { result: Ok res, rest } -> { result: Ok (u1 res), rest }
        _ ->
            when bytes |> Decode.decodeWith (Decode.decoder) fmt is
                { result: Ok res, rest } -> { result: Ok (u2 res), rest }
                { result: Err res, rest } -> { result: Err res, rest }

toEncoder=\@Union2 val->
    when val is 
        U1 u->u|>Encode.toEncoder 
        U2 u->u|>Encode.toEncoder 

expect
    encoded =
        dat : {union:Union2 U8 Str ,other:Str}
        dat = { union:u2 "hey" , other: "hi" }
        Encode.toBytes dat TotallyNotJson.json
        |> Str.fromUtf8

    expected = Ok "{\"other\":\"hi\",\"union\":\"hey\"}"
    expected == encoded
expect
    name : Result (Union2 U8 { hi : U8 }) _
    name = "{\"hi\":1}" |> Str.toUtf8 |> Decode.fromBytes TotallyNotJson.json
    name == Ok (u2 { hi: 1 })

expect
    name : Result (Union2 U8 { hi : U8 }) _
    name = "{\"hi\":1}" |> Str.toUtf8 |> Decode.fromBytes TotallyNotJson.json
    name == Ok (u2 { hi: 1 })

expect
    name : Result { field : Union2 Str U8 } _
    name =
        """
        {"field":"hi"}
        """
        |> Str.toUtf8
        |> Decode.fromBytes TotallyNotJson.json
    name == Ok ({ field: u1 "hi" })