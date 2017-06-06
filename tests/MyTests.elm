-- This module requires the following packages:
-- * elm-community/elm-test
-- * bartavelle/json-helpers
module MyTests exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Json.Decode exposing (field, Value)
import Json.Encode
import Json.Helpers exposing (..)
import Test exposing (..)
import Expect exposing (..)
import String

recordDecode : Test
recordDecode = describe "Record decoding checks"
              [ recordDecode1
              , recordDecode2
              ]

recordEncode : Test
recordEncode = describe "Record encoding checks"
              [ recordEncode1
              , recordEncode2
              ]

sumDecode : Test
sumDecode = describe "Sum decoding checks"
              [ sumDecode01
              , sumDecode02
              , sumDecode03
              , sumDecode04
              , sumDecode05
              , sumDecode06
              , sumDecode07
              , sumDecode08
              , sumDecode09
              , sumDecode10
              , sumDecode11
              , sumDecode12
              , sumDecodeUntagged
              ]

sumEncode : Test
sumEncode = describe "Sum encoding checks"
              [ sumEncode01
              , sumEncode02
              , sumEncode03
              , sumEncode04
              , sumEncode05
              , sumEncode06
              , sumEncode07
              , sumEncode08
              , sumEncode09
              , sumEncode10
              , sumEncode11
              , sumEncode12
              , sumEncodeUntagged
              ]

simpleDecode : Test
simpleDecode = describe "Simple records/types checks"
                [ simpleDecode01
                , simpleDecode02
                , simpleDecode03
                , simpleDecode04
                , simplerecordDecode01
                , simplerecordDecode02
                , simplerecordDecode03
                , simplerecordDecode04
                ]

simpleEncode : Test
simpleEncode = describe "Simple records/types checks"
                [ simpleEncode01
                , simpleEncode02
                , simpleEncode03
                , simpleEncode04
                , simplerecordEncode01
                , simplerecordEncode02
                , simplerecordEncode03
                , simplerecordEncode04
                ]

-- this is done to prevent artificial differences due to object ordering, this won't work with Maybe's though :(
equalHack : String -> String -> Expectation
equalHack a b =
    let remix = Json.Decode.decodeString Json.Decode.value
    in equal (remix a) (remix b)

type Record1 a = Record1
   { foo: Int
   , bar: (Maybe Int)
   , baz: a
   , qux: (Maybe a)
   }

jsonDecRecord1 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Record1 a )
jsonDecRecord1 localDecoder_a =
   ("foo" := Json.Decode.int) >>= \pfoo ->
   (Json.Decode.maybe ("bar" := Json.Decode.int)) >>= \pbar ->
   ("baz" := localDecoder_a) >>= \pbaz ->
   (Json.Decode.maybe ("qux" := localDecoder_a)) >>= \pqux ->
   Json.Decode.succeed (Record1 {foo = pfoo, bar = pbar, baz = pbaz, qux = pqux})

jsonEncRecord1 : (a -> Value) -> Record1 a -> Value
jsonEncRecord1 localEncoder_a (Record1 val) =
   Json.Encode.object
   [ ("foo", Json.Encode.int val.foo)
   , ("bar", (maybeEncode (Json.Encode.int)) val.bar)
   , ("baz", localEncoder_a val.baz)
   , ("qux", (maybeEncode (localEncoder_a)) val.qux)
   ]



type Record2 a = Record2
   { foo: Int
   , bar: (Maybe Int)
   , baz: a
   , qux: (Maybe a)
   }

jsonDecRecord2 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Record2 a )
jsonDecRecord2 localDecoder_a =
   ("foo" := Json.Decode.int) >>= \pfoo ->
   (Json.Decode.maybe ("bar" := Json.Decode.int)) >>= \pbar ->
   ("baz" := localDecoder_a) >>= \pbaz ->
   (Json.Decode.maybe ("qux" := localDecoder_a)) >>= \pqux ->
   Json.Decode.succeed (Record2 {foo = pfoo, bar = pbar, baz = pbaz, qux = pqux})

jsonEncRecord2 : (a -> Value) -> Record2 a -> Value
jsonEncRecord2 localEncoder_a (Record2 val) =
   Json.Encode.object
   [ ("foo", Json.Encode.int val.foo)
   , ("bar", (maybeEncode (Json.Encode.int)) val.bar)
   , ("baz", localEncoder_a val.baz)
   , ("qux", (maybeEncode (localEncoder_a)) val.qux)
   ]



type Sum01 a =
    Sum01A a
    | Sum01B (Maybe a)
    | Sum01C a a
    | Sum01D {foo: a}
    | Sum01E {bar: Int, baz: Int}

jsonDecSum01 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum01 a )
jsonDecSum01 localDecoder_a =
    let jsonDecDictSum01 = Dict.fromList
            [ ("Sum01A", Json.Decode.map Sum01A (localDecoder_a))
            , ("Sum01B", Json.Decode.map Sum01B (Json.Decode.maybe (localDecoder_a)))
            , ("Sum01C", Json.Decode.map2 Sum01C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a)))
            , ("Sum01D", Json.Decode.map Sum01D (   ("foo" := localDecoder_a) >>= \pfoo ->    Json.Decode.succeed {foo = pfoo}))
            , ("Sum01E", Json.Decode.map Sum01E (   ("bar" := Json.Decode.int) >>= \pbar ->    ("baz" := Json.Decode.int) >>= \pbaz ->    Json.Decode.succeed {bar = pbar, baz = pbaz}))
            ]
        jsonDecObjectSetSum01 = Set.fromList ["Sum01D", "Sum01E"]
    in  decodeSumTaggedObject "Sum01" "tag" "content" jsonDecDictSum01 jsonDecObjectSetSum01

jsonEncSum01 : (a -> Value) -> Sum01 a -> Value
jsonEncSum01 localEncoder_a val =
    let keyval v = case v of
                    Sum01A v1 -> ("Sum01A", encodeValue (localEncoder_a v1))
                    Sum01B v1 -> ("Sum01B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum01C v1 v2 -> ("Sum01C", encodeValue (Json.Encode.list [localEncoder_a v1, localEncoder_a v2]))
                    Sum01D vs -> ("Sum01D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum01E vs -> ("Sum01E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum02 a =
    Sum02A a
    | Sum02B (Maybe a)
    | Sum02C a a
    | Sum02D {foo: a}
    | Sum02E {bar: Int, baz: Int}

jsonDecSum02 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum02 a )
jsonDecSum02 localDecoder_a =
    let jsonDecDictSum02 = Dict.fromList
            [ ("Sum02A", Json.Decode.map Sum02A (localDecoder_a))
            , ("Sum02B", Json.Decode.map Sum02B (Json.Decode.maybe (localDecoder_a)))
            , ("Sum02C", Json.Decode.map2 Sum02C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a)))
            , ("Sum02D", Json.Decode.map Sum02D (   ("foo" := localDecoder_a) >>= \pfoo ->    Json.Decode.succeed {foo = pfoo}))
            , ("Sum02E", Json.Decode.map Sum02E (   ("bar" := Json.Decode.int) >>= \pbar ->    ("baz" := Json.Decode.int) >>= \pbaz ->    Json.Decode.succeed {bar = pbar, baz = pbaz}))
            ]
        jsonDecObjectSetSum02 = Set.fromList ["Sum02D", "Sum02E"]
    in  decodeSumTaggedObject "Sum02" "tag" "content" jsonDecDictSum02 jsonDecObjectSetSum02

jsonEncSum02 : (a -> Value) -> Sum02 a -> Value
jsonEncSum02 localEncoder_a val =
    let keyval v = case v of
                    Sum02A v1 -> ("Sum02A", encodeValue (localEncoder_a v1))
                    Sum02B v1 -> ("Sum02B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum02C v1 v2 -> ("Sum02C", encodeValue (Json.Encode.list [localEncoder_a v1, localEncoder_a v2]))
                    Sum02D vs -> ("Sum02D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum02E vs -> ("Sum02E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum03 a =
    Sum03A a
    | Sum03B (Maybe a)
    | Sum03C a a
    | Sum03D {foo: a}
    | Sum03E {bar: Int, baz: Int}

jsonDecSum03 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum03 a )
jsonDecSum03 localDecoder_a =
    let jsonDecDictSum03 = Dict.fromList
            [ ("Sum03A", Json.Decode.map Sum03A (localDecoder_a))
            , ("Sum03B", Json.Decode.map Sum03B (Json.Decode.maybe (localDecoder_a)))
            , ("Sum03C", Json.Decode.map2 Sum03C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a)))
            , ("Sum03D", Json.Decode.map Sum03D (   ("foo" := localDecoder_a) >>= \pfoo ->    Json.Decode.succeed {foo = pfoo}))
            , ("Sum03E", Json.Decode.map Sum03E (   ("bar" := Json.Decode.int) >>= \pbar ->    ("baz" := Json.Decode.int) >>= \pbaz ->    Json.Decode.succeed {bar = pbar, baz = pbaz}))
            ]
        jsonDecObjectSetSum03 = Set.fromList ["Sum03D", "Sum03E"]
    in  decodeSumTaggedObject "Sum03" "tag" "content" jsonDecDictSum03 jsonDecObjectSetSum03

jsonEncSum03 : (a -> Value) -> Sum03 a -> Value
jsonEncSum03 localEncoder_a val =
    let keyval v = case v of
                    Sum03A v1 -> ("Sum03A", encodeValue (localEncoder_a v1))
                    Sum03B v1 -> ("Sum03B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum03C v1 v2 -> ("Sum03C", encodeValue (Json.Encode.list [localEncoder_a v1, localEncoder_a v2]))
                    Sum03D vs -> ("Sum03D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum03E vs -> ("Sum03E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum04 a =
    Sum04A a
    | Sum04B (Maybe a)
    | Sum04C a a
    | Sum04D {foo: a}
    | Sum04E {bar: Int, baz: Int}

jsonDecSum04 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum04 a )
jsonDecSum04 localDecoder_a =
    let jsonDecDictSum04 = Dict.fromList
            [ ("Sum04A", Json.Decode.map Sum04A (localDecoder_a))
            , ("Sum04B", Json.Decode.map Sum04B (Json.Decode.maybe (localDecoder_a)))
            , ("Sum04C", Json.Decode.map2 Sum04C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a)))
            , ("Sum04D", Json.Decode.map Sum04D (   ("foo" := localDecoder_a) >>= \pfoo ->    Json.Decode.succeed {foo = pfoo}))
            , ("Sum04E", Json.Decode.map Sum04E (   ("bar" := Json.Decode.int) >>= \pbar ->    ("baz" := Json.Decode.int) >>= \pbaz ->    Json.Decode.succeed {bar = pbar, baz = pbaz}))
            ]
        jsonDecObjectSetSum04 = Set.fromList ["Sum04D", "Sum04E"]
    in  decodeSumTaggedObject "Sum04" "tag" "content" jsonDecDictSum04 jsonDecObjectSetSum04

jsonEncSum04 : (a -> Value) -> Sum04 a -> Value
jsonEncSum04 localEncoder_a val =
    let keyval v = case v of
                    Sum04A v1 -> ("Sum04A", encodeValue (localEncoder_a v1))
                    Sum04B v1 -> ("Sum04B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum04C v1 v2 -> ("Sum04C", encodeValue (Json.Encode.list [localEncoder_a v1, localEncoder_a v2]))
                    Sum04D vs -> ("Sum04D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum04E vs -> ("Sum04E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum05 a =
    Sum05A a
    | Sum05B (Maybe a)
    | Sum05C a a
    | Sum05D {foo: a}
    | Sum05E {bar: Int, baz: Int}

jsonDecSum05 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum05 a )
jsonDecSum05 localDecoder_a =
    let jsonDecDictSum05 = Dict.fromList
            [ ("Sum05A", Json.Decode.map Sum05A (localDecoder_a))
            , ("Sum05B", Json.Decode.map Sum05B (Json.Decode.maybe (localDecoder_a)))
            , ("Sum05C", Json.Decode.map2 Sum05C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a)))
            , ("Sum05D", Json.Decode.map Sum05D (   ("foo" := localDecoder_a) >>= \pfoo ->    Json.Decode.succeed {foo = pfoo}))
            , ("Sum05E", Json.Decode.map Sum05E (   ("bar" := Json.Decode.int) >>= \pbar ->    ("baz" := Json.Decode.int) >>= \pbaz ->    Json.Decode.succeed {bar = pbar, baz = pbaz}))
            ]
    in  decodeSumObjectWithSingleField  "Sum05" jsonDecDictSum05

jsonEncSum05 : (a -> Value) -> Sum05 a -> Value
jsonEncSum05 localEncoder_a val =
    let keyval v = case v of
                    Sum05A v1 -> ("Sum05A", encodeValue (localEncoder_a v1))
                    Sum05B v1 -> ("Sum05B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum05C v1 v2 -> ("Sum05C", encodeValue (Json.Encode.list [localEncoder_a v1, localEncoder_a v2]))
                    Sum05D vs -> ("Sum05D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum05E vs -> ("Sum05E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum06 a =
    Sum06A a
    | Sum06B (Maybe a)
    | Sum06C a a
    | Sum06D {foo: a}
    | Sum06E {bar: Int, baz: Int}

jsonDecSum06 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum06 a )
jsonDecSum06 localDecoder_a =
    let jsonDecDictSum06 = Dict.fromList
            [ ("Sum06A", Json.Decode.map Sum06A (localDecoder_a))
            , ("Sum06B", Json.Decode.map Sum06B (Json.Decode.maybe (localDecoder_a)))
            , ("Sum06C", Json.Decode.map2 Sum06C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a)))
            , ("Sum06D", Json.Decode.map Sum06D (   ("foo" := localDecoder_a) >>= \pfoo ->    Json.Decode.succeed {foo = pfoo}))
            , ("Sum06E", Json.Decode.map Sum06E (   ("bar" := Json.Decode.int) >>= \pbar ->    ("baz" := Json.Decode.int) >>= \pbaz ->    Json.Decode.succeed {bar = pbar, baz = pbaz}))
            ]
    in  decodeSumObjectWithSingleField  "Sum06" jsonDecDictSum06

jsonEncSum06 : (a -> Value) -> Sum06 a -> Value
jsonEncSum06 localEncoder_a val =
    let keyval v = case v of
                    Sum06A v1 -> ("Sum06A", encodeValue (localEncoder_a v1))
                    Sum06B v1 -> ("Sum06B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum06C v1 v2 -> ("Sum06C", encodeValue (Json.Encode.list [localEncoder_a v1, localEncoder_a v2]))
                    Sum06D vs -> ("Sum06D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum06E vs -> ("Sum06E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum07 a =
    Sum07A a
    | Sum07B (Maybe a)
    | Sum07C a a
    | Sum07D {foo: a}
    | Sum07E {bar: Int, baz: Int}

jsonDecSum07 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum07 a )
jsonDecSum07 localDecoder_a =
    let jsonDecDictSum07 = Dict.fromList
            [ ("Sum07A", Json.Decode.map Sum07A (localDecoder_a))
            , ("Sum07B", Json.Decode.map Sum07B (Json.Decode.maybe (localDecoder_a)))
            , ("Sum07C", Json.Decode.map2 Sum07C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a)))
            , ("Sum07D", Json.Decode.map Sum07D (   ("foo" := localDecoder_a) >>= \pfoo ->    Json.Decode.succeed {foo = pfoo}))
            , ("Sum07E", Json.Decode.map Sum07E (   ("bar" := Json.Decode.int) >>= \pbar ->    ("baz" := Json.Decode.int) >>= \pbaz ->    Json.Decode.succeed {bar = pbar, baz = pbaz}))
            ]
    in  decodeSumObjectWithSingleField  "Sum07" jsonDecDictSum07

jsonEncSum07 : (a -> Value) -> Sum07 a -> Value
jsonEncSum07 localEncoder_a val =
    let keyval v = case v of
                    Sum07A v1 -> ("Sum07A", encodeValue (localEncoder_a v1))
                    Sum07B v1 -> ("Sum07B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum07C v1 v2 -> ("Sum07C", encodeValue (Json.Encode.list [localEncoder_a v1, localEncoder_a v2]))
                    Sum07D vs -> ("Sum07D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum07E vs -> ("Sum07E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum08 a =
    Sum08A a
    | Sum08B (Maybe a)
    | Sum08C a a
    | Sum08D {foo: a}
    | Sum08E {bar: Int, baz: Int}

jsonDecSum08 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum08 a )
jsonDecSum08 localDecoder_a =
    let jsonDecDictSum08 = Dict.fromList
            [ ("Sum08A", Json.Decode.map Sum08A (localDecoder_a))
            , ("Sum08B", Json.Decode.map Sum08B (Json.Decode.maybe (localDecoder_a)))
            , ("Sum08C", Json.Decode.map2 Sum08C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a)))
            , ("Sum08D", Json.Decode.map Sum08D (   ("foo" := localDecoder_a) >>= \pfoo ->    Json.Decode.succeed {foo = pfoo}))
            , ("Sum08E", Json.Decode.map Sum08E (   ("bar" := Json.Decode.int) >>= \pbar ->    ("baz" := Json.Decode.int) >>= \pbaz ->    Json.Decode.succeed {bar = pbar, baz = pbaz}))
            ]
    in  decodeSumObjectWithSingleField  "Sum08" jsonDecDictSum08

jsonEncSum08 : (a -> Value) -> Sum08 a -> Value
jsonEncSum08 localEncoder_a val =
    let keyval v = case v of
                    Sum08A v1 -> ("Sum08A", encodeValue (localEncoder_a v1))
                    Sum08B v1 -> ("Sum08B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum08C v1 v2 -> ("Sum08C", encodeValue (Json.Encode.list [localEncoder_a v1, localEncoder_a v2]))
                    Sum08D vs -> ("Sum08D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum08E vs -> ("Sum08E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum09 a =
    Sum09A a
    | Sum09B (Maybe a)
    | Sum09C a a
    | Sum09D {foo: a}
    | Sum09E {bar: Int, baz: Int}

jsonDecSum09 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum09 a )
jsonDecSum09 localDecoder_a =
    let jsonDecDictSum09 = Dict.fromList
            [ ("Sum09A", Json.Decode.map Sum09A (localDecoder_a))
            , ("Sum09B", Json.Decode.map Sum09B (Json.Decode.maybe (localDecoder_a)))
            , ("Sum09C", Json.Decode.map2 Sum09C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a)))
            , ("Sum09D", Json.Decode.map Sum09D (   ("foo" := localDecoder_a) >>= \pfoo ->    Json.Decode.succeed {foo = pfoo}))
            , ("Sum09E", Json.Decode.map Sum09E (   ("bar" := Json.Decode.int) >>= \pbar ->    ("baz" := Json.Decode.int) >>= \pbaz ->    Json.Decode.succeed {bar = pbar, baz = pbaz}))
            ]
    in  decodeSumTwoElemArray  "Sum09" jsonDecDictSum09

jsonEncSum09 : (a -> Value) -> Sum09 a -> Value
jsonEncSum09 localEncoder_a val =
    let keyval v = case v of
                    Sum09A v1 -> ("Sum09A", encodeValue (localEncoder_a v1))
                    Sum09B v1 -> ("Sum09B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum09C v1 v2 -> ("Sum09C", encodeValue (Json.Encode.list [localEncoder_a v1, localEncoder_a v2]))
                    Sum09D vs -> ("Sum09D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum09E vs -> ("Sum09E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Sum10 a =
    Sum10A a
    | Sum10B (Maybe a)
    | Sum10C a a
    | Sum10D {foo: a}
    | Sum10E {bar: Int, baz: Int}

jsonDecSum10 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum10 a )
jsonDecSum10 localDecoder_a =
    let jsonDecDictSum10 = Dict.fromList
            [ ("Sum10A", Json.Decode.map Sum10A (localDecoder_a))
            , ("Sum10B", Json.Decode.map Sum10B (Json.Decode.maybe (localDecoder_a)))
            , ("Sum10C", Json.Decode.map2 Sum10C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a)))
            , ("Sum10D", Json.Decode.map Sum10D (   ("foo" := localDecoder_a) >>= \pfoo ->    Json.Decode.succeed {foo = pfoo}))
            , ("Sum10E", Json.Decode.map Sum10E (   ("bar" := Json.Decode.int) >>= \pbar ->    ("baz" := Json.Decode.int) >>= \pbaz ->    Json.Decode.succeed {bar = pbar, baz = pbaz}))
            ]
    in  decodeSumTwoElemArray  "Sum10" jsonDecDictSum10

jsonEncSum10 : (a -> Value) -> Sum10 a -> Value
jsonEncSum10 localEncoder_a val =
    let keyval v = case v of
                    Sum10A v1 -> ("Sum10A", encodeValue (localEncoder_a v1))
                    Sum10B v1 -> ("Sum10B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum10C v1 v2 -> ("Sum10C", encodeValue (Json.Encode.list [localEncoder_a v1, localEncoder_a v2]))
                    Sum10D vs -> ("Sum10D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum10E vs -> ("Sum10E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Sum11 a =
    Sum11A a
    | Sum11B (Maybe a)
    | Sum11C a a
    | Sum11D {foo: a}
    | Sum11E {bar: Int, baz: Int}

jsonDecSum11 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum11 a )
jsonDecSum11 localDecoder_a =
    let jsonDecDictSum11 = Dict.fromList
            [ ("Sum11A", Json.Decode.map Sum11A (localDecoder_a))
            , ("Sum11B", Json.Decode.map Sum11B (Json.Decode.maybe (localDecoder_a)))
            , ("Sum11C", Json.Decode.map2 Sum11C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a)))
            , ("Sum11D", Json.Decode.map Sum11D (   ("foo" := localDecoder_a) >>= \pfoo ->    Json.Decode.succeed {foo = pfoo}))
            , ("Sum11E", Json.Decode.map Sum11E (   ("bar" := Json.Decode.int) >>= \pbar ->    ("baz" := Json.Decode.int) >>= \pbaz ->    Json.Decode.succeed {bar = pbar, baz = pbaz}))
            ]
    in  decodeSumTwoElemArray  "Sum11" jsonDecDictSum11

jsonEncSum11 : (a -> Value) -> Sum11 a -> Value
jsonEncSum11 localEncoder_a val =
    let keyval v = case v of
                    Sum11A v1 -> ("Sum11A", encodeValue (localEncoder_a v1))
                    Sum11B v1 -> ("Sum11B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum11C v1 v2 -> ("Sum11C", encodeValue (Json.Encode.list [localEncoder_a v1, localEncoder_a v2]))
                    Sum11D vs -> ("Sum11D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum11E vs -> ("Sum11E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Sum12 a =
    Sum12A a
    | Sum12B (Maybe a)
    | Sum12C a a
    | Sum12D {foo: a}
    | Sum12E {bar: Int, baz: Int}

jsonDecSum12 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum12 a )
jsonDecSum12 localDecoder_a =
    let jsonDecDictSum12 = Dict.fromList
            [ ("Sum12A", Json.Decode.map Sum12A (localDecoder_a))
            , ("Sum12B", Json.Decode.map Sum12B (Json.Decode.maybe (localDecoder_a)))
            , ("Sum12C", Json.Decode.map2 Sum12C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a)))
            , ("Sum12D", Json.Decode.map Sum12D (   ("foo" := localDecoder_a) >>= \pfoo ->    Json.Decode.succeed {foo = pfoo}))
            , ("Sum12E", Json.Decode.map Sum12E (   ("bar" := Json.Decode.int) >>= \pbar ->    ("baz" := Json.Decode.int) >>= \pbaz ->    Json.Decode.succeed {bar = pbar, baz = pbaz}))
            ]
    in  decodeSumTwoElemArray  "Sum12" jsonDecDictSum12

jsonEncSum12 : (a -> Value) -> Sum12 a -> Value
jsonEncSum12 localEncoder_a val =
    let keyval v = case v of
                    Sum12A v1 -> ("Sum12A", encodeValue (localEncoder_a v1))
                    Sum12B v1 -> ("Sum12B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum12C v1 v2 -> ("Sum12C", encodeValue (Json.Encode.list [localEncoder_a v1, localEncoder_a v2]))
                    Sum12D vs -> ("Sum12D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum12E vs -> ("Sum12E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Simple01 a =
    Simple01 a

jsonDecSimple01 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple01 a )
jsonDecSimple01 localDecoder_a =
    Json.Decode.map Simple01 (localDecoder_a)


jsonEncSimple01 : (a -> Value) -> Simple01 a -> Value
jsonEncSimple01 localEncoder_a(Simple01 v1) =
    localEncoder_a v1



type Simple02 a =
    Simple02 a

jsonDecSimple02 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple02 a )
jsonDecSimple02 localDecoder_a =
    Json.Decode.map Simple02 (localDecoder_a)


jsonEncSimple02 : (a -> Value) -> Simple02 a -> Value
jsonEncSimple02 localEncoder_a(Simple02 v1) =
    localEncoder_a v1



type Simple03 a =
    Simple03 a

jsonDecSimple03 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple03 a )
jsonDecSimple03 localDecoder_a =
    Json.Decode.map Simple03 (localDecoder_a)


jsonEncSimple03 : (a -> Value) -> Simple03 a -> Value
jsonEncSimple03 localEncoder_a(Simple03 v1) =
    localEncoder_a v1



type Simple04 a =
    Simple04 a

jsonDecSimple04 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple04 a )
jsonDecSimple04 localDecoder_a =
    Json.Decode.map Simple04 (localDecoder_a)


jsonEncSimple04 : (a -> Value) -> Simple04 a -> Value
jsonEncSimple04 localEncoder_a(Simple04 v1) =
    localEncoder_a v1



type SimpleRecord01 a = SimpleRecord01
   { qux: a
   }

jsonDecSimpleRecord01 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord01 a )
jsonDecSimpleRecord01 localDecoder_a =
   ("qux" := localDecoder_a) >>= \pqux ->
   Json.Decode.succeed (SimpleRecord01 {qux = pqux})

jsonEncSimpleRecord01 : (a -> Value) -> SimpleRecord01 a -> Value
jsonEncSimpleRecord01 localEncoder_a (SimpleRecord01 val) =
   Json.Encode.object
   [ ("qux", localEncoder_a val.qux)
   ]



type SimpleRecord02 a = SimpleRecord02
   { qux: a
   }

jsonDecSimpleRecord02 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord02 a )
jsonDecSimpleRecord02 localDecoder_a =
   (localDecoder_a) >>= \pqux ->
   Json.Decode.succeed (SimpleRecord02 {qux = pqux})

jsonEncSimpleRecord02 : (a -> Value) -> SimpleRecord02 a -> Value
jsonEncSimpleRecord02 localEncoder_a (SimpleRecord02 val) =
   localEncoder_a val.qux


type SimpleRecord03 a = SimpleRecord03
   { qux: a
   }

jsonDecSimpleRecord03 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord03 a )
jsonDecSimpleRecord03 localDecoder_a =
   ("qux" := localDecoder_a) >>= \pqux ->
   Json.Decode.succeed (SimpleRecord03 {qux = pqux})

jsonEncSimpleRecord03 : (a -> Value) -> SimpleRecord03 a -> Value
jsonEncSimpleRecord03 localEncoder_a (SimpleRecord03 val) =
   Json.Encode.object
   [ ("qux", localEncoder_a val.qux)
   ]



type SimpleRecord04 a = SimpleRecord04
   { qux: a
   }

jsonDecSimpleRecord04 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord04 a )
jsonDecSimpleRecord04 localDecoder_a =
   (localDecoder_a) >>= \pqux ->
   Json.Decode.succeed (SimpleRecord04 {qux = pqux})

jsonEncSimpleRecord04 : (a -> Value) -> SimpleRecord04 a -> Value
jsonEncSimpleRecord04 localEncoder_a (SimpleRecord04 val) =
   localEncoder_a val.qux


type SumUntagged a =
    SMInt Int
    | SMList a

jsonDecSumUntagged : Json.Decode.Decoder a -> Json.Decode.Decoder ( SumUntagged a )
jsonDecSumUntagged localDecoder_a =
    let jsonDecDictSumUntagged = Dict.fromList
            [ ("SMInt", Json.Decode.map SMInt (Json.Decode.int))
            , ("SMList", Json.Decode.map SMList (localDecoder_a))
            ]
    in  Json.Decode.oneOf (Dict.values jsonDecDictSumUntagged)

jsonEncSumUntagged : (a -> Value) -> SumUntagged a -> Value
jsonEncSumUntagged localEncoder_a val =
    let keyval v = case v of
                    SMInt v1 -> ("SMInt", encodeValue (Json.Encode.int v1))
                    SMList v1 -> ("SMList", encodeValue (localEncoder_a v1))
    in encodeSumUntagged keyval val



sumEncode01 : Test
sumEncode01 = describe "Sum encode 01"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum01E\",\"bar\":0,\"baz\":0}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list << List.map Json.Encode.int) (Sum01E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum01C\",\"content\":[[2,-1],[1,0]]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list << List.map Json.Encode.int) (Sum01C [2,-1] [1,0]))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list << List.map Json.Encode.int) (Sum01A []))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum01D\",\"foo\":[4]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list << List.map Json.Encode.int) (Sum01D {foo = [4]}))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list << List.map Json.Encode.int) (Sum01A []))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum01B\",\"content\":[2]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list << List.map Json.Encode.int) (Sum01B (Just [2])))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[4,8]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list << List.map Json.Encode.int) (Sum01A [4,8]))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[6]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list << List.map Json.Encode.int) (Sum01A [6]))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[-6,-7,-9,5,-11,-13,-7,6,4,3,13,-2,3,-6]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list << List.map Json.Encode.int) (Sum01A [-6,-7,-9,5,-11,-13,-7,6,4,3,13,-2,3,-6]))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum01D\",\"foo\":[6,2,-1,4,-14,1,-2,-9,-11,16,2]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list << List.map Json.Encode.int) (Sum01D {foo = [6,2,-1,4,-14,1,-2,-9,-11,16,2]}))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[-2,-14,-8,-10,-1,19,-10,8]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list << List.map Json.Encode.int) (Sum01A [-2,-14,-8,-10,-1,19,-10,8]))))
  ]

sumEncode02 : Test
sumEncode02 = describe "Sum encode 02"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum02C\",\"content\":[[],[]]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list << List.map Json.Encode.int) (Sum02C [] []))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":-2,\"baz\":2}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list << List.map Json.Encode.int) (Sum02E {bar = -2, baz = 2}))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":4,\"baz\":0}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list << List.map Json.Encode.int) (Sum02E {bar = 4, baz = 0}))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum02A\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list << List.map Json.Encode.int) (Sum02A []))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":6,\"baz\":-1}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list << List.map Json.Encode.int) (Sum02E {bar = 6, baz = -1}))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum02D\",\"foo\":[-5,4,-1,9,1,2,7,5,9]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list << List.map Json.Encode.int) (Sum02D {foo = [-5,4,-1,9,1,2,7,5,9]}))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum02A\",\"content\":[-11,0,-11,11,2]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list << List.map Json.Encode.int) (Sum02A [-11,0,-11,11,2]))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum02B\",\"content\":[5,8,12,14]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list << List.map Json.Encode.int) (Sum02B (Just [5,8,12,14])))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum02D\",\"foo\":[-10,1,10,4,6,-4,-3,2,-1,8,6,4,15,5,-12,-2]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list << List.map Json.Encode.int) (Sum02D {foo = [-10,1,10,4,6,-4,-3,2,-1,8,6,4,15,5,-12,-2]}))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum02D\",\"foo\":[0,17,-11,3,0,17,-6,7,0,-2,12,-8,11,-2,-3,8]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list << List.map Json.Encode.int) (Sum02D {foo = [0,17,-11,3,0,17,-6,7,0,-2,12,-8,11,-2,-3,8]}))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":-11,\"baz\":16}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list << List.map Json.Encode.int) (Sum02E {bar = -11, baz = 16}))))
  ]

sumEncode03 : Test
sumEncode03 = describe "Sum encode 03"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum03B\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list << List.map Json.Encode.int) (Sum03B (Just [])))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum03C\",\"content\":[[],[1]]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list << List.map Json.Encode.int) (Sum03C [] [1]))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum03B\",\"content\":[2,-3,1]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list << List.map Json.Encode.int) (Sum03B (Just [2,-3,1])))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum03C\",\"content\":[[-4,-1,3],[3]]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list << List.map Json.Encode.int) (Sum03C [-4,-1,3] [3]))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum03C\",\"content\":[[-6,-7,-6,-7],[-3,7,5]]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list << List.map Json.Encode.int) (Sum03C [-6,-7,-6,-7] [-3,7,5]))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum03D\",\"foo\":[-2,7]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list << List.map Json.Encode.int) (Sum03D {foo = [-2,7]}))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum03A\",\"content\":[-10,-4,12,-2,5,4,9,-9]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list << List.map Json.Encode.int) (Sum03A [-10,-4,12,-2,5,4,9,-9]))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum03B\",\"content\":[-1,-9,-4,1,6]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list << List.map Json.Encode.int) (Sum03B (Just [-1,-9,-4,1,6])))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum03D\",\"foo\":[8,13,-6,11,9,-7,9,-1,-6,3,14,-8,5,16]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list << List.map Json.Encode.int) (Sum03D {foo = [8,13,-6,11,9,-7,9,-1,-6,3,14,-8,5,16]}))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum03E\",\"bar\":-11,\"baz\":0}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list << List.map Json.Encode.int) (Sum03E {bar = -11, baz = 0}))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum03D\",\"foo\":[1,13,-8,8,-14,-20,20,16,-1,0,-12,-9,15,19,-18,2,15,10,13]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list << List.map Json.Encode.int) (Sum03D {foo = [1,13,-8,8,-14,-20,20,16,-1,0,-12,-9,15,19,-18,2,15,10,13]}))))
  ]

sumEncode04 : Test
sumEncode04 = describe "Sum encode 04"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum04C\",\"content\":[[],[]]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list << List.map Json.Encode.int) (Sum04C [] []))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list << List.map Json.Encode.int) (Sum04D {foo = []}))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum04B\",\"content\":[-2,-1,0,1]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list << List.map Json.Encode.int) (Sum04B (Just [-2,-1,0,1])))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum04E\",\"bar\":4,\"baz\":-2}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list << List.map Json.Encode.int) (Sum04E {bar = 4, baz = -2}))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[-2,2,8,-4,5,-7,5,-2]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list << List.map Json.Encode.int) (Sum04D {foo = [-2,2,8,-4,5,-7,5,-2]}))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum04A\",\"content\":[-5,-9,-10,-1,1,-10]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list << List.map Json.Encode.int) (Sum04A [-5,-9,-10,-1,1,-10]))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum04C\",\"content\":[[-7,-1],[12]]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list << List.map Json.Encode.int) (Sum04C [-7,-1] [12]))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum04C\",\"content\":[[-1,-8,3,12,0,6,9,-8,13,7,-7],[-4,13,-8,-9,-8,1,-1,-8]]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list << List.map Json.Encode.int) (Sum04C [-1,-8,3,12,0,6,9,-8,13,7,-7] [-4,13,-8,-9,-8,1,-1,-8]))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum04A\",\"content\":[-12,11,-9,1,13,12,-11]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list << List.map Json.Encode.int) (Sum04A [-12,11,-9,1,13,12,-11]))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum04B\",\"content\":[-2,9,0,2,-5,4,17,-5,-17,-12,-6,-4]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list << List.map Json.Encode.int) (Sum04B (Just [-2,9,0,2,-5,4,17,-5,-17,-12,-6,-4])))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum04A\",\"content\":[11,-5]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list << List.map Json.Encode.int) (Sum04A [11,-5]))))
  ]

sumEncode05 : Test
sumEncode05 = describe "Sum encode 05"
  [ test "1" (\_ -> equalHack "{\"Sum05D\":{\"foo\":[]}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list << List.map Json.Encode.int) (Sum05D {foo = []}))))
  , test "2" (\_ -> equalHack "{\"Sum05B\":[]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list << List.map Json.Encode.int) (Sum05B (Just [])))))
  , test "3" (\_ -> equalHack "{\"Sum05C\":[[4,-4,-2],[-1,-4,4]]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list << List.map Json.Encode.int) (Sum05C [4,-4,-2] [-1,-4,4]))))
  , test "4" (\_ -> equalHack "{\"Sum05D\":{\"foo\":[-5,3,2,-4]}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list << List.map Json.Encode.int) (Sum05D {foo = [-5,3,2,-4]}))))
  , test "5" (\_ -> equalHack "{\"Sum05E\":{\"bar\":8,\"baz\":2}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list << List.map Json.Encode.int) (Sum05E {bar = 8, baz = 2}))))
  , test "6" (\_ -> equalHack "{\"Sum05E\":{\"bar\":-4,\"baz\":5}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list << List.map Json.Encode.int) (Sum05E {bar = -4, baz = 5}))))
  , test "7" (\_ -> equalHack "{\"Sum05E\":{\"bar\":-8,\"baz\":-3}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list << List.map Json.Encode.int) (Sum05E {bar = -8, baz = -3}))))
  , test "8" (\_ -> equalHack "{\"Sum05C\":[[3,-10],[]]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list << List.map Json.Encode.int) (Sum05C [3,-10] []))))
  , test "9" (\_ -> equalHack "{\"Sum05E\":{\"bar\":-9,\"baz\":-15}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list << List.map Json.Encode.int) (Sum05E {bar = -9, baz = -15}))))
  , test "10" (\_ -> equalHack "{\"Sum05A\":[18,-11,-4,-4,-4,-13]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list << List.map Json.Encode.int) (Sum05A [18,-11,-4,-4,-4,-13]))))
  , test "11" (\_ -> equalHack "{\"Sum05C\":[[-19,16,11,-8,15,-4,9],[-6,-8,4]]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list << List.map Json.Encode.int) (Sum05C [-19,16,11,-8,15,-4,9] [-6,-8,4]))))
  ]

sumEncode06 : Test
sumEncode06 = describe "Sum encode 06"
  [ test "1" (\_ -> equalHack "{\"Sum06E\":{\"bar\":0,\"baz\":0}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list << List.map Json.Encode.int) (Sum06E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "{\"Sum06A\":[-1]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list << List.map Json.Encode.int) (Sum06A [-1]))))
  , test "3" (\_ -> equalHack "{\"Sum06C\":[[1],[1,-3,-4,2]]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list << List.map Json.Encode.int) (Sum06C [1] [1,-3,-4,2]))))
  , test "4" (\_ -> equalHack "{\"Sum06E\":{\"bar\":-5,\"baz\":1}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list << List.map Json.Encode.int) (Sum06E {bar = -5, baz = 1}))))
  , test "5" (\_ -> equalHack "{\"Sum06D\":{\"foo\":[]}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list << List.map Json.Encode.int) (Sum06D {foo = []}))))
  , test "6" (\_ -> equalHack "{\"Sum06E\":{\"bar\":7,\"baz\":-3}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list << List.map Json.Encode.int) (Sum06E {bar = 7, baz = -3}))))
  , test "7" (\_ -> equalHack "{\"Sum06A\":[-4,-10,-6,-10,0,-6,-6,3,-6,7]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list << List.map Json.Encode.int) (Sum06A [-4,-10,-6,-10,0,-6,-6,3,-6,7]))))
  , test "8" (\_ -> equalHack "{\"Sum06B\":[12,-11,3,-14,-10,13]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list << List.map Json.Encode.int) (Sum06B (Just [12,-11,3,-14,-10,13])))))
  , test "9" (\_ -> equalHack "{\"Sum06C\":[[9,-7],[16,-14,-14,-12,4,-4,-8,-10,-10,8,11,4]]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list << List.map Json.Encode.int) (Sum06C [9,-7] [16,-14,-14,-12,4,-4,-8,-10,-10,8,11,4]))))
  , test "10" (\_ -> equalHack "{\"Sum06D\":{\"foo\":[4,3,2,-3,-16,13,16,-5,-13,-9,0,14,-7,-14,-12]}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list << List.map Json.Encode.int) (Sum06D {foo = [4,3,2,-3,-16,13,16,-5,-13,-9,0,14,-7,-14,-12]}))))
  , test "11" (\_ -> equalHack "{\"Sum06A\":[11,0,19,-16,-18,13,15,-4,17,-7,-16,-7,9,-12,20]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list << List.map Json.Encode.int) (Sum06A [11,0,19,-16,-18,13,15,-4,17,-7,-16,-7,9,-12,20]))))
  ]

sumEncode07 : Test
sumEncode07 = describe "Sum encode 07"
  [ test "1" (\_ -> equalHack "{\"Sum07B\":[]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list << List.map Json.Encode.int) (Sum07B (Just [])))))
  , test "2" (\_ -> equalHack "{\"Sum07E\":{\"bar\":-2,\"baz\":0}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list << List.map Json.Encode.int) (Sum07E {bar = -2, baz = 0}))))
  , test "3" (\_ -> equalHack "{\"Sum07D\":{\"foo\":[]}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list << List.map Json.Encode.int) (Sum07D {foo = []}))))
  , test "4" (\_ -> equalHack "{\"Sum07D\":{\"foo\":[]}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list << List.map Json.Encode.int) (Sum07D {foo = []}))))
  , test "5" (\_ -> equalHack "{\"Sum07D\":{\"foo\":[1,1,-8,7,0]}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list << List.map Json.Encode.int) (Sum07D {foo = [1,1,-8,7,0]}))))
  , test "6" (\_ -> equalHack "{\"Sum07E\":{\"bar\":-2,\"baz\":3}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list << List.map Json.Encode.int) (Sum07E {bar = -2, baz = 3}))))
  , test "7" (\_ -> equalHack "{\"Sum07C\":[[-12,8,-9,-4,-3,-1,0,11,6],[11,-4,-2,-8,0,-11,7,12,-9,11]]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list << List.map Json.Encode.int) (Sum07C [-12,8,-9,-4,-3,-1,0,11,6] [11,-4,-2,-8,0,-11,7,12,-9,11]))))
  , test "8" (\_ -> equalHack "{\"Sum07B\":[12,12,-6,10,-14,-3,-4,-7,7,-3,-5,-1,3]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list << List.map Json.Encode.int) (Sum07B (Just [12,12,-6,10,-14,-3,-4,-7,7,-3,-5,-1,3])))))
  , test "9" (\_ -> equalHack "{\"Sum07C\":[[5,-1,-11,-9,-11,7,16,14,-5,-13,-8,16],[]]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list << List.map Json.Encode.int) (Sum07C [5,-1,-11,-9,-11,7,16,14,-5,-13,-8,16] []))))
  , test "10" (\_ -> equalHack "{\"Sum07C\":[[-10,4,-12],[-11,16,-9,10,-16,5,-4,16,-14,14,16,7]]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list << List.map Json.Encode.int) (Sum07C [-10,4,-12] [-11,16,-9,10,-16,5,-4,16,-14,14,16,7]))))
  , test "11" (\_ -> equalHack "{\"Sum07A\":[4,7,11,18,-5,-7,-9,-16,-14]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list << List.map Json.Encode.int) (Sum07A [4,7,11,18,-5,-7,-9,-16,-14]))))
  ]

sumEncode08 : Test
sumEncode08 = describe "Sum encode 08"
  [ test "1" (\_ -> equalHack "{\"Sum08D\":{\"foo\":[]}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list << List.map Json.Encode.int) (Sum08D {foo = []}))))
  , test "2" (\_ -> equalHack "{\"Sum08E\":{\"bar\":2,\"baz\":1}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list << List.map Json.Encode.int) (Sum08E {bar = 2, baz = 1}))))
  , test "3" (\_ -> equalHack "{\"Sum08D\":{\"foo\":[1,2]}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list << List.map Json.Encode.int) (Sum08D {foo = [1,2]}))))
  , test "4" (\_ -> equalHack "{\"Sum08D\":{\"foo\":[-1,3,-2,-3,-4,1]}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list << List.map Json.Encode.int) (Sum08D {foo = [-1,3,-2,-3,-4,1]}))))
  , test "5" (\_ -> equalHack "{\"Sum08C\":[[3,-8],[5,-4,7,6,-7,5,5]]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list << List.map Json.Encode.int) (Sum08C [3,-8] [5,-4,7,6,-7,5,5]))))
  , test "6" (\_ -> equalHack "{\"Sum08B\":[1,0,-1]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list << List.map Json.Encode.int) (Sum08B (Just [1,0,-1])))))
  , test "7" (\_ -> equalHack "{\"Sum08B\":[]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list << List.map Json.Encode.int) (Sum08B (Just [])))))
  , test "8" (\_ -> equalHack "{\"Sum08A\":[4,5,-1,2,-14,-9,11]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list << List.map Json.Encode.int) (Sum08A [4,5,-1,2,-14,-9,11]))))
  , test "9" (\_ -> equalHack "{\"Sum08C\":[[2,-3,14,10,-5,-5,0,11,1,-4,-2,13,9,5,-9],[9,1,-2,15,-11,3,-14,1,-9,-11,15,-5,-1,-10]]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list << List.map Json.Encode.int) (Sum08C [2,-3,14,10,-5,-5,0,11,1,-4,-2,13,9,5,-9] [9,1,-2,15,-11,3,-14,1,-9,-11,15,-5,-1,-10]))))
  , test "10" (\_ -> equalHack "{\"Sum08C\":[[-11,-4,-5,6,-6,18,16,-18,-17,-16],[-3,6,-16,11,-12,11,-3,11,-2,-3,16,10]]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list << List.map Json.Encode.int) (Sum08C [-11,-4,-5,6,-6,18,16,-18,-17,-16] [-3,6,-16,11,-12,11,-3,11,-2,-3,16,10]))))
  , test "11" (\_ -> equalHack "{\"Sum08D\":{\"foo\":[-20,7,-7,-11,11,4,-1,-20,20,-11,-2,10,-10]}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list << List.map Json.Encode.int) (Sum08D {foo = [-20,7,-7,-11,11,4,-1,-20,20,-11,-2,10,-10]}))))
  ]

sumEncode09 : Test
sumEncode09 = describe "Sum encode 09"
  [ test "1" (\_ -> equalHack "[\"Sum09C\",[[],[]]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list << List.map Json.Encode.int) (Sum09C [] []))))
  , test "2" (\_ -> equalHack "[\"Sum09D\",{\"foo\":[0]}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list << List.map Json.Encode.int) (Sum09D {foo = [0]}))))
  , test "3" (\_ -> equalHack "[\"Sum09C\",[[],[-3,0,3]]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list << List.map Json.Encode.int) (Sum09C [] [-3,0,3]))))
  , test "4" (\_ -> equalHack "[\"Sum09D\",{\"foo\":[-2]}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list << List.map Json.Encode.int) (Sum09D {foo = [-2]}))))
  , test "5" (\_ -> equalHack "[\"Sum09C\",[[0,5,4,6,1,8],[5,7,7]]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list << List.map Json.Encode.int) (Sum09C [0,5,4,6,1,8] [5,7,7]))))
  , test "6" (\_ -> equalHack "[\"Sum09B\",[-1,9,5]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list << List.map Json.Encode.int) (Sum09B (Just [-1,9,5])))))
  , test "7" (\_ -> equalHack "[\"Sum09A\",[5,-12,4,7,-10]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list << List.map Json.Encode.int) (Sum09A [5,-12,4,7,-10]))))
  , test "8" (\_ -> equalHack "[\"Sum09E\",{\"bar\":-3,\"baz\":13}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list << List.map Json.Encode.int) (Sum09E {bar = -3, baz = 13}))))
  , test "9" (\_ -> equalHack "[\"Sum09E\",{\"bar\":-14,\"baz\":-7}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list << List.map Json.Encode.int) (Sum09E {bar = -14, baz = -7}))))
  , test "10" (\_ -> equalHack "[\"Sum09D\",{\"foo\":[5,8,-7,-8,-2,-7,2,-16,-2,8,-6,6,-5,12]}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list << List.map Json.Encode.int) (Sum09D {foo = [5,8,-7,-8,-2,-7,2,-16,-2,8,-6,6,-5,12]}))))
  , test "11" (\_ -> equalHack "[\"Sum09D\",{\"foo\":[9,-20,8,5,6,10,-18,-13,-5,8,8,-16,-20,13,11,-8,16,-16]}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list << List.map Json.Encode.int) (Sum09D {foo = [9,-20,8,5,6,10,-18,-13,-5,8,8,-16,-20,13,11,-8,16,-16]}))))
  ]

sumEncode10 : Test
sumEncode10 = describe "Sum encode 10"
  [ test "1" (\_ -> equalHack "[\"Sum10C\",[[],[]]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list << List.map Json.Encode.int) (Sum10C [] []))))
  , test "2" (\_ -> equalHack "[\"Sum10C\",[[],[0]]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list << List.map Json.Encode.int) (Sum10C [] [0]))))
  , test "3" (\_ -> equalHack "[\"Sum10D\",{\"foo\":[-1,2]}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list << List.map Json.Encode.int) (Sum10D {foo = [-1,2]}))))
  , test "4" (\_ -> equalHack "[\"Sum10D\",{\"foo\":[]}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list << List.map Json.Encode.int) (Sum10D {foo = []}))))
  , test "5" (\_ -> equalHack "[\"Sum10C\",[[3,-4,-4,6,-4,-7,1],[-1,-6,-1,3,7,-5,2,-2]]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list << List.map Json.Encode.int) (Sum10C [3,-4,-4,6,-4,-7,1] [-1,-6,-1,3,7,-5,2,-2]))))
  , test "6" (\_ -> equalHack "[\"Sum10B\",[-9]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list << List.map Json.Encode.int) (Sum10B (Just [-9])))))
  , test "7" (\_ -> equalHack "[\"Sum10E\",{\"bar\":10,\"baz\":5}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list << List.map Json.Encode.int) (Sum10E {bar = 10, baz = 5}))))
  , test "8" (\_ -> equalHack "[\"Sum10A\",[8,7,7,-10,-1,3,8,-13,-14,12,2,7,-10,-10]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list << List.map Json.Encode.int) (Sum10A [8,7,7,-10,-1,3,8,-13,-14,12,2,7,-10,-10]))))
  , test "9" (\_ -> equalHack "[\"Sum10C\",[[4,-4,9,8,1,-11,-13,14],[11,12,-10,-9,-2,12,-1,0,13,9,9,4,-11,6,10]]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list << List.map Json.Encode.int) (Sum10C [4,-4,9,8,1,-11,-13,14] [11,12,-10,-9,-2,12,-1,0,13,9,9,4,-11,6,10]))))
  , test "10" (\_ -> equalHack "[\"Sum10A\",[6,-5,-1,7,-12,-12,-3,-10,-12,5,-3,17,13,-15]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list << List.map Json.Encode.int) (Sum10A [6,-5,-1,7,-12,-12,-3,-10,-12,5,-3,17,13,-15]))))
  , test "11" (\_ -> equalHack "[\"Sum10B\",[16,-4,9,16,-17,-7,10,10,6,-2,-13,7,-4,12,-4]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list << List.map Json.Encode.int) (Sum10B (Just [16,-4,9,16,-17,-7,10,10,6,-2,-13,7,-4,12,-4])))))
  ]

sumEncode11 : Test
sumEncode11 = describe "Sum encode 11"
  [ test "1" (\_ -> equalHack "[\"Sum11B\",[]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list << List.map Json.Encode.int) (Sum11B (Just [])))))
  , test "2" (\_ -> equalHack "[\"Sum11E\",{\"bar\":2,\"baz\":-2}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list << List.map Json.Encode.int) (Sum11E {bar = 2, baz = -2}))))
  , test "3" (\_ -> equalHack "[\"Sum11A\",[2,1]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list << List.map Json.Encode.int) (Sum11A [2,1]))))
  , test "4" (\_ -> equalHack "[\"Sum11C\",[[-4],[-1,3]]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list << List.map Json.Encode.int) (Sum11C [-4] [-1,3]))))
  , test "5" (\_ -> equalHack "[\"Sum11A\",[-1,5,-3,-8,6,-3,0,1]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list << List.map Json.Encode.int) (Sum11A [-1,5,-3,-8,6,-3,0,1]))))
  , test "6" (\_ -> equalHack "[\"Sum11D\",{\"foo\":[7]}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list << List.map Json.Encode.int) (Sum11D {foo = [7]}))))
  , test "7" (\_ -> equalHack "[\"Sum11D\",{\"foo\":[8,-7,-7,-4,-10,-5,6,-9,7]}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list << List.map Json.Encode.int) (Sum11D {foo = [8,-7,-7,-4,-10,-5,6,-9,7]}))))
  , test "8" (\_ -> equalHack "[\"Sum11A\",[1,-9,-4,-6,-5,-6]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list << List.map Json.Encode.int) (Sum11A [1,-9,-4,-6,-5,-6]))))
  , test "9" (\_ -> equalHack "[\"Sum11D\",{\"foo\":[]}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list << List.map Json.Encode.int) (Sum11D {foo = []}))))
  , test "10" (\_ -> equalHack "[\"Sum11E\",{\"bar\":11,\"baz\":-10}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list << List.map Json.Encode.int) (Sum11E {bar = 11, baz = -10}))))
  , test "11" (\_ -> equalHack "[\"Sum11B\",[19,-1,20,-16,19,-4,-6,0,20,6,7,-12,-14,-20,10,-4,14]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list << List.map Json.Encode.int) (Sum11B (Just [19,-1,20,-16,19,-4,-6,0,20,6,7,-12,-14,-20,10,-4,14])))))
  ]

sumEncode12 : Test
sumEncode12 = describe "Sum encode 12"
  [ test "1" (\_ -> equalHack "[\"Sum12B\",[]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list << List.map Json.Encode.int) (Sum12B (Just [])))))
  , test "2" (\_ -> equalHack "[\"Sum12C\",[[2],[0]]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list << List.map Json.Encode.int) (Sum12C [2] [0]))))
  , test "3" (\_ -> equalHack "[\"Sum12E\",{\"bar\":3,\"baz\":-2}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list << List.map Json.Encode.int) (Sum12E {bar = 3, baz = -2}))))
  , test "4" (\_ -> equalHack "[\"Sum12D\",{\"foo\":[1,-3,5,3]}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list << List.map Json.Encode.int) (Sum12D {foo = [1,-3,5,3]}))))
  , test "5" (\_ -> equalHack "[\"Sum12C\",[[],[4,1,-7,-8]]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list << List.map Json.Encode.int) (Sum12C [] [4,1,-7,-8]))))
  , test "6" (\_ -> equalHack "[\"Sum12C\",[[5,9,-5,3,-6,1,-7,-3,-1],[7,-10,-6,1,-5,-9,-9,8,-5]]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list << List.map Json.Encode.int) (Sum12C [5,9,-5,3,-6,1,-7,-3,-1] [7,-10,-6,1,-5,-9,-9,8,-5]))))
  , test "7" (\_ -> equalHack "[\"Sum12C\",[[-11,8,3,3,-3,0,-1],[7,-7,1,5,-11,-3]]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list << List.map Json.Encode.int) (Sum12C [-11,8,3,3,-3,0,-1] [7,-7,1,5,-11,-3]))))
  , test "8" (\_ -> equalHack "[\"Sum12C\",[[9,4,-1,-8,-5],[]]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list << List.map Json.Encode.int) (Sum12C [9,4,-1,-8,-5] []))))
  , test "9" (\_ -> equalHack "[\"Sum12E\",{\"bar\":0,\"baz\":4}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list << List.map Json.Encode.int) (Sum12E {bar = 0, baz = 4}))))
  , test "10" (\_ -> equalHack "[\"Sum12A\",[-10,3,-18,14,4,-6,10,-7,-9]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list << List.map Json.Encode.int) (Sum12A [-10,3,-18,14,4,-6,10,-7,-9]))))
  , test "11" (\_ -> equalHack "[\"Sum12E\",{\"bar\":20,\"baz\":5}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list << List.map Json.Encode.int) (Sum12E {bar = 20, baz = 5}))))
  ]

sumDecode01 : Test
sumDecode01 = describe "Sum decode 01"
  [ test "1" (\_ -> equal (Ok (Sum01E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01E\",\"bar\":0,\"baz\":0}"))
  , test "2" (\_ -> equal (Ok (Sum01C [2,-1] [1,0])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01C\",\"content\":[[2,-1],[1,0]]}"))
  , test "3" (\_ -> equal (Ok (Sum01A [])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[]}"))
  , test "4" (\_ -> equal (Ok (Sum01D {foo = [4]})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01D\",\"foo\":[4]}"))
  , test "5" (\_ -> equal (Ok (Sum01A [])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[]}"))
  , test "6" (\_ -> equal (Ok (Sum01B (Just [2]))) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01B\",\"content\":[2]}"))
  , test "7" (\_ -> equal (Ok (Sum01A [4,8])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[4,8]}"))
  , test "8" (\_ -> equal (Ok (Sum01A [6])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[6]}"))
  , test "9" (\_ -> equal (Ok (Sum01A [-6,-7,-9,5,-11,-13,-7,6,4,3,13,-2,3,-6])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[-6,-7,-9,5,-11,-13,-7,6,4,3,13,-2,3,-6]}"))
  , test "10" (\_ -> equal (Ok (Sum01D {foo = [6,2,-1,4,-14,1,-2,-9,-11,16,2]})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01D\",\"foo\":[6,2,-1,4,-14,1,-2,-9,-11,16,2]}"))
  , test "11" (\_ -> equal (Ok (Sum01A [-2,-14,-8,-10,-1,19,-10,8])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[-2,-14,-8,-10,-1,19,-10,8]}"))
  ]

sumDecode02 : Test
sumDecode02 = describe "Sum decode 02"
  [ test "1" (\_ -> equal (Ok (Sum02C [] [])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02C\",\"content\":[[],[]]}"))
  , test "2" (\_ -> equal (Ok (Sum02E {bar = -2, baz = 2})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":-2,\"baz\":2}"))
  , test "3" (\_ -> equal (Ok (Sum02E {bar = 4, baz = 0})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":4,\"baz\":0}"))
  , test "4" (\_ -> equal (Ok (Sum02A [])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02A\",\"content\":[]}"))
  , test "5" (\_ -> equal (Ok (Sum02E {bar = 6, baz = -1})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":6,\"baz\":-1}"))
  , test "6" (\_ -> equal (Ok (Sum02D {foo = [-5,4,-1,9,1,2,7,5,9]})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02D\",\"foo\":[-5,4,-1,9,1,2,7,5,9]}"))
  , test "7" (\_ -> equal (Ok (Sum02A [-11,0,-11,11,2])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02A\",\"content\":[-11,0,-11,11,2]}"))
  , test "8" (\_ -> equal (Ok (Sum02B (Just [5,8,12,14]))) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02B\",\"content\":[5,8,12,14]}"))
  , test "9" (\_ -> equal (Ok (Sum02D {foo = [-10,1,10,4,6,-4,-3,2,-1,8,6,4,15,5,-12,-2]})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02D\",\"foo\":[-10,1,10,4,6,-4,-3,2,-1,8,6,4,15,5,-12,-2]}"))
  , test "10" (\_ -> equal (Ok (Sum02D {foo = [0,17,-11,3,0,17,-6,7,0,-2,12,-8,11,-2,-3,8]})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02D\",\"foo\":[0,17,-11,3,0,17,-6,7,0,-2,12,-8,11,-2,-3,8]}"))
  , test "11" (\_ -> equal (Ok (Sum02E {bar = -11, baz = 16})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":-11,\"baz\":16}"))
  ]

sumDecode03 : Test
sumDecode03 = describe "Sum decode 03"
  [ test "1" (\_ -> equal (Ok (Sum03B (Just []))) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03B\",\"content\":[]}"))
  , test "2" (\_ -> equal (Ok (Sum03C [] [1])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03C\",\"content\":[[],[1]]}"))
  , test "3" (\_ -> equal (Ok (Sum03B (Just [2,-3,1]))) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03B\",\"content\":[2,-3,1]}"))
  , test "4" (\_ -> equal (Ok (Sum03C [-4,-1,3] [3])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03C\",\"content\":[[-4,-1,3],[3]]}"))
  , test "5" (\_ -> equal (Ok (Sum03C [-6,-7,-6,-7] [-3,7,5])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03C\",\"content\":[[-6,-7,-6,-7],[-3,7,5]]}"))
  , test "6" (\_ -> equal (Ok (Sum03D {foo = [-2,7]})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03D\",\"foo\":[-2,7]}"))
  , test "7" (\_ -> equal (Ok (Sum03A [-10,-4,12,-2,5,4,9,-9])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03A\",\"content\":[-10,-4,12,-2,5,4,9,-9]}"))
  , test "8" (\_ -> equal (Ok (Sum03B (Just [-1,-9,-4,1,6]))) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03B\",\"content\":[-1,-9,-4,1,6]}"))
  , test "9" (\_ -> equal (Ok (Sum03D {foo = [8,13,-6,11,9,-7,9,-1,-6,3,14,-8,5,16]})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03D\",\"foo\":[8,13,-6,11,9,-7,9,-1,-6,3,14,-8,5,16]}"))
  , test "10" (\_ -> equal (Ok (Sum03E {bar = -11, baz = 0})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03E\",\"bar\":-11,\"baz\":0}"))
  , test "11" (\_ -> equal (Ok (Sum03D {foo = [1,13,-8,8,-14,-20,20,16,-1,0,-12,-9,15,19,-18,2,15,10,13]})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03D\",\"foo\":[1,13,-8,8,-14,-20,20,16,-1,0,-12,-9,15,19,-18,2,15,10,13]}"))
  ]

sumDecode04 : Test
sumDecode04 = describe "Sum decode 04"
  [ test "1" (\_ -> equal (Ok (Sum04C [] [])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04C\",\"content\":[[],[]]}"))
  , test "2" (\_ -> equal (Ok (Sum04D {foo = []})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[]}"))
  , test "3" (\_ -> equal (Ok (Sum04B (Just [-2,-1,0,1]))) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04B\",\"content\":[-2,-1,0,1]}"))
  , test "4" (\_ -> equal (Ok (Sum04E {bar = 4, baz = -2})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04E\",\"bar\":4,\"baz\":-2}"))
  , test "5" (\_ -> equal (Ok (Sum04D {foo = [-2,2,8,-4,5,-7,5,-2]})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[-2,2,8,-4,5,-7,5,-2]}"))
  , test "6" (\_ -> equal (Ok (Sum04A [-5,-9,-10,-1,1,-10])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04A\",\"content\":[-5,-9,-10,-1,1,-10]}"))
  , test "7" (\_ -> equal (Ok (Sum04C [-7,-1] [12])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04C\",\"content\":[[-7,-1],[12]]}"))
  , test "8" (\_ -> equal (Ok (Sum04C [-1,-8,3,12,0,6,9,-8,13,7,-7] [-4,13,-8,-9,-8,1,-1,-8])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04C\",\"content\":[[-1,-8,3,12,0,6,9,-8,13,7,-7],[-4,13,-8,-9,-8,1,-1,-8]]}"))
  , test "9" (\_ -> equal (Ok (Sum04A [-12,11,-9,1,13,12,-11])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04A\",\"content\":[-12,11,-9,1,13,12,-11]}"))
  , test "10" (\_ -> equal (Ok (Sum04B (Just [-2,9,0,2,-5,4,17,-5,-17,-12,-6,-4]))) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04B\",\"content\":[-2,9,0,2,-5,4,17,-5,-17,-12,-6,-4]}"))
  , test "11" (\_ -> equal (Ok (Sum04A [11,-5])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04A\",\"content\":[11,-5]}"))
  ]

sumDecode05 : Test
sumDecode05 = describe "Sum decode 05"
  [ test "1" (\_ -> equal (Ok (Sum05D {foo = []})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05D\":{\"foo\":[]}}"))
  , test "2" (\_ -> equal (Ok (Sum05B (Just []))) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05B\":[]}"))
  , test "3" (\_ -> equal (Ok (Sum05C [4,-4,-2] [-1,-4,4])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05C\":[[4,-4,-2],[-1,-4,4]]}"))
  , test "4" (\_ -> equal (Ok (Sum05D {foo = [-5,3,2,-4]})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05D\":{\"foo\":[-5,3,2,-4]}}"))
  , test "5" (\_ -> equal (Ok (Sum05E {bar = 8, baz = 2})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05E\":{\"bar\":8,\"baz\":2}}"))
  , test "6" (\_ -> equal (Ok (Sum05E {bar = -4, baz = 5})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05E\":{\"bar\":-4,\"baz\":5}}"))
  , test "7" (\_ -> equal (Ok (Sum05E {bar = -8, baz = -3})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05E\":{\"bar\":-8,\"baz\":-3}}"))
  , test "8" (\_ -> equal (Ok (Sum05C [3,-10] [])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05C\":[[3,-10],[]]}"))
  , test "9" (\_ -> equal (Ok (Sum05E {bar = -9, baz = -15})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05E\":{\"bar\":-9,\"baz\":-15}}"))
  , test "10" (\_ -> equal (Ok (Sum05A [18,-11,-4,-4,-4,-13])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05A\":[18,-11,-4,-4,-4,-13]}"))
  , test "11" (\_ -> equal (Ok (Sum05C [-19,16,11,-8,15,-4,9] [-6,-8,4])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05C\":[[-19,16,11,-8,15,-4,9],[-6,-8,4]]}"))
  ]

sumDecode06 : Test
sumDecode06 = describe "Sum decode 06"
  [ test "1" (\_ -> equal (Ok (Sum06E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06E\":{\"bar\":0,\"baz\":0}}"))
  , test "2" (\_ -> equal (Ok (Sum06A [-1])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06A\":[-1]}"))
  , test "3" (\_ -> equal (Ok (Sum06C [1] [1,-3,-4,2])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06C\":[[1],[1,-3,-4,2]]}"))
  , test "4" (\_ -> equal (Ok (Sum06E {bar = -5, baz = 1})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06E\":{\"bar\":-5,\"baz\":1}}"))
  , test "5" (\_ -> equal (Ok (Sum06D {foo = []})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06D\":{\"foo\":[]}}"))
  , test "6" (\_ -> equal (Ok (Sum06E {bar = 7, baz = -3})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06E\":{\"bar\":7,\"baz\":-3}}"))
  , test "7" (\_ -> equal (Ok (Sum06A [-4,-10,-6,-10,0,-6,-6,3,-6,7])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06A\":[-4,-10,-6,-10,0,-6,-6,3,-6,7]}"))
  , test "8" (\_ -> equal (Ok (Sum06B (Just [12,-11,3,-14,-10,13]))) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06B\":[12,-11,3,-14,-10,13]}"))
  , test "9" (\_ -> equal (Ok (Sum06C [9,-7] [16,-14,-14,-12,4,-4,-8,-10,-10,8,11,4])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06C\":[[9,-7],[16,-14,-14,-12,4,-4,-8,-10,-10,8,11,4]]}"))
  , test "10" (\_ -> equal (Ok (Sum06D {foo = [4,3,2,-3,-16,13,16,-5,-13,-9,0,14,-7,-14,-12]})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06D\":{\"foo\":[4,3,2,-3,-16,13,16,-5,-13,-9,0,14,-7,-14,-12]}}"))
  , test "11" (\_ -> equal (Ok (Sum06A [11,0,19,-16,-18,13,15,-4,17,-7,-16,-7,9,-12,20])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06A\":[11,0,19,-16,-18,13,15,-4,17,-7,-16,-7,9,-12,20]}"))
  ]

sumDecode07 : Test
sumDecode07 = describe "Sum decode 07"
  [ test "1" (\_ -> equal (Ok (Sum07B (Just []))) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07B\":[]}"))
  , test "2" (\_ -> equal (Ok (Sum07E {bar = -2, baz = 0})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07E\":{\"bar\":-2,\"baz\":0}}"))
  , test "3" (\_ -> equal (Ok (Sum07D {foo = []})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07D\":{\"foo\":[]}}"))
  , test "4" (\_ -> equal (Ok (Sum07D {foo = []})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07D\":{\"foo\":[]}}"))
  , test "5" (\_ -> equal (Ok (Sum07D {foo = [1,1,-8,7,0]})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07D\":{\"foo\":[1,1,-8,7,0]}}"))
  , test "6" (\_ -> equal (Ok (Sum07E {bar = -2, baz = 3})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07E\":{\"bar\":-2,\"baz\":3}}"))
  , test "7" (\_ -> equal (Ok (Sum07C [-12,8,-9,-4,-3,-1,0,11,6] [11,-4,-2,-8,0,-11,7,12,-9,11])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07C\":[[-12,8,-9,-4,-3,-1,0,11,6],[11,-4,-2,-8,0,-11,7,12,-9,11]]}"))
  , test "8" (\_ -> equal (Ok (Sum07B (Just [12,12,-6,10,-14,-3,-4,-7,7,-3,-5,-1,3]))) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07B\":[12,12,-6,10,-14,-3,-4,-7,7,-3,-5,-1,3]}"))
  , test "9" (\_ -> equal (Ok (Sum07C [5,-1,-11,-9,-11,7,16,14,-5,-13,-8,16] [])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07C\":[[5,-1,-11,-9,-11,7,16,14,-5,-13,-8,16],[]]}"))
  , test "10" (\_ -> equal (Ok (Sum07C [-10,4,-12] [-11,16,-9,10,-16,5,-4,16,-14,14,16,7])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07C\":[[-10,4,-12],[-11,16,-9,10,-16,5,-4,16,-14,14,16,7]]}"))
  , test "11" (\_ -> equal (Ok (Sum07A [4,7,11,18,-5,-7,-9,-16,-14])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07A\":[4,7,11,18,-5,-7,-9,-16,-14]}"))
  ]

sumDecode08 : Test
sumDecode08 = describe "Sum decode 08"
  [ test "1" (\_ -> equal (Ok (Sum08D {foo = []})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08D\":{\"foo\":[]}}"))
  , test "2" (\_ -> equal (Ok (Sum08E {bar = 2, baz = 1})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08E\":{\"bar\":2,\"baz\":1}}"))
  , test "3" (\_ -> equal (Ok (Sum08D {foo = [1,2]})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08D\":{\"foo\":[1,2]}}"))
  , test "4" (\_ -> equal (Ok (Sum08D {foo = [-1,3,-2,-3,-4,1]})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08D\":{\"foo\":[-1,3,-2,-3,-4,1]}}"))
  , test "5" (\_ -> equal (Ok (Sum08C [3,-8] [5,-4,7,6,-7,5,5])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08C\":[[3,-8],[5,-4,7,6,-7,5,5]]}"))
  , test "6" (\_ -> equal (Ok (Sum08B (Just [1,0,-1]))) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08B\":[1,0,-1]}"))
  , test "7" (\_ -> equal (Ok (Sum08B (Just []))) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08B\":[]}"))
  , test "8" (\_ -> equal (Ok (Sum08A [4,5,-1,2,-14,-9,11])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08A\":[4,5,-1,2,-14,-9,11]}"))
  , test "9" (\_ -> equal (Ok (Sum08C [2,-3,14,10,-5,-5,0,11,1,-4,-2,13,9,5,-9] [9,1,-2,15,-11,3,-14,1,-9,-11,15,-5,-1,-10])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08C\":[[2,-3,14,10,-5,-5,0,11,1,-4,-2,13,9,5,-9],[9,1,-2,15,-11,3,-14,1,-9,-11,15,-5,-1,-10]]}"))
  , test "10" (\_ -> equal (Ok (Sum08C [-11,-4,-5,6,-6,18,16,-18,-17,-16] [-3,6,-16,11,-12,11,-3,11,-2,-3,16,10])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08C\":[[-11,-4,-5,6,-6,18,16,-18,-17,-16],[-3,6,-16,11,-12,11,-3,11,-2,-3,16,10]]}"))
  , test "11" (\_ -> equal (Ok (Sum08D {foo = [-20,7,-7,-11,11,4,-1,-20,20,-11,-2,10,-10]})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08D\":{\"foo\":[-20,7,-7,-11,11,4,-1,-20,20,-11,-2,10,-10]}}"))
  ]

sumDecode09 : Test
sumDecode09 = describe "Sum decode 09"
  [ test "1" (\_ -> equal (Ok (Sum09C [] [])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09C\",[[],[]]]"))
  , test "2" (\_ -> equal (Ok (Sum09D {foo = [0]})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09D\",{\"foo\":[0]}]"))
  , test "3" (\_ -> equal (Ok (Sum09C [] [-3,0,3])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09C\",[[],[-3,0,3]]]"))
  , test "4" (\_ -> equal (Ok (Sum09D {foo = [-2]})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09D\",{\"foo\":[-2]}]"))
  , test "5" (\_ -> equal (Ok (Sum09C [0,5,4,6,1,8] [5,7,7])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09C\",[[0,5,4,6,1,8],[5,7,7]]]"))
  , test "6" (\_ -> equal (Ok (Sum09B (Just [-1,9,5]))) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09B\",[-1,9,5]]"))
  , test "7" (\_ -> equal (Ok (Sum09A [5,-12,4,7,-10])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09A\",[5,-12,4,7,-10]]"))
  , test "8" (\_ -> equal (Ok (Sum09E {bar = -3, baz = 13})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09E\",{\"bar\":-3,\"baz\":13}]"))
  , test "9" (\_ -> equal (Ok (Sum09E {bar = -14, baz = -7})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09E\",{\"bar\":-14,\"baz\":-7}]"))
  , test "10" (\_ -> equal (Ok (Sum09D {foo = [5,8,-7,-8,-2,-7,2,-16,-2,8,-6,6,-5,12]})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09D\",{\"foo\":[5,8,-7,-8,-2,-7,2,-16,-2,8,-6,6,-5,12]}]"))
  , test "11" (\_ -> equal (Ok (Sum09D {foo = [9,-20,8,5,6,10,-18,-13,-5,8,8,-16,-20,13,11,-8,16,-16]})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09D\",{\"foo\":[9,-20,8,5,6,10,-18,-13,-5,8,8,-16,-20,13,11,-8,16,-16]}]"))
  ]

sumDecode10 : Test
sumDecode10 = describe "Sum decode 10"
  [ test "1" (\_ -> equal (Ok (Sum10C [] [])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10C\",[[],[]]]"))
  , test "2" (\_ -> equal (Ok (Sum10C [] [0])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10C\",[[],[0]]]"))
  , test "3" (\_ -> equal (Ok (Sum10D {foo = [-1,2]})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10D\",{\"foo\":[-1,2]}]"))
  , test "4" (\_ -> equal (Ok (Sum10D {foo = []})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10D\",{\"foo\":[]}]"))
  , test "5" (\_ -> equal (Ok (Sum10C [3,-4,-4,6,-4,-7,1] [-1,-6,-1,3,7,-5,2,-2])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10C\",[[3,-4,-4,6,-4,-7,1],[-1,-6,-1,3,7,-5,2,-2]]]"))
  , test "6" (\_ -> equal (Ok (Sum10B (Just [-9]))) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10B\",[-9]]"))
  , test "7" (\_ -> equal (Ok (Sum10E {bar = 10, baz = 5})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10E\",{\"bar\":10,\"baz\":5}]"))
  , test "8" (\_ -> equal (Ok (Sum10A [8,7,7,-10,-1,3,8,-13,-14,12,2,7,-10,-10])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10A\",[8,7,7,-10,-1,3,8,-13,-14,12,2,7,-10,-10]]"))
  , test "9" (\_ -> equal (Ok (Sum10C [4,-4,9,8,1,-11,-13,14] [11,12,-10,-9,-2,12,-1,0,13,9,9,4,-11,6,10])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10C\",[[4,-4,9,8,1,-11,-13,14],[11,12,-10,-9,-2,12,-1,0,13,9,9,4,-11,6,10]]]"))
  , test "10" (\_ -> equal (Ok (Sum10A [6,-5,-1,7,-12,-12,-3,-10,-12,5,-3,17,13,-15])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10A\",[6,-5,-1,7,-12,-12,-3,-10,-12,5,-3,17,13,-15]]"))
  , test "11" (\_ -> equal (Ok (Sum10B (Just [16,-4,9,16,-17,-7,10,10,6,-2,-13,7,-4,12,-4]))) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10B\",[16,-4,9,16,-17,-7,10,10,6,-2,-13,7,-4,12,-4]]"))
  ]

sumDecode11 : Test
sumDecode11 = describe "Sum decode 11"
  [ test "1" (\_ -> equal (Ok (Sum11B (Just []))) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11B\",[]]"))
  , test "2" (\_ -> equal (Ok (Sum11E {bar = 2, baz = -2})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11E\",{\"bar\":2,\"baz\":-2}]"))
  , test "3" (\_ -> equal (Ok (Sum11A [2,1])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11A\",[2,1]]"))
  , test "4" (\_ -> equal (Ok (Sum11C [-4] [-1,3])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11C\",[[-4],[-1,3]]]"))
  , test "5" (\_ -> equal (Ok (Sum11A [-1,5,-3,-8,6,-3,0,1])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11A\",[-1,5,-3,-8,6,-3,0,1]]"))
  , test "6" (\_ -> equal (Ok (Sum11D {foo = [7]})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11D\",{\"foo\":[7]}]"))
  , test "7" (\_ -> equal (Ok (Sum11D {foo = [8,-7,-7,-4,-10,-5,6,-9,7]})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11D\",{\"foo\":[8,-7,-7,-4,-10,-5,6,-9,7]}]"))
  , test "8" (\_ -> equal (Ok (Sum11A [1,-9,-4,-6,-5,-6])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11A\",[1,-9,-4,-6,-5,-6]]"))
  , test "9" (\_ -> equal (Ok (Sum11D {foo = []})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11D\",{\"foo\":[]}]"))
  , test "10" (\_ -> equal (Ok (Sum11E {bar = 11, baz = -10})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11E\",{\"bar\":11,\"baz\":-10}]"))
  , test "11" (\_ -> equal (Ok (Sum11B (Just [19,-1,20,-16,19,-4,-6,0,20,6,7,-12,-14,-20,10,-4,14]))) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11B\",[19,-1,20,-16,19,-4,-6,0,20,6,7,-12,-14,-20,10,-4,14]]"))
  ]

sumDecode12 : Test
sumDecode12 = describe "Sum decode 12"
  [ test "1" (\_ -> equal (Ok (Sum12B (Just []))) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12B\",[]]"))
  , test "2" (\_ -> equal (Ok (Sum12C [2] [0])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12C\",[[2],[0]]]"))
  , test "3" (\_ -> equal (Ok (Sum12E {bar = 3, baz = -2})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12E\",{\"bar\":3,\"baz\":-2}]"))
  , test "4" (\_ -> equal (Ok (Sum12D {foo = [1,-3,5,3]})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12D\",{\"foo\":[1,-3,5,3]}]"))
  , test "5" (\_ -> equal (Ok (Sum12C [] [4,1,-7,-8])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12C\",[[],[4,1,-7,-8]]]"))
  , test "6" (\_ -> equal (Ok (Sum12C [5,9,-5,3,-6,1,-7,-3,-1] [7,-10,-6,1,-5,-9,-9,8,-5])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12C\",[[5,9,-5,3,-6,1,-7,-3,-1],[7,-10,-6,1,-5,-9,-9,8,-5]]]"))
  , test "7" (\_ -> equal (Ok (Sum12C [-11,8,3,3,-3,0,-1] [7,-7,1,5,-11,-3])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12C\",[[-11,8,3,3,-3,0,-1],[7,-7,1,5,-11,-3]]]"))
  , test "8" (\_ -> equal (Ok (Sum12C [9,4,-1,-8,-5] [])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12C\",[[9,4,-1,-8,-5],[]]]"))
  , test "9" (\_ -> equal (Ok (Sum12E {bar = 0, baz = 4})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12E\",{\"bar\":0,\"baz\":4}]"))
  , test "10" (\_ -> equal (Ok (Sum12A [-10,3,-18,14,4,-6,10,-7,-9])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12A\",[-10,3,-18,14,4,-6,10,-7,-9]]"))
  , test "11" (\_ -> equal (Ok (Sum12E {bar = 20, baz = 5})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12E\",{\"bar\":20,\"baz\":5}]"))
  ]

recordDecode1 : Test
recordDecode1 = describe "Record decode 1"
  [ test "1" (\_ -> equal (Ok (Record1 {foo = 0, bar = Just 0, baz = [], qux = Just []})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":0,\"bar\":0,\"baz\":[],\"qux\":[]}"))
  , test "2" (\_ -> equal (Ok (Record1 {foo = -1, bar = Just (-1), baz = [], qux = Just [1]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-1,\"bar\":-1,\"baz\":[],\"qux\":[1]}"))
  , test "3" (\_ -> equal (Ok (Record1 {foo = -3, bar = Just 4, baz = [1,2], qux = Just [-4,3,4,2]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-3,\"bar\":4,\"baz\":[1,2],\"qux\":[-4,3,4,2]}"))
  , test "4" (\_ -> equal (Ok (Record1 {foo = 6, bar = Just 6, baz = [2,-3,1], qux = Just [1,-3]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":6,\"bar\":6,\"baz\":[2,-3,1],\"qux\":[1,-3]}"))
  , test "5" (\_ -> equal (Ok (Record1 {foo = -1, bar = Just (-7), baz = [-7,-5], qux = Just [-7,-5,-2,0,-7,-5,2,1]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-1,\"bar\":-7,\"baz\":[-7,-5],\"qux\":[-7,-5,-2,0,-7,-5,2,1]}"))
  , test "6" (\_ -> equal (Ok (Record1 {foo = -1, bar = Just 8, baz = [-3,-9,2,-1,-6,-6,7,-3,1,-5], qux = Just [-4,5]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-1,\"bar\":8,\"baz\":[-3,-9,2,-1,-6,-6,7,-3,1,-5],\"qux\":[-4,5]}"))
  , test "7" (\_ -> equal (Ok (Record1 {foo = 1, bar = Just (-2), baz = [-6,-6,12,12,6,0,1], qux = Just [12,12,-3,-5,1]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":1,\"bar\":-2,\"baz\":[-6,-6,12,12,6,0,1],\"qux\":[12,12,-3,-5,1]}"))
  , test "8" (\_ -> equal (Ok (Record1 {foo = 10, bar = Just (-4), baz = [-12,-12,-4], qux = Just [-13,-7,12,1,14,13,-5,6,0,5,-12,4]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":10,\"bar\":-4,\"baz\":[-12,-12,-4],\"qux\":[-13,-7,12,1,14,13,-5,6,0,5,-12,4]}"))
  , test "9" (\_ -> equal (Ok (Record1 {foo = -3, bar = Just 0, baz = [-1,-4,-16,11,13], qux = Just [12,2,9,-16,7,-7,13,1]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-3,\"bar\":0,\"baz\":[-1,-4,-16,11,13],\"qux\":[12,2,9,-16,7,-7,13,1]}"))
  , test "10" (\_ -> equal (Ok (Record1 {foo = -16, bar = Just 0, baz = [-15,-16,14,3,3,-5,16,6], qux = Just [7,-14,-4,-18,-11,-15,6,-13,-11,-3,17]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-16,\"bar\":0,\"baz\":[-15,-16,14,3,3,-5,16,6],\"qux\":[7,-14,-4,-18,-11,-15,6,-13,-11,-3,17]}"))
  , test "11" (\_ -> equal (Ok (Record1 {foo = 9, bar = Just 19, baz = [13,5,9,18,17,5,-4,20], qux = Just [2,19,11,-18,-18,-8,-6,10,19,-7,-13]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":9,\"bar\":19,\"baz\":[13,5,9,18,17,5,-4,20],\"qux\":[2,19,11,-18,-18,-8,-6,10,19,-7,-13]}"))
  ]

recordDecode2 : Test
recordDecode2 = describe "Record decode 2"
  [ test "1" (\_ -> equal (Ok (Record2 {foo = 0, bar = Just 0, baz = [], qux = Just []})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":0,\"qux\":[],\"foo\":0,\"baz\":[]}"))
  , test "2" (\_ -> equal (Ok (Record2 {foo = -2, bar = Just 2, baz = [0], qux = Just [0]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":2,\"qux\":[0],\"foo\":-2,\"baz\":[0]}"))
  , test "3" (\_ -> equal (Ok (Record2 {foo = -3, bar = Just (-2), baz = [-3,-1,0], qux = Just []})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-2,\"qux\":[],\"foo\":-3,\"baz\":[-3,-1,0]}"))
  , test "4" (\_ -> equal (Ok (Record2 {foo = 3, bar = Just (-1), baz = [-5,1,0,-1,0], qux = Just [-2,4,-6,1]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-1,\"qux\":[-2,4,-6,1],\"foo\":3,\"baz\":[-5,1,0,-1,0]}"))
  , test "5" (\_ -> equal (Ok (Record2 {foo = 2, bar = Just (-3), baz = [0,-2,1,-3], qux = Just [-7,0,-6,-4]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-3,\"qux\":[-7,0,-6,-4],\"foo\":2,\"baz\":[0,-2,1,-3]}"))
  , test "6" (\_ -> equal (Ok (Record2 {foo = 3, bar = Just (-10), baz = [9,-5,8,2,-9,6], qux = Just [-4,-10,5]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-10,\"qux\":[-4,-10,5],\"foo\":3,\"baz\":[9,-5,8,2,-9,6]}"))
  , test "7" (\_ -> equal (Ok (Record2 {foo = 12, bar = Just (-6), baz = [8,8,-10,-9,7,9,2], qux = Just [12,-11,-6,7,11,3,3]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-6,\"qux\":[12,-11,-6,7,11,3,3],\"foo\":12,\"baz\":[8,8,-10,-9,7,9,2]}"))
  , test "8" (\_ -> equal (Ok (Record2 {foo = 0, bar = Just 13, baz = [-13,-8,-10,3,-12,-3,11,14], qux = Just [-11,10,13,-5]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":13,\"qux\":[-11,10,13,-5],\"foo\":0,\"baz\":[-13,-8,-10,3,-12,-3,11,14]}"))
  , test "9" (\_ -> equal (Ok (Record2 {foo = -10, bar = Just (-5), baz = [7,-9,-9,-11,7,8,1,3,-7,15,2,14], qux = Just [-14,-8,15,-12,2]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-5,\"qux\":[-14,-8,15,-12,2],\"foo\":-10,\"baz\":[7,-9,-9,-11,7,8,1,3,-7,15,2,14]}"))
  , test "10" (\_ -> equal (Ok (Record2 {foo = 9, bar = Just 0, baz = [18], qux = Just []})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":0,\"qux\":[],\"foo\":9,\"baz\":[18]}"))
  , test "11" (\_ -> equal (Ok (Record2 {foo = -19, bar = Just (-5), baz = [-10,4,-9,-17,20,15], qux = Just [10,-14,15,20,12,-15,14,-9,20]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-5,\"qux\":[10,-14,15,20,12,-15,14,-9,20],\"foo\":-19,\"baz\":[-10,4,-9,-17,20,15]}"))
  ]

recordEncode1 : Test
recordEncode1 = describe "Record encode 1"
  [ test "1" (\_ -> equalHack "{\"foo\":0,\"bar\":0,\"baz\":[],\"qux\":[]}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list << List.map Json.Encode.int) (Record1 {foo = 0, bar = Just 0, baz = [], qux = Just []}))))
  , test "2" (\_ -> equalHack "{\"foo\":-1,\"bar\":-1,\"baz\":[],\"qux\":[1]}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list << List.map Json.Encode.int) (Record1 {foo = -1, bar = Just (-1), baz = [], qux = Just [1]}))))
  , test "3" (\_ -> equalHack "{\"foo\":-3,\"bar\":4,\"baz\":[1,2],\"qux\":[-4,3,4,2]}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list << List.map Json.Encode.int) (Record1 {foo = -3, bar = Just 4, baz = [1,2], qux = Just [-4,3,4,2]}))))
  , test "4" (\_ -> equalHack "{\"foo\":6,\"bar\":6,\"baz\":[2,-3,1],\"qux\":[1,-3]}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list << List.map Json.Encode.int) (Record1 {foo = 6, bar = Just 6, baz = [2,-3,1], qux = Just [1,-3]}))))
  , test "5" (\_ -> equalHack "{\"foo\":-1,\"bar\":-7,\"baz\":[-7,-5],\"qux\":[-7,-5,-2,0,-7,-5,2,1]}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list << List.map Json.Encode.int) (Record1 {foo = -1, bar = Just (-7), baz = [-7,-5], qux = Just [-7,-5,-2,0,-7,-5,2,1]}))))
  , test "6" (\_ -> equalHack "{\"foo\":-1,\"bar\":8,\"baz\":[-3,-9,2,-1,-6,-6,7,-3,1,-5],\"qux\":[-4,5]}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list << List.map Json.Encode.int) (Record1 {foo = -1, bar = Just 8, baz = [-3,-9,2,-1,-6,-6,7,-3,1,-5], qux = Just [-4,5]}))))
  , test "7" (\_ -> equalHack "{\"foo\":1,\"bar\":-2,\"baz\":[-6,-6,12,12,6,0,1],\"qux\":[12,12,-3,-5,1]}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list << List.map Json.Encode.int) (Record1 {foo = 1, bar = Just (-2), baz = [-6,-6,12,12,6,0,1], qux = Just [12,12,-3,-5,1]}))))
  , test "8" (\_ -> equalHack "{\"foo\":10,\"bar\":-4,\"baz\":[-12,-12,-4],\"qux\":[-13,-7,12,1,14,13,-5,6,0,5,-12,4]}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list << List.map Json.Encode.int) (Record1 {foo = 10, bar = Just (-4), baz = [-12,-12,-4], qux = Just [-13,-7,12,1,14,13,-5,6,0,5,-12,4]}))))
  , test "9" (\_ -> equalHack "{\"foo\":-3,\"bar\":0,\"baz\":[-1,-4,-16,11,13],\"qux\":[12,2,9,-16,7,-7,13,1]}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list << List.map Json.Encode.int) (Record1 {foo = -3, bar = Just 0, baz = [-1,-4,-16,11,13], qux = Just [12,2,9,-16,7,-7,13,1]}))))
  , test "10" (\_ -> equalHack "{\"foo\":-16,\"bar\":0,\"baz\":[-15,-16,14,3,3,-5,16,6],\"qux\":[7,-14,-4,-18,-11,-15,6,-13,-11,-3,17]}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list << List.map Json.Encode.int) (Record1 {foo = -16, bar = Just 0, baz = [-15,-16,14,3,3,-5,16,6], qux = Just [7,-14,-4,-18,-11,-15,6,-13,-11,-3,17]}))))
  , test "11" (\_ -> equalHack "{\"foo\":9,\"bar\":19,\"baz\":[13,5,9,18,17,5,-4,20],\"qux\":[2,19,11,-18,-18,-8,-6,10,19,-7,-13]}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list << List.map Json.Encode.int) (Record1 {foo = 9, bar = Just 19, baz = [13,5,9,18,17,5,-4,20], qux = Just [2,19,11,-18,-18,-8,-6,10,19,-7,-13]}))))
  ]

recordEncode2 : Test
recordEncode2 = describe "Record encode 2"
  [ test "1" (\_ -> equalHack "{\"bar\":0,\"qux\":[],\"foo\":0,\"baz\":[]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list << List.map Json.Encode.int) (Record2 {foo = 0, bar = Just 0, baz = [], qux = Just []}))))
  , test "2" (\_ -> equalHack "{\"bar\":2,\"qux\":[0],\"foo\":-2,\"baz\":[0]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list << List.map Json.Encode.int) (Record2 {foo = -2, bar = Just 2, baz = [0], qux = Just [0]}))))
  , test "3" (\_ -> equalHack "{\"bar\":-2,\"qux\":[],\"foo\":-3,\"baz\":[-3,-1,0]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list << List.map Json.Encode.int) (Record2 {foo = -3, bar = Just (-2), baz = [-3,-1,0], qux = Just []}))))
  , test "4" (\_ -> equalHack "{\"bar\":-1,\"qux\":[-2,4,-6,1],\"foo\":3,\"baz\":[-5,1,0,-1,0]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list << List.map Json.Encode.int) (Record2 {foo = 3, bar = Just (-1), baz = [-5,1,0,-1,0], qux = Just [-2,4,-6,1]}))))
  , test "5" (\_ -> equalHack "{\"bar\":-3,\"qux\":[-7,0,-6,-4],\"foo\":2,\"baz\":[0,-2,1,-3]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list << List.map Json.Encode.int) (Record2 {foo = 2, bar = Just (-3), baz = [0,-2,1,-3], qux = Just [-7,0,-6,-4]}))))
  , test "6" (\_ -> equalHack "{\"bar\":-10,\"qux\":[-4,-10,5],\"foo\":3,\"baz\":[9,-5,8,2,-9,6]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list << List.map Json.Encode.int) (Record2 {foo = 3, bar = Just (-10), baz = [9,-5,8,2,-9,6], qux = Just [-4,-10,5]}))))
  , test "7" (\_ -> equalHack "{\"bar\":-6,\"qux\":[12,-11,-6,7,11,3,3],\"foo\":12,\"baz\":[8,8,-10,-9,7,9,2]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list << List.map Json.Encode.int) (Record2 {foo = 12, bar = Just (-6), baz = [8,8,-10,-9,7,9,2], qux = Just [12,-11,-6,7,11,3,3]}))))
  , test "8" (\_ -> equalHack "{\"bar\":13,\"qux\":[-11,10,13,-5],\"foo\":0,\"baz\":[-13,-8,-10,3,-12,-3,11,14]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list << List.map Json.Encode.int) (Record2 {foo = 0, bar = Just 13, baz = [-13,-8,-10,3,-12,-3,11,14], qux = Just [-11,10,13,-5]}))))
  , test "9" (\_ -> equalHack "{\"bar\":-5,\"qux\":[-14,-8,15,-12,2],\"foo\":-10,\"baz\":[7,-9,-9,-11,7,8,1,3,-7,15,2,14]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list << List.map Json.Encode.int) (Record2 {foo = -10, bar = Just (-5), baz = [7,-9,-9,-11,7,8,1,3,-7,15,2,14], qux = Just [-14,-8,15,-12,2]}))))
  , test "10" (\_ -> equalHack "{\"bar\":0,\"qux\":[],\"foo\":9,\"baz\":[18]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list << List.map Json.Encode.int) (Record2 {foo = 9, bar = Just 0, baz = [18], qux = Just []}))))
  , test "11" (\_ -> equalHack "{\"bar\":-5,\"qux\":[10,-14,15,20,12,-15,14,-9,20],\"foo\":-19,\"baz\":[-10,4,-9,-17,20,15]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list << List.map Json.Encode.int) (Record2 {foo = -19, bar = Just (-5), baz = [-10,4,-9,-17,20,15], qux = Just [10,-14,15,20,12,-15,14,-9,20]}))))
  ]

simpleEncode01 : Test
simpleEncode01 = describe "Simple encode 01"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list << List.map Json.Encode.int) (Simple01 []))))
  , test "2" (\_ -> equalHack "[1]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list << List.map Json.Encode.int) (Simple01 [1]))))
  , test "3" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list << List.map Json.Encode.int) (Simple01 []))))
  , test "4" (\_ -> equalHack "[6,-1,1,-5]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list << List.map Json.Encode.int) (Simple01 [6,-1,1,-5]))))
  , test "5" (\_ -> equalHack "[-2,-8,6,-5,-2,1,3,0]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list << List.map Json.Encode.int) (Simple01 [-2,-8,6,-5,-2,1,3,0]))))
  , test "6" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list << List.map Json.Encode.int) (Simple01 []))))
  , test "7" (\_ -> equalHack "[6]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list << List.map Json.Encode.int) (Simple01 [6]))))
  , test "8" (\_ -> equalHack "[2,-1,-2,-10,11,12,-4,-14,3]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list << List.map Json.Encode.int) (Simple01 [2,-1,-2,-10,11,12,-4,-14,3]))))
  , test "9" (\_ -> equalHack "[-10,1,11,9,7,-8,-13,-11,8,-12,-8,-10,-4]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list << List.map Json.Encode.int) (Simple01 [-10,1,11,9,7,-8,-13,-11,8,-12,-8,-10,-4]))))
  , test "10" (\_ -> equalHack "[-11,-16,6,7]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list << List.map Json.Encode.int) (Simple01 [-11,-16,6,7]))))
  , test "11" (\_ -> equalHack "[-17,-1,6,-20,-8,-2,18,-8,-1,-7,11,18]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list << List.map Json.Encode.int) (Simple01 [-17,-1,6,-20,-8,-2,18,-8,-1,-7,11,18]))))
  ]

simpleEncode02 : Test
simpleEncode02 = describe "Simple encode 02"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list << List.map Json.Encode.int) (Simple02 []))))
  , test "2" (\_ -> equalHack "[-1,0]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list << List.map Json.Encode.int) (Simple02 [-1,0]))))
  , test "3" (\_ -> equalHack "[-2,0]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list << List.map Json.Encode.int) (Simple02 [-2,0]))))
  , test "4" (\_ -> equalHack "[-3,5,2]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list << List.map Json.Encode.int) (Simple02 [-3,5,2]))))
  , test "5" (\_ -> equalHack "[4,6,7,8,8,-6]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list << List.map Json.Encode.int) (Simple02 [4,6,7,8,8,-6]))))
  , test "6" (\_ -> equalHack "[-6,5,5,-4]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list << List.map Json.Encode.int) (Simple02 [-6,5,5,-4]))))
  , test "7" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list << List.map Json.Encode.int) (Simple02 []))))
  , test "8" (\_ -> equalHack "[3,-4,1,7,8,1,8]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list << List.map Json.Encode.int) (Simple02 [3,-4,1,7,8,1,8]))))
  , test "9" (\_ -> equalHack "[8,5]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list << List.map Json.Encode.int) (Simple02 [8,5]))))
  , test "10" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list << List.map Json.Encode.int) (Simple02 []))))
  , test "11" (\_ -> equalHack "[-7,10,12,14,13,-15,-9,17,18,-5,6]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list << List.map Json.Encode.int) (Simple02 [-7,10,12,14,13,-15,-9,17,18,-5,6]))))
  ]

simpleEncode03 : Test
simpleEncode03 = describe "Simple encode 03"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list << List.map Json.Encode.int) (Simple03 []))))
  , test "2" (\_ -> equalHack "[-1]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list << List.map Json.Encode.int) (Simple03 [-1]))))
  , test "3" (\_ -> equalHack "[2,-4]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list << List.map Json.Encode.int) (Simple03 [2,-4]))))
  , test "4" (\_ -> equalHack "[2,2]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list << List.map Json.Encode.int) (Simple03 [2,2]))))
  , test "5" (\_ -> equalHack "[-4]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list << List.map Json.Encode.int) (Simple03 [-4]))))
  , test "6" (\_ -> equalHack "[9,10]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list << List.map Json.Encode.int) (Simple03 [9,10]))))
  , test "7" (\_ -> equalHack "[-5,-5,6,8,-11,-10,-11]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list << List.map Json.Encode.int) (Simple03 [-5,-5,6,8,-11,-10,-11]))))
  , test "8" (\_ -> equalHack "[-8,-3,-6,-10,-6,11,5,13,2]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list << List.map Json.Encode.int) (Simple03 [-8,-3,-6,-10,-6,11,5,13,2]))))
  , test "9" (\_ -> equalHack "[15,14,-12,-14,-13,1,-9,2,4,6,11,14]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list << List.map Json.Encode.int) (Simple03 [15,14,-12,-14,-13,1,-9,2,4,6,11,14]))))
  , test "10" (\_ -> equalHack "[2]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list << List.map Json.Encode.int) (Simple03 [2]))))
  , test "11" (\_ -> equalHack "[6]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list << List.map Json.Encode.int) (Simple03 [6]))))
  ]

simpleEncode04 : Test
simpleEncode04 = describe "Simple encode 04"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list << List.map Json.Encode.int) (Simple04 []))))
  , test "2" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list << List.map Json.Encode.int) (Simple04 []))))
  , test "3" (\_ -> equalHack "[0,-3]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list << List.map Json.Encode.int) (Simple04 [0,-3]))))
  , test "4" (\_ -> equalHack "[-5,-1,-2]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list << List.map Json.Encode.int) (Simple04 [-5,-1,-2]))))
  , test "5" (\_ -> equalHack "[3,-8,1,-8]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list << List.map Json.Encode.int) (Simple04 [3,-8,1,-8]))))
  , test "6" (\_ -> equalHack "[-5,8]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list << List.map Json.Encode.int) (Simple04 [-5,8]))))
  , test "7" (\_ -> equalHack "[-10,-4,7,0]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list << List.map Json.Encode.int) (Simple04 [-10,-4,7,0]))))
  , test "8" (\_ -> equalHack "[-4,10,3,3,-10,9,5,-8,-7,-4,-6]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list << List.map Json.Encode.int) (Simple04 [-4,10,3,3,-10,9,5,-8,-7,-4,-6]))))
  , test "9" (\_ -> equalHack "[13,7,-7,-1,-16,-7,7,2,-3,-4,-12]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list << List.map Json.Encode.int) (Simple04 [13,7,-7,-1,-16,-7,7,2,-3,-4,-12]))))
  , test "10" (\_ -> equalHack "[-12,13,-18]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list << List.map Json.Encode.int) (Simple04 [-12,13,-18]))))
  , test "11" (\_ -> equalHack "[-19,-4]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list << List.map Json.Encode.int) (Simple04 [-19,-4]))))
  ]

simpleDecode01 : Test
simpleDecode01 = describe "Simple decode 01"
  [ test "1" (\_ -> equal (Ok (Simple01 [])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple01 [1])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[1]"))
  , test "3" (\_ -> equal (Ok (Simple01 [])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "4" (\_ -> equal (Ok (Simple01 [6,-1,1,-5])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[6,-1,1,-5]"))
  , test "5" (\_ -> equal (Ok (Simple01 [-2,-8,6,-5,-2,1,3,0])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-2,-8,6,-5,-2,1,3,0]"))
  , test "6" (\_ -> equal (Ok (Simple01 [])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "7" (\_ -> equal (Ok (Simple01 [6])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[6]"))
  , test "8" (\_ -> equal (Ok (Simple01 [2,-1,-2,-10,11,12,-4,-14,3])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[2,-1,-2,-10,11,12,-4,-14,3]"))
  , test "9" (\_ -> equal (Ok (Simple01 [-10,1,11,9,7,-8,-13,-11,8,-12,-8,-10,-4])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-10,1,11,9,7,-8,-13,-11,8,-12,-8,-10,-4]"))
  , test "10" (\_ -> equal (Ok (Simple01 [-11,-16,6,7])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-11,-16,6,7]"))
  , test "11" (\_ -> equal (Ok (Simple01 [-17,-1,6,-20,-8,-2,18,-8,-1,-7,11,18])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-17,-1,6,-20,-8,-2,18,-8,-1,-7,11,18]"))
  ]

simpleDecode02 : Test
simpleDecode02 = describe "Simple decode 02"
  [ test "1" (\_ -> equal (Ok (Simple02 [])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple02 [-1,0])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-1,0]"))
  , test "3" (\_ -> equal (Ok (Simple02 [-2,0])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-2,0]"))
  , test "4" (\_ -> equal (Ok (Simple02 [-3,5,2])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-3,5,2]"))
  , test "5" (\_ -> equal (Ok (Simple02 [4,6,7,8,8,-6])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[4,6,7,8,8,-6]"))
  , test "6" (\_ -> equal (Ok (Simple02 [-6,5,5,-4])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-6,5,5,-4]"))
  , test "7" (\_ -> equal (Ok (Simple02 [])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "8" (\_ -> equal (Ok (Simple02 [3,-4,1,7,8,1,8])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[3,-4,1,7,8,1,8]"))
  , test "9" (\_ -> equal (Ok (Simple02 [8,5])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[8,5]"))
  , test "10" (\_ -> equal (Ok (Simple02 [])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "11" (\_ -> equal (Ok (Simple02 [-7,10,12,14,13,-15,-9,17,18,-5,6])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-7,10,12,14,13,-15,-9,17,18,-5,6]"))
  ]

simpleDecode03 : Test
simpleDecode03 = describe "Simple decode 03"
  [ test "1" (\_ -> equal (Ok (Simple03 [])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple03 [-1])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-1]"))
  , test "3" (\_ -> equal (Ok (Simple03 [2,-4])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[2,-4]"))
  , test "4" (\_ -> equal (Ok (Simple03 [2,2])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[2,2]"))
  , test "5" (\_ -> equal (Ok (Simple03 [-4])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-4]"))
  , test "6" (\_ -> equal (Ok (Simple03 [9,10])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[9,10]"))
  , test "7" (\_ -> equal (Ok (Simple03 [-5,-5,6,8,-11,-10,-11])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-5,-5,6,8,-11,-10,-11]"))
  , test "8" (\_ -> equal (Ok (Simple03 [-8,-3,-6,-10,-6,11,5,13,2])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-8,-3,-6,-10,-6,11,5,13,2]"))
  , test "9" (\_ -> equal (Ok (Simple03 [15,14,-12,-14,-13,1,-9,2,4,6,11,14])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[15,14,-12,-14,-13,1,-9,2,4,6,11,14]"))
  , test "10" (\_ -> equal (Ok (Simple03 [2])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[2]"))
  , test "11" (\_ -> equal (Ok (Simple03 [6])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[6]"))
  ]

simpleDecode04 : Test
simpleDecode04 = describe "Simple decode 04"
  [ test "1" (\_ -> equal (Ok (Simple04 [])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple04 [])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "3" (\_ -> equal (Ok (Simple04 [0,-3])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[0,-3]"))
  , test "4" (\_ -> equal (Ok (Simple04 [-5,-1,-2])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-5,-1,-2]"))
  , test "5" (\_ -> equal (Ok (Simple04 [3,-8,1,-8])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[3,-8,1,-8]"))
  , test "6" (\_ -> equal (Ok (Simple04 [-5,8])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-5,8]"))
  , test "7" (\_ -> equal (Ok (Simple04 [-10,-4,7,0])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-10,-4,7,0]"))
  , test "8" (\_ -> equal (Ok (Simple04 [-4,10,3,3,-10,9,5,-8,-7,-4,-6])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-4,10,3,3,-10,9,5,-8,-7,-4,-6]"))
  , test "9" (\_ -> equal (Ok (Simple04 [13,7,-7,-1,-16,-7,7,2,-3,-4,-12])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[13,7,-7,-1,-16,-7,7,2,-3,-4,-12]"))
  , test "10" (\_ -> equal (Ok (Simple04 [-12,13,-18])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-12,13,-18]"))
  , test "11" (\_ -> equal (Ok (Simple04 [-19,-4])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-19,-4]"))
  ]

simplerecordEncode01 : Test
simplerecordEncode01 = describe "SimpleRecord encode 01"
  [ test "1" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord01 {qux = []}))))
  , test "2" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord01 {qux = []}))))
  , test "3" (\_ -> equalHack "{\"qux\":[0,-4]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord01 {qux = [0,-4]}))))
  , test "4" (\_ -> equalHack "{\"qux\":[3,1,-2,-6,-3]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord01 {qux = [3,1,-2,-6,-3]}))))
  , test "5" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord01 {qux = []}))))
  , test "6" (\_ -> equalHack "{\"qux\":[2,2,3,-10,-4,5,-4,-5,2]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord01 {qux = [2,2,3,-10,-4,5,-4,-5,2]}))))
  , test "7" (\_ -> equalHack "{\"qux\":[-5,-6,11,-1,-4,-8,6,9,10]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord01 {qux = [-5,-6,11,-1,-4,-8,6,9,10]}))))
  , test "8" (\_ -> equalHack "{\"qux\":[1,1,11,5,13,-7,-1,3]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord01 {qux = [1,1,11,5,13,-7,-1,3]}))))
  , test "9" (\_ -> equalHack "{\"qux\":[13,-15,-3,-1,-12,10,-16,4,16,-15,-8,-16,4,-15,16,-9]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord01 {qux = [13,-15,-3,-1,-12,10,-16,4,16,-15,-8,-16,4,-15,16,-9]}))))
  , test "10" (\_ -> equalHack "{\"qux\":[17,11,-11,-3,3,10,-8,17,-13,17,-11,-6,-7,-18,-7,12]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord01 {qux = [17,11,-11,-3,3,10,-8,17,-13,17,-11,-6,-7,-18,-7,12]}))))
  , test "11" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord01 {qux = []}))))
  ]

simplerecordEncode02 : Test
simplerecordEncode02 = describe "SimpleRecord encode 02"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord02 {qux = []}))))
  , test "2" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord02 {qux = []}))))
  , test "3" (\_ -> equalHack "[-2,4,-4]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord02 {qux = [-2,4,-4]}))))
  , test "4" (\_ -> equalHack "[4,-4,-3,2]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord02 {qux = [4,-4,-3,2]}))))
  , test "5" (\_ -> equalHack "[-3,0,4]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord02 {qux = [-3,0,4]}))))
  , test "6" (\_ -> equalHack "[-10]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord02 {qux = [-10]}))))
  , test "7" (\_ -> equalHack "[-8,4,1,11,3,-4,-2,-12,-7,-7,5,12]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord02 {qux = [-8,4,1,11,3,-4,-2,-12,-7,-7,5,12]}))))
  , test "8" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord02 {qux = []}))))
  , test "9" (\_ -> equalHack "[16,-12,-8,-15]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord02 {qux = [16,-12,-8,-15]}))))
  , test "10" (\_ -> equalHack "[3,-6]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord02 {qux = [3,-6]}))))
  , test "11" (\_ -> equalHack "[11,0,-16,-13,6]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord02 {qux = [11,0,-16,-13,6]}))))
  ]

simplerecordEncode03 : Test
simplerecordEncode03 = describe "SimpleRecord encode 03"
  [ test "1" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord03 {qux = []}))))
  , test "2" (\_ -> equalHack "{\"qux\":[0,1]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord03 {qux = [0,1]}))))
  , test "3" (\_ -> equalHack "{\"qux\":[4,4]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord03 {qux = [4,4]}))))
  , test "4" (\_ -> equalHack "{\"qux\":[2,3,0,1,0]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord03 {qux = [2,3,0,1,0]}))))
  , test "5" (\_ -> equalHack "{\"qux\":[4,-6,7,-4,0,-5]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord03 {qux = [4,-6,7,-4,0,-5]}))))
  , test "6" (\_ -> equalHack "{\"qux\":[-2,-7]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord03 {qux = [-2,-7]}))))
  , test "7" (\_ -> equalHack "{\"qux\":[6,-4,4,-5,0]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord03 {qux = [6,-4,4,-5,0]}))))
  , test "8" (\_ -> equalHack "{\"qux\":[14,9,2]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord03 {qux = [14,9,2]}))))
  , test "9" (\_ -> equalHack "{\"qux\":[6,-5,6,-16,-1,16,11,5,-10,14,1,2,-12]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord03 {qux = [6,-5,6,-16,-1,16,11,5,-10,14,1,2,-12]}))))
  , test "10" (\_ -> equalHack "{\"qux\":[-5,-10,-9,5]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord03 {qux = [-5,-10,-9,5]}))))
  , test "11" (\_ -> equalHack "{\"qux\":[12,2,-2,3,-1]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord03 {qux = [12,2,-2,3,-1]}))))
  ]

simplerecordEncode04 : Test
simplerecordEncode04 = describe "SimpleRecord encode 04"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord04 {qux = []}))))
  , test "2" (\_ -> equalHack "[2,0]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord04 {qux = [2,0]}))))
  , test "3" (\_ -> equalHack "[2,-3,-4]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord04 {qux = [2,-3,-4]}))))
  , test "4" (\_ -> equalHack "[3,-4,-3,-2,-4]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord04 {qux = [3,-4,-3,-2,-4]}))))
  , test "5" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord04 {qux = []}))))
  , test "6" (\_ -> equalHack "[5,10,-4,10,6,0,-4]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord04 {qux = [5,10,-4,10,6,0,-4]}))))
  , test "7" (\_ -> equalHack "[-6,12,-4,3,-2,-9,0,1,-10]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord04 {qux = [-6,12,-4,3,-2,-9,0,1,-10]}))))
  , test "8" (\_ -> equalHack "[1,5,-10,2,-8,14,-13,7,0,5]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord04 {qux = [1,5,-10,2,-8,14,-13,7,0,5]}))))
  , test "9" (\_ -> equalHack "[-7,-9,-4,8,-3,-12,-10,-12,-14,1,-8]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord04 {qux = [-7,-9,-4,8,-3,-12,-10,-12,-14,1,-8]}))))
  , test "10" (\_ -> equalHack "[-11,4,4,-4,4,6,7,-12,-1,-17,16,-17,-1,18,-8,-8,6]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord04 {qux = [-11,4,4,-4,4,6,7,-12,-1,-17,16,-17,-1,18,-8,-8,6]}))))
  , test "11" (\_ -> equalHack "[-8,-4,3,10,13,9,14,9,-19,-20,-13,9,-16,-15,-17,4,-3,-11]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list << List.map Json.Encode.int) (SimpleRecord04 {qux = [-8,-4,3,10,13,9,14,9,-19,-20,-13,9,-16,-15,-17,4,-3,-11]}))))
  ]

simplerecordDecode01 : Test
simplerecordDecode01 = describe "SimpleRecord decode 01"
  [ test "1" (\_ -> equal (Ok (SimpleRecord01 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "2" (\_ -> equal (Ok (SimpleRecord01 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "3" (\_ -> equal (Ok (SimpleRecord01 {qux = [0,-4]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[0,-4]}"))
  , test "4" (\_ -> equal (Ok (SimpleRecord01 {qux = [3,1,-2,-6,-3]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[3,1,-2,-6,-3]}"))
  , test "5" (\_ -> equal (Ok (SimpleRecord01 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "6" (\_ -> equal (Ok (SimpleRecord01 {qux = [2,2,3,-10,-4,5,-4,-5,2]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[2,2,3,-10,-4,5,-4,-5,2]}"))
  , test "7" (\_ -> equal (Ok (SimpleRecord01 {qux = [-5,-6,11,-1,-4,-8,6,9,10]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-5,-6,11,-1,-4,-8,6,9,10]}"))
  , test "8" (\_ -> equal (Ok (SimpleRecord01 {qux = [1,1,11,5,13,-7,-1,3]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[1,1,11,5,13,-7,-1,3]}"))
  , test "9" (\_ -> equal (Ok (SimpleRecord01 {qux = [13,-15,-3,-1,-12,10,-16,4,16,-15,-8,-16,4,-15,16,-9]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[13,-15,-3,-1,-12,10,-16,4,16,-15,-8,-16,4,-15,16,-9]}"))
  , test "10" (\_ -> equal (Ok (SimpleRecord01 {qux = [17,11,-11,-3,3,10,-8,17,-13,17,-11,-6,-7,-18,-7,12]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[17,11,-11,-3,3,10,-8,17,-13,17,-11,-6,-7,-18,-7,12]}"))
  , test "11" (\_ -> equal (Ok (SimpleRecord01 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  ]

simplerecordDecode02 : Test
simplerecordDecode02 = describe "SimpleRecord decode 02"
  [ test "1" (\_ -> equal (Ok (SimpleRecord02 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (SimpleRecord02 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "3" (\_ -> equal (Ok (SimpleRecord02 {qux = [-2,4,-4]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-2,4,-4]"))
  , test "4" (\_ -> equal (Ok (SimpleRecord02 {qux = [4,-4,-3,2]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[4,-4,-3,2]"))
  , test "5" (\_ -> equal (Ok (SimpleRecord02 {qux = [-3,0,4]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-3,0,4]"))
  , test "6" (\_ -> equal (Ok (SimpleRecord02 {qux = [-10]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-10]"))
  , test "7" (\_ -> equal (Ok (SimpleRecord02 {qux = [-8,4,1,11,3,-4,-2,-12,-7,-7,5,12]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-8,4,1,11,3,-4,-2,-12,-7,-7,5,12]"))
  , test "8" (\_ -> equal (Ok (SimpleRecord02 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "9" (\_ -> equal (Ok (SimpleRecord02 {qux = [16,-12,-8,-15]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[16,-12,-8,-15]"))
  , test "10" (\_ -> equal (Ok (SimpleRecord02 {qux = [3,-6]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[3,-6]"))
  , test "11" (\_ -> equal (Ok (SimpleRecord02 {qux = [11,0,-16,-13,6]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[11,0,-16,-13,6]"))
  ]

simplerecordDecode03 : Test
simplerecordDecode03 = describe "SimpleRecord decode 03"
  [ test "1" (\_ -> equal (Ok (SimpleRecord03 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "2" (\_ -> equal (Ok (SimpleRecord03 {qux = [0,1]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[0,1]}"))
  , test "3" (\_ -> equal (Ok (SimpleRecord03 {qux = [4,4]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[4,4]}"))
  , test "4" (\_ -> equal (Ok (SimpleRecord03 {qux = [2,3,0,1,0]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[2,3,0,1,0]}"))
  , test "5" (\_ -> equal (Ok (SimpleRecord03 {qux = [4,-6,7,-4,0,-5]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[4,-6,7,-4,0,-5]}"))
  , test "6" (\_ -> equal (Ok (SimpleRecord03 {qux = [-2,-7]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-2,-7]}"))
  , test "7" (\_ -> equal (Ok (SimpleRecord03 {qux = [6,-4,4,-5,0]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[6,-4,4,-5,0]}"))
  , test "8" (\_ -> equal (Ok (SimpleRecord03 {qux = [14,9,2]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[14,9,2]}"))
  , test "9" (\_ -> equal (Ok (SimpleRecord03 {qux = [6,-5,6,-16,-1,16,11,5,-10,14,1,2,-12]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[6,-5,6,-16,-1,16,11,5,-10,14,1,2,-12]}"))
  , test "10" (\_ -> equal (Ok (SimpleRecord03 {qux = [-5,-10,-9,5]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-5,-10,-9,5]}"))
  , test "11" (\_ -> equal (Ok (SimpleRecord03 {qux = [12,2,-2,3,-1]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[12,2,-2,3,-1]}"))
  ]

simplerecordDecode04 : Test
simplerecordDecode04 = describe "SimpleRecord decode 04"
  [ test "1" (\_ -> equal (Ok (SimpleRecord04 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (SimpleRecord04 {qux = [2,0]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[2,0]"))
  , test "3" (\_ -> equal (Ok (SimpleRecord04 {qux = [2,-3,-4]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[2,-3,-4]"))
  , test "4" (\_ -> equal (Ok (SimpleRecord04 {qux = [3,-4,-3,-2,-4]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[3,-4,-3,-2,-4]"))
  , test "5" (\_ -> equal (Ok (SimpleRecord04 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "6" (\_ -> equal (Ok (SimpleRecord04 {qux = [5,10,-4,10,6,0,-4]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[5,10,-4,10,6,0,-4]"))
  , test "7" (\_ -> equal (Ok (SimpleRecord04 {qux = [-6,12,-4,3,-2,-9,0,1,-10]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-6,12,-4,3,-2,-9,0,1,-10]"))
  , test "8" (\_ -> equal (Ok (SimpleRecord04 {qux = [1,5,-10,2,-8,14,-13,7,0,5]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[1,5,-10,2,-8,14,-13,7,0,5]"))
  , test "9" (\_ -> equal (Ok (SimpleRecord04 {qux = [-7,-9,-4,8,-3,-12,-10,-12,-14,1,-8]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-7,-9,-4,8,-3,-12,-10,-12,-14,1,-8]"))
  , test "10" (\_ -> equal (Ok (SimpleRecord04 {qux = [-11,4,4,-4,4,6,7,-12,-1,-17,16,-17,-1,18,-8,-8,6]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-11,4,4,-4,4,6,7,-12,-1,-17,16,-17,-1,18,-8,-8,6]"))
  , test "11" (\_ -> equal (Ok (SimpleRecord04 {qux = [-8,-4,3,10,13,9,14,9,-19,-20,-13,9,-16,-15,-17,4,-3,-11]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-8,-4,3,10,13,9,14,9,-19,-20,-13,9,-16,-15,-17,4,-3,-11]"))
  ]

sumEncodeUntagged : Test
sumEncodeUntagged = describe "Sum encode Untagged"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list << List.map Json.Encode.int) (SMList []))))
  , test "2" (\_ -> equalHack "-2"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list << List.map Json.Encode.int) (SMInt (-2)))))
  , test "3" (\_ -> equalHack "[1,0,-2]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list << List.map Json.Encode.int) (SMList [1,0,-2]))))
  , test "4" (\_ -> equalHack "[-3]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list << List.map Json.Encode.int) (SMList [-3]))))
  , test "5" (\_ -> equalHack "-4"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list << List.map Json.Encode.int) (SMInt (-4)))))
  , test "6" (\_ -> equalHack "[-9,1,10,-1,6,0,4,3]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list << List.map Json.Encode.int) (SMList [-9,1,10,-1,6,0,4,3]))))
  , test "7" (\_ -> equalHack "[-3,7,5,-11,10,-3,0,-1,9,-2]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list << List.map Json.Encode.int) (SMList [-3,7,5,-11,10,-3,0,-1,9,-2]))))
  , test "8" (\_ -> equalHack "[12,7]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list << List.map Json.Encode.int) (SMList [12,7]))))
  , test "9" (\_ -> equalHack "5"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list << List.map Json.Encode.int) (SMInt 5))))
  , test "10" (\_ -> equalHack "14"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list << List.map Json.Encode.int) (SMInt 14))))
  , test "11" (\_ -> equalHack "[19]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list << List.map Json.Encode.int) (SMList [19]))))
  ]

sumDecodeUntagged : Test
sumDecodeUntagged = describe "Sum decode Untagged"
  [ test "1" (\_ -> equal (Ok (SMList [])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (SMInt (-2))) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "-2"))
  , test "3" (\_ -> equal (Ok (SMList [1,0,-2])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[1,0,-2]"))
  , test "4" (\_ -> equal (Ok (SMList [-3])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[-3]"))
  , test "5" (\_ -> equal (Ok (SMInt (-4))) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "-4"))
  , test "6" (\_ -> equal (Ok (SMList [-9,1,10,-1,6,0,4,3])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[-9,1,10,-1,6,0,4,3]"))
  , test "7" (\_ -> equal (Ok (SMList [-3,7,5,-11,10,-3,0,-1,9,-2])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[-3,7,5,-11,10,-3,0,-1,9,-2]"))
  , test "8" (\_ -> equal (Ok (SMList [12,7])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[12,7]"))
  , test "9" (\_ -> equal (Ok (SMInt 5)) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "5"))
  , test "10" (\_ -> equal (Ok (SMInt 14)) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "14"))
  , test "11" (\_ -> equal (Ok (SMList [19])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[19]"))
  ]

