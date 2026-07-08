-- Api.elm
module Api exposing (apiUrl, getScore, postScore)

import Http
import Json.Decode as Decode
import Json.Encode as Encode

apiUrl : String
apiUrl =
    "http://127.0.0.1:3030/score/spieler1"

getScore : (Result Http.Error Int -> msg) -> Cmd msg
getScore toMsg =
    Http.get
        { url = apiUrl
        , expect = Http.expectJson toMsg (Decode.field "score" Decode.int)
        }

postScore : Int -> (Result Http.Error Int -> msg) -> Cmd msg
postScore neuerScore toMsg =
    Http.post
        { url = apiUrl
        , body = Http.jsonBody (Encode.object [ ( "score", Encode.int neuerScore ) ])
        , expect = Http.expectJson toMsg (Decode.field "score" Decode.int)
        }