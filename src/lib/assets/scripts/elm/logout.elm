module Logout exposing (..)

import Html exposing (..)
import Navigation exposing (load)
import Http exposing (Error, Response, request)
import Html.Events exposing (onWithOptions, defaultOptions)
import Json.Decode exposing (succeed)
import Html.Attributes exposing (href)

type alias Model = ()

type Msg = Logout String | AfterLogout (Result Http.Error String)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init =
  ((), Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Logout url -> ((), deleteSession url)
    AfterLogout (Ok url) -> ((), Navigation.load url)
    AfterLogout (Err _) -> ((), Navigation.load "/")

deleteSession : String -> Cmd Msg
deleteSession url =
  Http.send AfterLogout <|
    Http.request
      { method = "DELETE"
      , headers = []
      , url = url
      , body = Http.emptyBody
      , expect = Http.expectString
      , timeout = Nothing
      , withCredentials = False
      }

view : Model -> Html Msg
view model =
  a [ onWithOptions "click"
        { defaultOptions | preventDefault = True } <|
        Json.Decode.succeed (Logout deleteSessionUrl)
    , href deleteSessionUrl
    ]
    [ text "Sign out" ]

deleteSessionUrl : String
deleteSessionUrl = "/session/delete" 

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
