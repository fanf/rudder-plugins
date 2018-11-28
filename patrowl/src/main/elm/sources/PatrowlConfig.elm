port module PatrowlConfig exposing (..)

import Html exposing (..)
import Html.Attributes exposing ( style, class, type_, checked )
import Html.Events exposing (..)
import String
import Toasty
import Toasty.Defaults
import Http exposing (Error)
import Http exposing (..)
import Json.Encode as E
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing(..)
import Dict exposing (Dict)

------------------------------
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions   model =  Sub.none

------------------------------
-- Init and main --
------------------------------

init : { contextPath: String } -> (Model, Cmd Msg)
init flags =
  let
    initModel = Model flags.contextPath Nothing Toasty.initialState
  in
    initModel ! [ getPatrowlConf initModel ]

main = programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


------------------------------
-- MODEL --
------------------------------


-- the full list of digest + list of users
type alias PatrowlConf =
  { baseUrl  : String
  , idMapping: Dict String Int
  }

type alias Model =
  { contextPath: String
  , patrowlConf: Maybe PatrowlConf  -- from API
  , toasties   : Toasty.Stack Toasty.Defaults.Toast
  }

type Msg
  = GetPatrowlConfig (Result Error PatrowlConf)
  -- NOTIFICATIONS
  | ToastyMsg (Toasty.Msg Toasty.Defaults.Toast)


------------------------------
-- API --
------------------------------

-- API call to get the category tree
getPatrowlConf : Model -> Cmd Msg
getPatrowlConf model =
  let
    url     = (model.contextPath ++ "/secure/api/patrowl/config")
    headers = []
    req = request {
        method          = "GET"
      , headers         = []
      , url             = url
      , body            = emptyBody
      , expect          = expectJson decodeApiCurrentPatrowlConf
      , timeout         = Nothing
      , withCredentials = False
      }
  in
    send GetPatrowlConfig req

-- encode / decode JSON

-- decode the JSON answer from a "get" API call - only "data" field content is interesting
decodeApiCurrentPatrowlConf : Decoder PatrowlConf
decodeApiCurrentPatrowlConf =
  D.at ["data" ] decodeCurrentPatrowlConf


decodeCurrentPatrowlConf : Decoder PatrowlConf
decodeCurrentPatrowlConf = decode PatrowlConf
  |> required "patrowlBaseUrl" D.string
  |> required "nodeIdMapping"  (D.dict D.int)

------------------------------
-- UPDATE --
------------------------------

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
{-- Api Calls message --}
    GetPatrowlConfig result ->
      case result of
        Ok conf ->
          let
            newModel = { model | patrowlConf = Just conf }
          in
            (newModel, Cmd.none)
        Err err  ->
          processApiError err model

    ToastyMsg subMsg ->
      Toasty.update defaultConfig ToastyMsg subMsg model

processApiError: Error -> Model -> (Model, Cmd Msg)
processApiError err model =
  let
    newModel = { model | patrowlConf = Nothing }
  in
    (newModel, Cmd.none) |> (createErrorNotification "Error while trying to fetch settings." err)

------------------------------
-- VIEW --
------------------------------


view: Model -> Html Msg
view model =
  let
    content = case model.patrowlConf of
                Nothing     -> text "Waiting for data from server..."
                Just config -> displayPatrowlConf config
  in
    div [] [
        content
      , div[class "toasties"][Toasty.view defaultConfig Toasty.Defaults.view ToastyMsg model.toasties]
    ]


displayPatrowlConf: PatrowlConf -> Html Msg
displayPatrowlConf patrowlConf =
  div [class "row"] [
    div[class "col-xs-12"]
    [ p [ class "callout-fade callout-info" ]
      [ div [class "marker"][span [class "glyphicon glyphicon-info-sign"][]]
      , text ("Patrowl server base URL: " ++ patrowlConf.baseUrl)
      ]
    ]
  ]


------------------------------
-- NOTIFICATIONS --
------------------------------

getErrorMessage : Http.Error -> String
getErrorMessage e =
  let
    errMessage = case e of
      Http.BadStatus b         -> let
                                    status = b.status
                                    message = status.message
                                  in
                                    ("Code "++Basics.toString(status.code)++" : "++message)
      Http.BadUrl str          -> "Invalid API url"
      Http.Timeout             -> "It took too long to get a response"
      Http.NetworkError        -> "Network error"
      Http.BadPayload str rstr -> str
  in
    errMessage


defaultConfig : Toasty.Config Msg
defaultConfig =
  Toasty.Defaults.config
    |> Toasty.delay 999999999
    |> Toasty.containerAttrs
    [ style
        [ ( "position", "fixed" )
        , ( "top", "50px" )
        , ( "right", "30px" )
        , ( "width", "100%" )
        , ( "max-width", "500px" )
        , ( "list-style-type", "none" )
        , ( "padding", "0" )
        , ( "margin", "0" )
        ]
    ]

tempConfig : Toasty.Config Msg
tempConfig = defaultConfig |> Toasty.delay 3000


addTempToast : Toasty.Defaults.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addTempToast toast ( model, cmd ) =
  Toasty.addToast tempConfig ToastyMsg toast ( model, cmd )

addToast : Toasty.Defaults.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
  Toasty.addToast defaultConfig ToastyMsg toast ( model, cmd )

createSuccessNotification : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
createSuccessNotification message =
  addTempToast (Toasty.Defaults.Success "Success!" message)

createErrorNotification : String -> Http.Error -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
createErrorNotification message e =
  addToast (Toasty.Defaults.Error "Error..." (message ++"  ("++(getErrorMessage e)++")"))

createDecodeErrorNotification : String -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
createDecodeErrorNotification message e =
  addToast (Toasty.Defaults.Error "Error..." (message ++"  ("++(e)++")"))

