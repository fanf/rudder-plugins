port module PatrowlNode exposing (..)

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

init : { contextPath: String, assetId: Int, report: D.Value} -> (Model, Cmd Msg)
init flags =
  let
    initModel = Model flags.contextPath flags.assetId (parseJsonReport flags.report) Toasty.initialState
  in
    (initModel, Cmd.none)

main = programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


------------------------------
-- MODEL --
------------------------------

type alias RiskLevel =
  { grade   : String
  , total   : Int
  , info    : Int
  , low     : Int
  , medium  : Int
  , high    : Int
  , critical: Int
  }

type alias AssetReport =
  { name       : String
  , description: String
  , assetType  : String -- typically 'ip', 'range', etc
  , assetValue : String -- typically the value for the given type
  , criticity  : String
  , createdAt  : String
  , updatedAt  : String
  , riskLevel  : RiskLevel
  }

type alias Model =
  { contextPath: String
  , assetId    : Int
  , assetReport: Result String AssetReport
  , toasties   : Toasty.Stack Toasty.Defaults.Toast
  }

type Msg
  = ToastyMsg (Toasty.Msg Toasty.Defaults.Toast)

-- encode / decode JSON

parseJsonReport : D.Value -> Result String AssetReport
parseJsonReport value = D.decodeValue decodeFullAssetReport value

decodeFullAssetReport : Decoder AssetReport
decodeFullAssetReport =
  D.at ["asset" ] decodeAssetReport




decodeAssetReport : Decoder AssetReport
decodeAssetReport = decode AssetReport
  |> required "name"        D.string
  |> required "description" D.string
  |> required "type"        D.string
  |> required "value"       D.string
  |> required "criticity"   D.string
  |> required "created_at"  D.string
  |> required "updated_at"  D.string
  |> required "risk_level"  decodeRiskLevel

decodeRiskLevel : Decoder RiskLevel
decodeRiskLevel = decode  RiskLevel
  |> required "grade"    D.string
  |> required "total"    D.int
  |> required "info"     D.int
  |> required "low"      D.int
  |> required "medium"   D.int
  |> required "high"     D.int
  |> required "critical" D.int

------------------------------
-- UPDATE --
------------------------------

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
  ToastyMsg subMsg ->
    Toasty.update defaultConfig ToastyMsg subMsg model

processApiError: Error -> Model -> (Model, Cmd Msg)
processApiError err model =
  let
    newModel = { model | assetReport = Err "API error" }
  in
    (newModel, Cmd.none) |> (createErrorNotification "Error while trying to fetch settings." err)

------------------------------
-- VIEW --
------------------------------


view: Model -> Html Msg
view model =
  let
    content = case model.assetReport of
                Err err   -> text ("There was an error when trying to get security reports: " ++ err)
                Ok report -> displayAssetReport report
  in
    div [] [
        content
      , div[class "toasties"][Toasty.view defaultConfig Toasty.Defaults.view ToastyMsg model.toasties]
    ]


displayAssetReport: AssetReport -> Html Msg
displayAssetReport assetReport =
  div [class "container-fluid"]
  [ div [class "row"]
    [ div [class "col-xs-12"]
      [ dl [class "dl-horizontal col-xs-6"]
        [ dt [] [ text "Name:"]
        , dd [] [ b [] [text assetReport.name] ]
        , dt [] [ text "Asset type [value]:" ]
        , dd [] [ text (assetReport.assetType ++ " [" ++ assetReport.assetValue ++ "]") ]
        , dt [] [ text "Description:" ]
        , dd [] [ text assetReport.description ]
        , dt [] [ text "Criticity:" ]
        , dd [] [ text assetReport.criticity ]
        , dt [] [ text "Created at:" ]
        , dd [] [ text assetReport.createdAt ]
        , dt [] [ text "Last updated:" ]
        , dd [] [ text assetReport.updatedAt ]
        ]
      , div [class "col-xs-6 text-center"]
        [ div [class "row"]
          [ b [] [text "Global Security Rating"]
          , span [class "glyphicon glyphicon-dashboard"] []
          , div [class ("risk-grade risk-grade-"++assetReport.riskLevel.grade)]
            [ b [] [text assetReport.riskLevel.grade]
            ]
          ]
        ]
      ]
    ]
  , div [ class "row finding-stats"]
    [ div [class "col-md-2 col-md-offset-1"]
      [ div [class "tile tile-critical"]
        [ div [] [ text "Critical:"]
        , span [class "label label-critical"] [text (toString assetReport.riskLevel.critical)]
        ]
      ]
    , div [class "col-md-2"]
      [ div [class "tile tile-high"]
        [ div [] [ text "High:"]
        , span [class "label label-high"] [text (toString assetReport.riskLevel.high)]
        ]
      ]
    , div [class "col-md-2"]
      [ div [class "tile tile-medium"]
        [ div [] [ text "Medium:"]
        , span [class "label label-medium"] [text (toString assetReport.riskLevel.medium)]
        ]
      ]
    , div [class "col-md-2"]
      [ div [class "tile tile-low"]
        [ div [] [ text "Low:"]
        , span [class "label label-low"] [text (toString assetReport.riskLevel.low)]
        ]
      ]
    , div [class "col-md-2"]
      [ div [class "tile tile-info"]
        [ div [] [ text "Info:"]
        , span [class "label label-info"] [text (toString assetReport.riskLevel.info)]
        ]
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

