port module PackageScanNode exposing (..)

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

init : { contextPath: String, report: D.Value} -> (Model, Cmd Msg)
init flags =
  let
    initModel = Model flags.contextPath (parseJsonReport flags.report) Toasty.initialState
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

type alias Vulnerability =
  { bulletinVersion: String -- should be the version where vuln is corrected
  , cvss           : CVSS
  , cvelist        : List String
  }

type alias CVSS =
  { score: Float
  , vector: String
  }

type alias Report =
  { packages       : Dict String (Dict String (List Vulnerability))
  , vulnerabilities: List String
  , cvss           : CVSS
  , cvslist        : List String
  }

type alias Model =
  { contextPath: String
  , report: Result String Report
  , toasties   : Toasty.Stack Toasty.Defaults.Toast
  }

type Msg
  = ToastyMsg (Toasty.Msg Toasty.Defaults.Toast)

-- encode / decode JSON

parseJsonReport : D.Value -> Result String Report
parseJsonReport value = D.decodeValue decodeFullReport value

decodeFullReport : Decoder Report
decodeFullReport =
  D.at ["data" ] decodeReport




decodeReport : Decoder Report
decodeReport = decode Report
  |> required "packages"        (D.dict (D.dict (D.list decodeVulnerability)))
  |> required "vulnerabilities" (D.list D.string)
  |> required "cvss"            decodeCvss
  |> required "cvelist"         (D.list D.string)

decodeCvss : Decoder CVSS
decodeCvss = decode  CVSS
  |> required "score"  D.float
  |> required "vector" D.string

decodeVulnerability : Decoder Vulnerability
decodeVulnerability = decode Vulnerability
  |> required "bulletinVersion" D.string
  |> required "cvss"            decodeCvss
  |> required "cvelist"         (D.list D.string)


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
    newModel = { model | report = Err "API error" }
  in
    (newModel, Cmd.none) |> (createErrorNotification "Error while trying to fetch settings." err)

------------------------------
-- VIEW --
------------------------------


view: Model -> Html Msg
view model =
  let
    content = case model.report of
                Err err   -> [ text ("There was an error when trying to get security reports: " ++ err) ]
                Ok report ->
                  if List.isEmpty report.vulnerabilities then
                    [ div [class "row"]
                      [ div [class "col-xs-12 bg-success"]
                        [ b [] [text "No known vulnerabilities in packages installed on that node."]
                        ]
                      ]
                    ]

                  else
                    [ displayReport report ]
  in
    div [] (List.append
         content
         [div[class "toasties"][Toasty.view defaultConfig Toasty.Defaults.view ToastyMsg model.toasties]]
    )


displayReport: Report -> Html Msg
displayReport report =
  div [class "container-fluid"]
  [ div [class "row"]
    [ div [class "col-xs-4"]
      [ b [] [text "CVSS Security Rating"]
      ]
    , div [class "col-xs-8"]
      [ b [] [text (toString report.cvss.score)]
      ]
    ]
  , div [class "row"]
    [ div [class "col-xs-4"]
      [ b [] [text "CVSS Vector"]
      ]
    , div [class "col-xs-8"]
      [ span [] [text report.cvss.vector]
      ]
    ]
  , div [class "row"]
    [ div [class "col-xs-4"]
      [ b [] [text "Vulnerabilities"]
      ]
    ]
  , div [class "row"]
    [ div [class "col-xs-8"]
      (List.map (\v -> a [class "col-xs-4"] [ text v]) report.vulnerabilities)
    ]
  , div [class "row"]
    [ div [class "col-xs-4"]
      [ b [] [text "Affected Packages"]
      ]
    ]
  , div [class "row"]
    [ div [class "col-xs-8"]
      (List.map (\v -> a [class "col-xs-4"] [ text v]) (Dict.keys report.packages))
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

