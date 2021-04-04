module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Url
import Url.Builder as Query exposing (crossOrigin)



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type alias Flags =
    ()


type alias Model =
    { key : Nav.Key
    , url : Url.Url

    -- REQUEST
    , get_id : String
    , post_name : String
    , post_age : String

    -- RESPONSE
    , get_result_user : Maybe User
    , get_result_userList : Maybe (List User)
    , post_result_guid : Maybe Guid
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , url = url

      -- REQUEST
      , get_id = ""
      , post_name = ""
      , post_age = ""

      -- RESPONSE
      , get_result_user = Nothing
      , get_result_userList = Nothing
      , post_result_guid = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = TextChanged TextField String
    | GetUser
    | GetUserList
    | PostUser
    | ResultUser (Result Http.Error User)
    | ResultUserList (Result Http.Error (List User))
    | ResultPostUser (Result Http.Error Guid)
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


type TextField
    = GetId
    | PostName
    | PostAge


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextChanged textField input ->
            case textField of
                GetId ->
                    ( { model | get_id = input }
                    , Cmd.none
                    )

                PostName ->
                    ( { model | post_name = input }
                    , Cmd.none
                    )

                PostAge ->
                    ( { model | post_age = input }
                    , Cmd.none
                    )

        -- HTTP REQUEST
        GetUser ->
            ( model
            , getUser ResultUser model.get_id
            )

        GetUserList ->
            ( model
            , getUserList ResultUserList
            )

        PostUser ->
            ( model
            , postUser ResultPostUser
                { name = model.post_name
                , age = model.post_age |> String.toInt |> Maybe.withDefault 0
                }
            )

        -- HTTP RESPONSE
        ResultUser result ->
            case result of
                Ok user ->
                    ( { model | get_result_user = Just user }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ResultUserList result ->
            case result of
                Ok users ->
                    ( { model | get_result_userList = Just users }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ResultPostUser result ->
            case result of
                Ok guid ->
                    ( { model | post_result_guid = Just guid }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        -- APPLICATION
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Application Title"
    , body =
        [ div []
            [ div []
                [ input
                    [ onInput (TextChanged GetId), placeholder "id", value model.get_id ]
                    []
                , button
                    [ onClick GetUser ]
                    [ text "get" ]
                , case model.get_result_user of
                    Just user ->
                        viewUser user

                    Nothing ->
                        text ""
                ]
            , div []
                [ button [ onClick GetUserList ] [ text "getAll" ]
                , case model.get_result_userList of
                    Just userList ->
                        div [] (List.map viewUser userList)

                    Nothing ->
                        text ""
                ]
            , div []
                [ input [ onInput (TextChanged PostName), placeholder "name", value model.post_name ] []
                , input [ onInput (TextChanged PostAge), placeholder "age", value model.post_age ] []
                , button
                    [ onClick PostUser ]
                    [ text "post" ]
                , case model.post_result_guid of
                    Just guid ->
                        div [] [ text <| "guid:" ++ guid ]

                    Nothing ->
                        text ""
                ]
            ]
        ]
    }


viewUser : User -> Html msg
viewUser user =
    div []
        [ div [] [ text <| "id:" ++ user.id ]
        , div [] [ text <| "name:" ++ user.name ]
        , div [] [ text <| "age:" ++ String.fromInt user.age ]
        ]



-- HTTP


type alias Guid =
    String


origin : String
origin =
    "https://localhost:5001"


type alias User =
    { id : String
    , name : String
    , age : Int
    }


getUser : (Result Http.Error User -> msg) -> String -> Cmd msg
getUser msg id =
    let
        url =
            crossOrigin
                origin
                [ "api", "user" ]
                [ Query.string "id" id ]
    in
    Http.get
        { url = url
        , expect = Http.expectJson msg userDecoder
        }


getUserList : (Result Http.Error (List User) -> msg) -> Cmd msg
getUserList msg =
    let
        url =
            crossOrigin
                origin
                [ "api", "user", "list" ]
                []
    in
    Http.get
        { url = url
        , expect = Http.expectJson msg (D.list userDecoder)
        }


postUser : (Result Http.Error Guid -> msg) -> { name : String, age : Int } -> Cmd msg
postUser msg req =
    let
        url =
            crossOrigin
                origin
                [ "api", "user" ]
                []

        body =
            E.object
                [ ( "name", E.string req.name )
                , ( "age", E.int req.age )
                ]
    in
    Http.post
        { url = url
        , body = Http.jsonBody body
        , expect = Http.expectJson msg (D.field "id" D.string)
        }


userDecoder : Decoder User
userDecoder =
    D.map3 User
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "age" D.int)
