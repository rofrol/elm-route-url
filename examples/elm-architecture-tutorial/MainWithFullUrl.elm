module Main exposing (..)

import Html exposing (Html, div, p, text, table, tr, td, map)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import RouteUrl exposing (RouteUrlProgram, UrlChange)
import RouteUrl.Builder as Builder exposing (Builder)
import Navigation exposing (Location)


-- Note that I'm renaming these locally for simplicity.

import Example1.Counter as Example1
import Example5.RandomGif as Example5
import Example8.SpinSquarePair as Example8


main : RouteUrlProgram Never Model Action
main =
    RouteUrl.program
        { delta2url = delta2url
        , location2messages = url2messages
        , init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL
-- We'll need to know which example we're showing at the moment.


type Example
    = Example1
    | Example5
    | Example8


{-| We need to collect all the data that each example wants to track. Now, we
could do this in a couple of ways. If we want to remember all the data as we
display one thing or another, we would do this as a record. If we wanted to
only remember the data that we're currently looking at, we might do this as
a union type. I'll do it the record way for now.

In a real app, you are likely to divide the model into parts which are
"permanent" (in the sense that the app needs to remember them, no matter
what the user is looking at now), and parts that are "transient" (which need
to be remembered, but only while the user is looking at a particular thing).
So, in that cae, some things would be in a record, whereas other things would
be in a union type.
-}
type alias Model =
    { example1 : Example1.Model
    , example5 : Example5.Model
    , example8 :
        Example8.Model
        -- And, we need to track which example we're actually showing
    , currentExample : Example
    }



-- Now, to init our model, we have to collect each examples init


init : ( Model, Cmd Action )
init =
    let
        model =
            { example1 = Example1.init
            , example5 = Tuple.first Example5.init
            , example8 = Tuple.first Example8.init
            , currentExample = Example5
            }
    in
        ( model, Cmd.none )



-- SUBSCRIPTIONS
-- I happen to know that only Example8 uses them


subscriptions : Model -> Sub Action
subscriptions model =
    --     Sub.none
    Sub.map Example8Action (Example8.subscriptions model.example8)



-- UPDATE


type Action
    = Example1Action Example1.Action
    | Example5Action Example5.Action
    | Example8Action Example8.Action
    | ShowExample Example
    | NoOp


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        NoOp ->
            ( model, Cmd.none )

        ShowExample example ->
            case example of
                Example1 ->
                    ( { model | currentExample = example, example1 = Example1.init }
                    , Cmd.none
                    )

                Example5 ->
                    ( { model | currentExample = example, example5 = Tuple.first Example5.init }
                    , Cmd.map Example5Action <| Tuple.second Example5.init
                    )

                Example8 ->
                    ( { model | currentExample = example, example8 = Tuple.first Example8.init }
                    , Cmd.map Example8Action <| Tuple.second Example8.init
                    )

        Example1Action subaction ->
            ( { model | example1 = Example1.update subaction model.example1 }
            , Cmd.none
            )

        Example5Action subaction ->
            let
                result =
                    Example5.update subaction model.example5
            in
                ( { model | example5 = Tuple.first result }
                , Cmd.map Example5Action <| Tuple.second result
                )

        Example8Action subaction ->
            let
                result =
                    Example8.update subaction model.example8
            in
                ( { model | example8 = Tuple.first result }
                , Cmd.map Example8Action <| Tuple.second result
                )



-- VIEW


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


view : Model -> Html Action
view model =
    let
        viewExample =
            case model.currentExample of
                Example1 ->
                    map Example1Action (Example1.view model.example1)

                Example5 ->
                    map Example5Action (Example5.view model.example5)

                Example8 ->
                    map Example8Action (Example8.view model.example8)

        makeTitle ( example, title ) =
            let
                styleList =
                    if example == model.currentExample then
                        [ "font-weight" => "bold"
                        ]
                    else
                        [ "font-weight" => "normal"
                        , "color" => "blue"
                        , "cursor" => "pointer"
                        ]

                -- If we're already on a page, we don't have a click action
                clickAction =
                    if example == model.currentExample then
                        []
                    else
                        [ onClick (ShowExample example) ]
            in
                p (style styleList :: clickAction)
                    [ text title ]

        toc =
            div [] <|
                List.map makeTitle
                    [ ( Example1, Example1.title )
                    , ( Example5, Example5.title )
                    , ( Example8, Example8.title )
                    ]
    in
        table []
            [ tr []
                [ td
                    [ style
                        [ "vertical-align" => "top"
                        , "width" => "25%"
                        , "padding" => "8px"
                        , "margin" => "8px"
                        ]
                    ]
                    [ toc ]
                , td
                    [ style
                        [ "vertical-align" => "top"
                        , "width" => "75%"
                        , "padding" => "8px"
                        , "margin" => "8px"
                        , "border" => "1px dotted black"
                        ]
                    ]
                    [ viewExample ]
                ]
            ]



-- ROUTING


delta2url : Model -> Model -> Maybe UrlChange
delta2url previous current =
    -- So, as the last step, we map our possible `Builder` to a `UrlChange`.
    Maybe.map Builder.toUrlChange <|
        delta2builder previous current


delta2builder : Model -> Model -> Maybe Builder
delta2builder previous current =
    case current.currentExample of
        Example1 ->
            -- First, we ask the submodule for a `Maybe Builder`. Then, we use
            -- `map` to prepend something to the path.
            Example1.delta2builder previous.example1 current.example1
                |> Maybe.map (Builder.prependToPath [ "example1" ])

        Example5 ->
            Example5.delta2builder previous.example5 current.example5
                |> Maybe.map (Builder.prependToPath [ "example5" ])

        Example8 ->
            Example8.delta2builder previous.example8 current.example8
                |> Maybe.map (Builder.prependToPath [ "example8" ])


{-| This is an example of a `location2messages` function ... I'm calling it
`url2messages` to illustrate something that uses the full URL.
-}
url2messages : Location -> List Action
url2messages location =
    builder2messages (Builder.fromUrl location.href)


builder2messages : Builder -> List Action
builder2messages builder =
    case Builder.path builder of
        first :: rest ->
            let
                subBuilder =
                    Builder.replacePath rest builder
            in
                case first of
                    "example1" ->
                        -- We give the Example1 module a chance to interpret
                        -- the rest of the location, and then we prepend an
                        -- action for the part we interpreted.
                        (ShowExample Example1) :: List.map Example1Action (Example1.builder2messages subBuilder)

                    "example5" ->
                        (ShowExample Example5) :: List.map Example5Action (Example5.builder2messages subBuilder)

                    "example8" ->
                        (ShowExample Example8) :: List.map Example8Action (Example8.builder2messages subBuilder)

                    _ ->
                        -- Normally, you'd want to show an error of some kind here.
                        -- But, for the moment, I'll just default to example5
                        [ ShowExample Example5 ]

        _ ->
            -- Normally, you'd want to show an error of some kind here.
            -- But, for the moment, I'll just default to example5
            [ ShowExample Example5 ]
