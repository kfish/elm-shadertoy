module View exposing (view)

import Html
import Math.Matrix4 exposing (..)
import Math.Vector3 exposing (..)
import Time exposing (Time)

import Model
import View.Crate
import View.Diamond
import View.Ground
import WebGL
import Html exposing (Html, text, div, p)
import Html.Attributes exposing (width, height, style)
import Model exposing (Model, Msg)
import Window

{-| Generate a View from a Model
-}
view : Model -> Html Msg
view { person, lifetime, maybeWindowSize, maybeTexture, isLocked } =
    case (maybeWindowSize, maybeTexture) of
        (Nothing, _) -> text ""
        (_, Nothing) -> text ""
        (Just windowSize, Just texture) ->
            layoutScene windowSize lifetime isLocked texture person

layoutScene : Window.Size -> Time -> Bool -> WebGL.Texture -> Model.Person -> Html Msg
layoutScene windowSize t isLocked texture person =
    div
        [ style
            [ ( "width", toString width ++ "px" )
            , ( "height", toString height ++ "px" )
            , ( "position", "relative" )
            , ( "backgroundColor", "rgb(135, 206, 235)" )
            ]
        ]
        [ WebGL.toHtml
            [ width windowSize.width
            , height windowSize.height
            , style [ ( "display", "block" ) ]
            ]
            (renderWorld windowSize t texture (perspective windowSize person))
        , div
            [ style
                [ ( "position", "absolute" )
                , ( "font-family", "monospace" )
                , ( "text-align", "center" )
                , ( "left", "20px" )
                , ( "right", "20px" )
                , ( "top", "20px" )
                ]
            ]
            (if isLocked then
                exitMsg
             else
                enterMsg
            )
        ]

{-| Set up 3D world
-}
renderWorld : Window.Size -> Time -> WebGL.Texture -> Mat4 -> List WebGL.Renderable
renderWorld windowSize t texture perspective =
    let
        renderCrates =
            [ View.Diamond.fogMountainsDiamond windowSize t (translate3 0 1.5 0 perspective)
            , View.Diamond.cloudsDiamond windowSize t (translate3 5 1.5 1 perspective)
            , View.Crate.voronoiCube windowSize t (translate3 10 0 10 perspective)
            , View.Crate.fireCube windowSize t (translate3 -10 0 -10 perspective)
            , View.Crate.fogMountainsCube windowSize t (translate3 10 1.5 -10 perspective)
            , View.Crate.textureCube texture (translate3 -2 0 -17 perspective)
            ]
    in
        (View.Ground.renderGround perspective) :: renderCrates

{-| Calculate the viewer's field of view
-}
perspective : Window.Size -> Model.Person -> Mat4
perspective { width, height } person =
    mul (makePerspective 45 (toFloat width / toFloat height) 0.01 100)
        (makeLookAt person.position (person.position `add` Model.direction person) j)

enterMsg : List (Html Msg)
enterMsg = message "Click to go full screen and move your head with the mouse."

exitMsg : List (Html Msg)
exitMsg = message "Press <escape> to exit full screen."

message : String -> List (Html Msg)
message msg =
    [ p [] [ Html.text "WASD keys to move, space bar to jump." ]
    , p [] [ Html.text msg ]
    ]
