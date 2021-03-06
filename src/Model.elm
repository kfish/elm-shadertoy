module Model exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Texture as Texture exposing (Texture, Error)
import Window
import Time exposing (..)
import Task exposing (Task)

{-| Every half a second there's an event coming through;
these are all the valid actions we could receive.
# Move - the user is trying to jump using the space key, move using the
arrow keys, or the window is being resized.
# TextureLoaded - a texture has been loaded across the wire
-}

type Msg
    = TextureLoaded (Result Error Texture)
    | KeyChange (Keys -> Keys)
    | MouseMove MouseMovement
    | LockRequest Bool
    | LockUpdate Bool
    | Animate Time
    | Resize Window.Size

type alias Person =
    { position : Vec3
    , velocity : Vec3
    , horizontalAngle : Float
    , verticalAngle   : Float
    }

type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , space : Bool
    }

{-| This type is returned by the fullscreen JS api in PointerLock.js
for mouse movement -}
type alias MouseMovement = (Int, Int)

{-| This is the application's Model data structure -}
type alias Model =
    { person : Person
    , lifetime : Time
    , maybeTexture : Maybe Texture
    , maybeWindowSize : Maybe Window.Size
    , keys : Keys
    , wantToBeLocked : Bool
    , isLocked : Bool
    , message : String
    }

type alias Args =
    { movement : MouseMovement
    , isLocked : Bool
    }

{-| When the application first starts, this is the initial state of the Model.

Not using the movement attribute of Args at this time;
it's a carryover from the original, and the additional complexity
to actually use it is probably not worth it in this case.
It's still a useful example using Html.programWithFlags though.
-}
init : Args -> (Model, Cmd Msg)
init { movement, isLocked } =
    ( { person =
            { position = vec3 0 eyeLevel -10
            , velocity = vec3 0 0 0
            , horizontalAngle = degrees 90
            , verticalAngle = 0
            }
      , lifetime = 0
      , maybeTexture = Nothing
      , maybeWindowSize = Nothing
      , keys = Keys False False False False False
      , wantToBeLocked = True
      , isLocked = isLocked
      , message = "No texture yet"
      }
    , Cmd.batch
        [ Task.attempt TextureLoaded (Texture.load "resources/woodCrate.jpg")
        , Task.perform Resize Window.size
        ]
    )

direction : Person -> Vec3
direction person =
    let h = person.horizontalAngle
        v = person.verticalAngle
    in
        vec3 (cos h) (sin v) (sin h)

eyeLevel : Float
eyeLevel = 2

