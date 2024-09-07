module Engine.Module exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)



-- SIGNAL


type Signal
    = Float Float
    | Bool Bool



-- MODULE


type alias Part a =
    { name : String
    , channels : List String
    , state : a
    }


type alias Module a =
    Dict String a


newModule : List ( String, a ) -> Module a
newModule parts =
    Dict.fromList parts


updateModule : (ModuleMsg -> String -> a -> a) -> ModuleMsg -> Module a -> Module a
updateModule updatePart msg m =
    Dict.map
        (\name value ->
            updatePart msg name value
        )
        m


updateModules : (ModuleMsg -> String -> a -> a) -> ModuleMsg -> Modules a -> Modules a
updateModules updatePart msg modules =
    Array.map (updateModule updatePart msg) modules


updateModuleAtIndex : Int -> (ModuleMsg -> String -> a -> a) -> ModuleMsg -> Modules a -> Modules a
updateModuleAtIndex index updatePart msg modules =
    let
        m =
            Array.get index modules
                |> Maybe.map (updateModule updatePart msg)
    in
    case m of
        Just updatedModule ->
            Array.set index updatedModule modules

        _ ->
            modules


type alias Modules a =
    Array (Module a)


empty : Modules a
empty =
    Array.empty


addModule : Module a -> Modules a -> Modules a
addModule m modules =
    Array.push m modules


modulesToList : Modules a -> List ( Int, Module a )
modulesToList modules =
    Array.toIndexedList modules


type alias ModuleMsg =
    { name : String
    , signal : Signal
    }
