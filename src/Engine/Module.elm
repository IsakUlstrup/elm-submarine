module Engine.Module exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)



-- SIGNAL


type Signal
    = Signal Float


newSignal : Float -> Signal
newSignal n =
    Signal (clamp -1 1 n)


signalToBool : Signal -> Bool
signalToBool (Signal signal) =
    signal == 1


signalToFloat : Signal -> Float
signalToFloat (Signal signal) =
    signal



-- MODULE


type alias Module a =
    Dict String a



updateModule : (Signal -> a -> a) -> ModuleMsg -> Int -> Module a -> Module a
updateModule updatePart msg moduleId m =
	if msg.id == moduleId then
		Dict.map (\_ value -> updatePart msg.signal value)   m
	else
		m

type alias Modules a =
    Array (Module a)


type alias ModuleMsg =
    { id : Int
    , name : String
    , signal : Signal
    }
