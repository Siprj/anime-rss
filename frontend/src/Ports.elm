port module Ports exposing (fetchXToken, xTokenReceiver)

port fetchXToken : () -> Cmd msg
port xTokenReceiver: (String -> msg) -> Sub msg
