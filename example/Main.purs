module Main where

import Prelude
import WebSocket
import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff ())
import Control.Monad.Trans (lift)
import Data.Either (Either (Left, Right))

main :: forall eff. Eff (ws :: WS, console :: CONSOLE | eff) Unit
main = do
  log "START"
  runWebSocket $ do
    result <- withWebSocket config handlers
    case result of
         Right _ -> output "DONE"
         Left err -> output err

config =
  { uri: "ws://127.0.0.1:9001"
  , protocols: []
  }

handlers :: forall eff. WebSocketHandler (console :: CONSOLE | eff)
handlers s = (defaultHandlers s)
  { onOpen = onOpenHandler
  , onMessage = onMessageHandler
  }

onOpenHandler :: forall eff. WithWebSocket (console :: CONSOLE | eff) Unit
onOpenHandler = log "OPEN"

onMessageHandler :: forall eff. String -> WithWebSocket (console :: CONSOLE | eff) Unit
onMessageHandler msg = log msg

output :: forall eff. String -> WebSocket (console :: CONSOLE | eff) Unit
output = lift <<< log

