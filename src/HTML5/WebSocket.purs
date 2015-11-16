module WebSocket
  ( Protocol ()
  , Socket ()
  , URI ()
  , WS ()
  , WebSocket ()
  , WebSocketConfig ()
  , WebSocketError ()
  , WebSocketHandler ()
  , WithWebSocket ()
  , defaultHandlers
  , send
  , withWebSocket
  , runWebSocket
  ) where

import Prelude
import Control.Monad.Eff
import Data.Either
import Data.Function

import Control.Monad.Cont.Trans

type WebSocketConfig =
  { uri :: URI
  , protocols :: Array Protocol
  }

type URI = String
type Protocol = String

type WithWebSocket eff = Eff (ws :: WS | eff)
type WebSocket eff = ContT Unit (WithWebSocket eff)
type WebSocketError = String

foreign import data WS :: !

foreign import data Socket :: *

type WebSocketHandler eff = Socket ->
  { onOpen :: WithWebSocket eff Unit
  , onMessage :: String -> WithWebSocket eff Unit
  }

runWebSocket :: forall eff. WebSocket eff Unit -> WithWebSocket eff Unit
runWebSocket = flip runContT return

foreign import withWebSocketImpl :: forall eff.
          Fn4 WebSocketConfig
              (WebSocketHandler eff)
              (Unit -> WithWebSocket eff Unit)
              (WebSocketError -> WithWebSocket eff Unit)
              (WithWebSocket eff Unit)

defaultHandlers :: forall eff. WebSocketHandler eff
defaultHandlers _ =
  { onOpen: return unit
  , onMessage: const $ return unit
  }

withWebSocket :: forall eff. WebSocketConfig
                 -> WebSocketHandler eff
                 -> WebSocket eff (Either WebSocketError Unit)
withWebSocket c h = ContT $ \k -> runFn4 withWebSocketImpl c h
                                         (k <<< Right)
                                         (k <<< Left)

foreign import sendImpl :: forall eff.  Fn2 Socket String (WithWebSocket eff Unit)

send :: forall eff.  Socket -> String -> WithWebSocket eff Unit
send = runFn2 sendImpl
