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
  , bsend
  , withWebSocket
  , runWebSocket
  ) where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)
import Data.Either (Either(..))
import Control.Monad.Cont.Trans (ContT(..), runContT)
import Data.Function.Uncurried (runFn2, Fn2, Fn4, runFn4)
import Data.ArrayBuffer.Types (ArrayBuffer)


type WebSocketConfig =
  { uri :: URI
  , protocols :: Array Protocol
  , binary :: Boolean
  }

type URI = String
type Protocol = String

type WithWebSocket eff = Eff (ws :: WS | eff)
type WebSocket eff = ContT Unit (WithWebSocket eff)
type WebSocketError = String

foreign import data WS :: Effect

foreign import data Socket :: Type

type WebSocketHandler eff = Socket ->
  { onOpen :: WithWebSocket eff Unit
  , onMessage :: String -> WithWebSocket eff Unit
  , onBuffer :: ArrayBuffer -> WithWebSocket eff Unit
  }

runWebSocket :: forall eff. WebSocket eff Unit -> WithWebSocket eff Unit
runWebSocket = flip runContT pure

foreign import withWebSocketImpl :: forall eff.
          Fn4 WebSocketConfig
              (WebSocketHandler eff)
              (Unit -> WithWebSocket eff Unit)
              (WebSocketError -> WithWebSocket eff Unit)
              (WithWebSocket eff Unit)

defaultHandlers :: forall eff. WebSocketHandler eff
defaultHandlers _ =
  { onOpen: pure unit
  , onMessage: const $ pure unit
  , onBuffer: const $ pure unit
  }

withWebSocket :: forall eff. WebSocketConfig
                 -> WebSocketHandler eff
                 -> WebSocket eff (Either WebSocketError Unit)
withWebSocket c h = ContT $ \k -> runFn4 withWebSocketImpl c h
                                         (k <<< Right)
                                         (k <<< Left)

foreign import sendImpl :: forall eff a.  Fn2 Socket a (WithWebSocket eff Unit)

send :: forall eff.  Socket -> String -> WithWebSocket eff Unit
send = runFn2 sendImpl

bsend :: forall eff.  Socket -> ArrayBuffer -> WithWebSocket eff Unit
bsend = runFn2 sendImpl
