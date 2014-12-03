module HTML5.WebSocket
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

import Control.Monad.Eff (Eff (..))
import Data.Either
import Data.Function

import Control.Monad.Cont.Trans

type WebSocketConfig =
  { uri :: URI
  , protocols :: [Protocol]
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

foreign import withWebSocketImpl """
  function withWebSocketImpl (config, handlers, ok, err) {
    return function () {
      var socket, h;
      try {
        socket = new window.WebSocket(config.uri, config.protocols);
      } catch (e) {
        err(e.type)();
        return {};
      }
      h = handlers(socket);
      socket.onopen = function () {
        h.onOpen();
        return {};
      };
      socket.onmessage = function (ev) {
        h.onMessage(ev.data)();
        return {};
      };
      socket.onclose = function () {
        ok({})();
        return {};
      };
      socket.onerror = function (e) {
        err(e)();
        return {};
      };
      return {};
    };
  }""" :: forall eff.
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

foreign import sendImpl """
  function sendImpl (socket, data) {
    return function () {
      socket.send(data);
      return {};
    };
  }""" :: forall eff.  Fn2 Socket String (WithWebSocket eff Unit)

send :: forall eff.  Socket -> String -> WithWebSocket eff Unit
send = runFn2 sendImpl
