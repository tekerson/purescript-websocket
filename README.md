# Module Documentation

## Module HTML5.WebSocket

### Types

    type Protocol = String

    data Socket :: *

    type URI = String

    type WebSocket eff = ContT Unit (WithWebSocket eff)

    type WebSocketConfig = { protocols :: [Protocol], uri :: URI }

    type WebSocketError = String

    type WebSocketHandler eff = Socket -> { onMessage :: String -> WithWebSocket eff Unit, onOpen :: WithWebSocket eff Unit }

    type WithWebSocket eff = Eff (ws :: WS | eff)


### Values

    defaultHandlers :: forall eff. WebSocketHandler eff

    send :: forall eff. Socket -> String -> WithWebSocket eff Unit

    withWebSocket :: forall eff. WebSocketConfig -> WebSocketHandler eff -> WebSocket eff (Either WebSocketError Unit)