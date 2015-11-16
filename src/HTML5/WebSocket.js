"use strict";

// module WebSocket

exports.withWebSocketImpl = function(config, handlers, ok, err) {
    return function() {
        var socket, h;
        try {
            socket = new window.WebSocket(config.uri, config.protocols);
        } catch (e) {
            err(e.type)();
            return {};
        }
        h = handlers(socket);
        socket.onopen = function() {
            h.onOpen();
            return {};
        };
        socket.onmessage = function(ev) {
            h.onMessage(ev.data)();
            return {};
        };
        socket.onclose = function() {
            ok({})();
            return {};
        };
        socket.onerror = function(e) {
            err(e)();
            return {};
        };
        return {};
    };
};

exports.sendImpl = function(socket, data) {
    return function() {
        socket.send(data);
        return {};
    };
};
