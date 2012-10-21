# lwt-websocket

1. provide server to client channel of websocket.

2. provide client to server channel of websocket.

## server to client channel

Lwt_websocket.Channel.accept sock_listen >>= fun (addr, channel) ->
channel#write_text_frame "hello, i am server."

## client to server channel

Lwt_websocket.Channel.connect "127.0.0.1" 8080 ~sec_socket_key:"aaa" >>= fun channel ->
channel#write_text_frame "hello, i am client."

