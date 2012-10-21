(*
  channel.ml
  Copyright (c) 2012- Masaki WATANABE
  license: see LICENSE
*)
open Utils
open ExtString
open HttpRequest
open Lwt
open Unix

class type channel = object
  method shutdown : unit
  method read_frame : Frame.t Lwt.t
  method write_text_frame : string -> unit Lwt.t
  method write_text_frame_with_mask : string -> unit Lwt.t
  method write_ping_frame : unit Lwt.t
  method write_pong_frame : unit Lwt.t
  method write_close_frame : unit Lwt.t
end

type t = channel

let websocket_guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
;;

let string_of_sha1 str =
  Cryptokit.hash_string (Cryptokit.Hash.sha1()) str
;;

let string_of_base64 str =
  Cryptokit.transform_string (Cryptokit.Base64.encode_compact_pad ()) str
;;

let get_sec_websocket_accept_key sec_websocket_key =
  String.concat "" [sec_websocket_key; websocket_guid] +>
    string_of_sha1 +>
    string_of_base64
;;

let get_handshake_response sec_websocket_accept_key =
  String.concat "\r\n" [
    "HTTP/1.1 101 Switching Protocols";
    "Upgrade: WebSocket";
    "Connection: Upgrade";
    Printf.sprintf "Sec-WebSocket-Accept: %s\r\n" sec_websocket_accept_key;
    "";
  ]
;;

let accept sock_listen =
  Lwt_unix.accept sock_listen >>= fun (sock, addr) ->
  Lwt_unix.set_close_on_exec sock;
  Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;

  let inchan = Lwt_chan.in_channel_of_descr sock in
  let outchan = Lwt_chan.out_channel_of_descr sock in
  let stream = Lwt_io.read_chars inchan in
  let send str = Lwt_chan.output_string outchan str >>= fun () ->
    Lwt_chan.flush outchan in

  (** first, read client http request *)
  HttpRequest.parse stream >>= fun request ->
  let sec_websocket_key = List.assoc "Sec-WebSocket-Key" request.input_header_fields in
  let sec_websocket_accept_key = get_sec_websocket_accept_key sec_websocket_key in

  (** second, write back handshake response *)
  let response = get_handshake_response sec_websocket_accept_key in
  send response >>= fun () ->

  (** generate client to server channel *)
  let ws_channel : channel = object
    method shutdown = Lwt_unix.shutdown sock Lwt_unix.SHUTDOWN_ALL
    method read_frame = Frame.unpack stream
    method write_text_frame str = send @@ Frame.pack_text_frame str ~mask:false
    method write_text_frame_with_mask str = send @@ Frame.pack_text_frame str ~mask:true
    method write_ping_frame = send @@ Frame.pack_ping_frame ()
    method write_pong_frame = send @@ Frame.pack_pong_frame ()
    method write_close_frame = send @@ Frame.pack_close_frame ()
  end in

  return (ws_channel, addr)
;;

let connect ?(sec_socket_key="none") ~host ~port =
  let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.set_close_on_exec sock;
  Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
  let host_entry = Unix.gethostbyname host in
  let inet_addr = host_entry.h_addr_list.(0) in
  let sockaddr = ADDR_INET(inet_addr, port) in
  Lwt_unix.connect sock sockaddr >>= fun () ->

  let inchan = Lwt_chan.in_channel_of_descr sock in
  let outchan = Lwt_chan.out_channel_of_descr sock in
  let stream = Lwt_io.read_chars inchan in
  let send str = Lwt_chan.output_string outchan str >>= fun () ->
    Lwt_chan.flush outchan in

  (** first, send websocket request with sec_socket_key *)
  send @@ spf "GET http://%s:%d HTTP/1.0\r\n" host port >>= fun () ->
  send @@ spf "Sec-WebSocket-Key:%s\r\n\r\n" sec_socket_key >>= fun () ->

  (** second, read handshake response *)
  HttpRequest.parse stream >>= fun request ->

  (** generate client to server channel *)
  let ws_channel : channel = object
    method shutdown = Lwt_unix.shutdown sock Lwt_unix.SHUTDOWN_ALL
    method read_frame = Frame.unpack stream
    method write_text_frame str = send @@ Frame.pack_text_frame str ~mask:true (** client to server frame MUST be masked. *)
    method write_text_frame_with_mask str = send @@ Frame.pack_text_frame str ~mask:true
    method write_ping_frame = send @@ Frame.pack_ping_frame ()
    method write_pong_frame = send @@ Frame.pack_pong_frame ()
    method write_close_frame = send @@ Frame.pack_close_frame ~mask:true ()
  end in

  return ws_channel
;;
