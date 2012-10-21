open Unix
open Lwt

let rec main sock_listen =

  (** accept client, and get the websocket channel for this client *)
  Lwt_websocket.Channel.accept sock_listen >>= fun (channel, addr) ->
  handle_client channel

and handle_client channel =
  channel#read_frame >>= function

    (** ping -> pong *)
    | Lwt_websocket.Frame.PingFrame(msg) ->
      print_endline (Printf.sprintf "ping(%s)" msg);
      channel#write_pong_frame >>= fun () ->
      handle_client channel (** wait for close frame from client *)

    (** text frame -> echo back *)
    | Lwt_websocket.Frame.TextFrame text ->
      channel#write_text_frame text >>= fun () ->
      handle_client channel (** wait for close frame from client *)

    | Lwt_websocket.Frame.BinaryFrame ->
      print_endline "not supported";
      return ()

    | Lwt_websocket.Frame.PongFrame(msg) ->
      print_endline "pong received";
      return ()

    (** close frame -> close frame *)
    | Lwt_websocket.Frame.CloseFrame(status_code, body) ->
      print_endline "close frame received";

      (** http://tools.ietf.org/html/rfc6455#section-5.5.1

	  If an endpoint receives a Close frame and did not previously send a
	  Close frame, the endpoint MUST send a Close frame in response. 
      *)
      channel#write_close_frame

    | Lwt_websocket.Frame.UndefinedFrame(msg) ->
      print_endline msg;
      return ()
;;

let () =
  let host = ref "127.0.0.1" in
  let port = ref 8080 in

  Arg.parse [
    ("-host", Arg.String (fun s -> host := s), "host");
    ("-port", Arg.Int (fun i -> port := i), "port");
  ] ignore "server.exe -host [hostname] -port [port_no]";
  
  let addr_inet = Unix.ADDR_INET (Unix.inet_addr_of_string !host, !port) in
  let sock_listen = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

  (** setup listen socket *)
  Lwt_unix.setsockopt sock_listen Unix.SO_REUSEADDR true;
  Lwt_unix.bind sock_listen addr_inet;
  Lwt_unix.listen sock_listen 5;

  Lwt_main.run (main sock_listen)
;;
