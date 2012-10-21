open Unix
open Lwt

let rec main host port =
  Lwt_websocket.Channel.connect
    ~sec_socket_key:"hoge" ~host ~port >>= fun channel ->
  
  (** send ping *)
  channel#write_ping_frame >>= fun () ->
  print_endline "ping send";

  (** read pong *)
  channel#read_frame >>= fun frame ->

  debug_frame frame;

  (** send close frame *)
  channel#write_close_frame >>= fun () ->

  (** read close frame *)
  channel#read_frame >>= fun frame ->

  debug_frame frame;

  return ()

and debug_frame = function
  | Lwt_websocket.Frame.PongFrame(msg) ->
    Printf.printf "pong ok(%s)\n" msg;

  | Lwt_websocket.Frame.CloseFrame(code, msg) ->
    Printf.printf "close frame(code=%x, msg=%s)\n" code msg;

  | _ ->
    Printf.printf "other frame"
;;

let () = 
  let host = ref "127.0.0.1" in
  let port = ref 8080 in

  Arg.parse [
    ("-host", Arg.String (fun s -> host := s), "host");
    ("-port", Arg.Int (fun i -> port := i), "port");
  ] ignore "client.exe -host [hostname] -port [port_no]";

  let _ = Lwt_main.run (main !host !port) in ()
;;

