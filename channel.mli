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

val accept : Lwt_unix.file_descr -> (channel * Lwt_unix.sockaddr) Lwt.t
(** [accept socket_for_listen] return server side channel and client sockaddr.
*)

val connect :
  ?sec_socket_key:string ->
  host:string -> port:int -> channel Lwt.t
(** [connect sec_socket_key host port] return client side channel.
*)
