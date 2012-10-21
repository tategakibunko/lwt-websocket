type frame =
    PingFrame of string
  | PongFrame of string
  | TextFrame of string
  | CloseFrame of int * string
  | BinaryFrame
  | UndefinedFrame of string

type t = frame

val pack_ping_frame :
  ?payload_text:string ->
  unit -> string

val pack_pong_frame :
  ?payload_text:string ->
  unit -> string

val pack_close_frame :
  ?mask:bool ->
  ?status_code:int option ->
  ?payload_text:string ->
  unit -> string

val pack_text_frame :
  ?mask:bool ->
  string -> string

val unpack :
  char Lwt_stream.t -> frame Lwt.t
