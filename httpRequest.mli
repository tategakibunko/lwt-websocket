type http_request = {
  path_args : string list;
  get_args : (string * string) list;
  input_header_fields : (string * string) list;
}

type t = http_request

val parse : char Lwt_stream.t -> http_request Lwt.t
