(*
  httpRequest.ml
  Copyright (c) 2012- Masaki WATANABE
  license: see LICENSE
*)
open Utils
open ExtString
open Lwt

type http_request = {
  path_args : string list;
  get_args : (string * string) list;
  input_header_fields : (string * string) list;
}

type t = http_request

let string_until c stream =
  Lwt_stream.get_while (fun c1 -> c <> c1) stream >>= fun chars ->
  Lwt_stream.next stream >>= fun _ ->
  return (String.implode chars)
;;

let parse_header stream =
  string_until ' ' stream >>= fun method_str ->
  string_until ' ' stream >>= fun query_string ->
  string_until '\r' stream >>= fun version ->
  Lwt_stream.next stream >>= function
    | '\n' ->
      return (method_str, query_string, version)
    | _ -> failwith "invalid header"
;;

let parse_field stream =
  Lwt_stream.npeek 2 stream >>= function
    | ['\r'; '\n'] ->
      Lwt_stream.nget 2 stream >>= fun _ -> return None
    | _ ->
      string_until ':' stream >>= fun key ->
      string_until '\r' stream >>= fun value ->
      Lwt_stream.next stream >>= function
	| '\n' ->
	  return @@ Some (String.strip key, String.strip value)
	| _ ->
	  return None
;;

let parse_many parse_fn stream =
  let rec iter ret =
    parse_fn stream >>= function
      | Some value -> iter (value :: ret)
      | None -> return ret in
  iter []
;;

let parse_query_args query_string =
  let parse_path_args str =
    String.nsplit str "/" in
  let parse_nv str =
    match String.nsplit "=" str with
      | [name; value] -> (name, value)
      | _ -> (str, "") in
  let parse_get_args str =
    String.nsplit "&" str +> List.map parse_nv in
  match String.nsplit query_string "?" with
    | before :: after :: _ -> (parse_path_args before, parse_get_args after)
    | _ -> (parse_path_args query_string, [])
;;

let parse stream =
  parse_header stream >>= fun (method_str, query_string, version) ->
  parse_many parse_field stream >>= fun input_header_fields ->
  let (path_args, get_args) = parse_query_args query_string in
  return {path_args; get_args; input_header_fields}
;;
