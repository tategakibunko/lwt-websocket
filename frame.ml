(*
  frame.ml
  Copyright (c) 2012- Masaki WATANABE
  license: see LICENSE
  
  http://tools.ietf.org/html/rfc6455

  0                   1                   2                   3
  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 +-+-+-+-+-------+-+-------------+-------------------------------+
 |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
 |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
 |N|V|V|V|       |S|             |   (if payload len==126/127)   |
 | |1|2|3|       |K|             |                               |
 +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
 |     Extended payload length continued, if payload len == 127  |
 + - - - - - - - - - - - - - - - +-------------------------------+
 |                               |Masking-key, if MASK set to 1  |
 +-------------------------------+-------------------------------+
 | Masking-key (continued)       |          Payload Data         |
 +-------------------------------- - - - - - - - - - - - - - - - +
 :                     Payload Data continued ...                :
 + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
 |                     Payload Data continued ...                |
 +---------------------------------------------------------------+
*)
open Utils
open Big_int
open Lwt

type frame =
    PingFrame of string
  | PongFrame of string
  | TextFrame of string
  | CloseFrame of int * string
  | BinaryFrame
  | UndefinedFrame of string

type t = frame

type opcode =
    CONT_FRAME
  | TEXT_FRAME
  | BINARY_FRAME
  | RESERVED_FRAME
  | CLOSE_FRAME
  | PING_FRAME
  | PONG_FRAME
  | RESERVED_CONTROL_FRAME

let opcode_of_int = function
  | 0x00 -> CONT_FRAME
  | 0x01 -> TEXT_FRAME
  | 0x02 -> BINARY_FRAME
  | 0x03 -> RESERVED_FRAME
  | 0x04 -> RESERVED_FRAME
  | 0x05 -> RESERVED_FRAME
  | 0x06 -> RESERVED_FRAME
  | 0x07 -> RESERVED_FRAME
  | 0x08 -> CLOSE_FRAME
  | 0x09 -> PING_FRAME
  | 0x0A -> PONG_FRAME
  | 0x0B -> RESERVED_CONTROL_FRAME
  | 0x0C -> RESERVED_CONTROL_FRAME
  | 0x0D -> RESERVED_CONTROL_FRAME
  | 0x0E -> RESERVED_CONTROL_FRAME
  | 0x0F -> RESERVED_CONTROL_FRAME
  | opcode -> failwith @@ spf "invalid opcode(%x)" opcode
;;

let string_of_opcode = function
  | CONT_FRAME -> "CONT_FRAME"
  | TEXT_FRAME -> "TEXT_FRAME"
  | BINARY_FRAME -> "BINARY_FRAME"
  | RESERVED_FRAME -> "RESERVED_FRAME"
  | CLOSE_FRAME -> "CLOSE_FRAME"
  | PING_FRAME -> "PING_FRAME"
  | PONG_FRAME -> "PONG_FRAME"
  | RESERVED_CONTROL_FRAME -> "RESERVED_CONTROL_FRAME"
;;

let string_of_bytes stream n =
  Lwt_stream.nget n stream >>= fun chars ->
  return @@ ExtString.String.implode chars
;;

let bitstring_of_bytes stream n =
  string_of_bytes stream n >>= fun str ->
  return @@ Bitstring.bitstring_of_string str
;;

let get_mask_index ?(mask_len=4) big_index =
  mod_big_int big_index (big_int_of_int mask_len)
  +> int_of_big_int
;;

let gen_mask len =
  Array.init len (fun _ -> Random.int 256)
;;

let apply_mask mask4 c1 i4 =
  match mask4 with
    | [||] -> c1
    | _ -> char_of_int @@ (int_of_char c1) lxor mask4.(i4)
;;

let pack_payload_len ?(mask=false) len =
  let buff = Buffer.create 8 in
  let set_mask_flag len =
    if mask then
      len lor 0x80
    else len in
  if len < 126 then
    Buffer.add_char buff @@ char_of_int (set_mask_flag len)
  else if len >= 126 && len < 65536 then (* 2^16 *)
    begin
      Buffer.add_char buff @@ char_of_int (set_mask_flag 126);
      List.iter (Buffer.add_char buff) @@ be_of_le len ~nbyte:2
    end
  else if len < max_int then
    begin
      Buffer.add_char buff @@ char_of_int (set_mask_flag 127);
      List.iter (Buffer.add_char buff) @@ be_of_le len ~nbyte:8
    end
  else
    failwith "Not supported(too long text frame)"; (* TODO *)
  Buffer.contents buff
;;

let pack_status_code status_code =
  let buff = Buffer.create 2 in
  let byte2 = be_of_le status_code ~nbyte:2 in
  List.iter (Buffer.add_char buff) byte2;
  Buffer.contents buff
;;

let pack_mask mask4 =
  let buff = Buffer.create 4 in
  Buffer.add_char buff @@ char_of_int mask4.(0);
  Buffer.add_char buff @@ char_of_int mask4.(1);
  Buffer.add_char buff @@ char_of_int mask4.(2);
  Buffer.add_char buff @@ char_of_int mask4.(3);
  Buffer.contents buff
;;

let pack_masked_payload_text mask4 text len =
  let buff = Buffer.create len in
  for i = 0 to len - 1 do
    Buffer.add_char buff @@ apply_mask mask4 text.[i] (i mod 4)
  done;
  Buffer.contents buff
;;

let pack_frame ?(mask=false) ?(status_code=None) ?(payload_text="") header_byte =
  let buff = Buffer.create 20 in
  let payload_text_len = String.length payload_text in
  let payload_len = if status_code = None then payload_text_len else payload_text_len + 2 in
  let mask4 = gen_mask 4 in

  (* header byte(1byte) *)
  Buffer.add_char buff @@ char_of_int header_byte;

  (* payload_len(7bit .. 7bit + 8byte) *)
  Buffer.add_string buff @@ pack_payload_len payload_len ~mask; (* payload_len *)

  (* mask(4byte if any) *)
  if mask then
    Buffer.add_string buff @@ pack_mask mask4
  ;

  (* status_code(2 byte if any) *)
  (match status_code with
    | Some code ->
      List.iter (Buffer.add_char buff) @@ be_of_le code ~nbyte:2
    | _ -> ());

  (* payload text *)
  (match mask with
    | true ->
      let payload_text' = pack_masked_payload_text mask4 payload_text payload_text_len in
      Buffer.add_string buff payload_text'
    | false ->
      Buffer.add_string buff payload_text);

  Buffer.contents buff
;;

let pack_ping_frame ?(payload_text="ping") () =
  pack_frame 0x89 ~payload_text:"ping"
;;
  
let pack_pong_frame ?(payload_text="pong") () =
  pack_frame 0x8A ~payload_text
;;

(** caution: if status_code is not specified, payload_text is ignored *)
let pack_close_frame ?(mask=false) ?(status_code=None) ?(payload_text="") () =
  let header_byte = 0x88 in
  match status_code with
    | None -> pack_frame header_byte ~payload_text:""
    | _ -> pack_frame header_byte ~mask ~payload_text ~status_code
;;

let pack_text_frame ?(mask=false) payload_text =
  pack_frame 0x81 ~mask ~payload_text
;;

let parse_real_payload_len payload_len stream =
  match payload_len with
    | 126 ->
      bitstring_of_bytes stream 2 >>= fun bits ->
      (bitmatch bits with
	| {len2 : 16 : bigendian} -> return @@ big_int_of_int len2
	| { _ } -> return zero_big_int)

    | 127 ->
      bitstring_of_bytes stream 8 >>= fun bits ->
      (bitmatch bits with
	| {len8 : 64 : bigendian} -> return @@ big_int_of_int64 len8
	| { _ } -> return zero_big_int)
    | _ -> return @@ big_int_of_int payload_len
;;

let parse_payload_mask stream =
  bitstring_of_bytes stream 4 >>= fun bits ->
  bitmatch bits with
    | { mask4 : 32 : string } ->
      return (ExtString.String.explode mask4 +> List.map int_of_char +> Array.of_list)

    | { _ } ->  return [||]
;;

let parse_payload_text ?(payload_mask=[||]) real_len stream =
  let buff = Buffer.create 128 in
  let rec iter big_i =
    if lt_big_int big_i real_len then
      begin
	Lwt_stream.next stream >>= fun c1 ->
	(match payload_mask with
	  | [||] -> Buffer.add_char buff c1
	  | mask -> Buffer.add_char buff (apply_mask mask c1 (get_mask_index big_i)));
	iter (add_big_int big_i unit_big_int)
      end
    else
      return @@ Buffer.contents buff in
  iter zero_big_int
;;

let parse_status_code stream =
  bitstring_of_bytes stream 2 >>= fun bits ->
  bitmatch bits with
    | {status_code : 16 : bigendian} -> return status_code
    | { _ } -> return 0
;;

let unpack stream =
  bitstring_of_bytes stream 2 >>= fun bits ->
  bitmatch bits with
    | { fin : 1; rsv1 : 1; rsv2 : 1; rsv3 : 1; opcode_int : 4; is_mask : 1; payload_len : 7 } ->
      let opcode = opcode_of_int opcode_int in
      parse_real_payload_len payload_len stream >>= fun real_payload_len ->
      (if is_mask then
	  parse_payload_mask stream
       else return [||]) >>= fun payload_mask ->

      (if opcode = CLOSE_FRAME && payload_len >= 2 then
	  parse_status_code stream
       else return 0) >>= fun status_code ->

      (match opcode, payload_len with
	| _, payload_len when payload_len = 0 ->
	  return ""

	| CLOSE_FRAME, payload_len when payload_len <= 2 ->
	  return ""

	| CLOSE_FRAME, _ ->
	  let real_payload_len' = Big_int.sub_big_int real_payload_len (big_int_of_int 2) in
	  parse_payload_text real_payload_len' stream ~payload_mask
	| _, _ ->
	  parse_payload_text real_payload_len stream ~payload_mask) >>= fun payload_text ->

      (match opcode with
	| TEXT_FRAME ->
	  return @@ TextFrame(payload_text)

	| PING_FRAME ->
	  return @@ PingFrame(payload_text)

	| PONG_FRAME ->
	  return @@ PongFrame(payload_text)

	| CLOSE_FRAME ->
	  return @@ CloseFrame(status_code, payload_text)

	| _ ->
	  let emsg = spf "undefined opcode %d" opcode_int in
	  return @@ UndefinedFrame(emsg))

    | { _ } -> return @@ UndefinedFrame("invalid header")
;;

