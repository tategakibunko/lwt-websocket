open OUnit
open Lwt_websocket.Utils

let debug_bytes bytes =
  List.map (fun b -> spf "%02x" b) bytes
  +> String.concat "-"
  +> print_endline
;;

let test_be_of_le () =
  let test nbyte value expect =
    let bytes = be_of_le value ~nbyte +> List.map int_of_char in
    if bytes <> expect then
      debug_bytes bytes
    ;
    assert_equal bytes expect in
  test 2 0xAB   [0x00; 0xAB];
  test 2 0xABCD [0xAB; 0xCD];
  test 4 0xABCD [0x00; 0x00; 0xAB; 0xCD];
;;

let suite = "Wsock.Utils" >::: [
  "test_be_of_le" >:: test_be_of_le;
]
;;


