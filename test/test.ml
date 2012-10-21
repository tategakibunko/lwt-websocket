open OUnit

let suite = "Wsock" >::: [
  Test_utils.suite;
]
;;

let _ =
  run_test_tt suite ~verbose:true
;;
