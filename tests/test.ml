let () =
  let open Alcotest in
  run "All tests" [
    "Sam", Test_sam.tests ;
  ]
