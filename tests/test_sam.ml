open Biotk

let just_parse fn () =
  Sam.with_file_exn fn ~f:(fun _ als -> Seq.iter ignore als)


let tests =
  let open Alcotest in
  [
    test_case "parse ex2.sam" `Quick (just_parse "../data/ex2.sam") ;
  ]
