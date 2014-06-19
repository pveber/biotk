open OUnit
open Gzt
open Core.Std
open CFStream

let assert_contents label reference value =
  assert_equal
    ~msg:(label ^ ": different number of items")
    ~printer:Int.to_string
    (List.length reference)
    (List.length value) ;
  List.iteri
    (List.zip_exn reference value)
    (fun i (r, v) ->
       let msg s = sprintf "%s:%d: %s" label i s in
       match r, v with
       | Result.Ok _, Result.Error _ -> assert_failure (msg "Expected item, got error")
       | Error _, Ok _ -> assert_failure (msg "Expected error, got item")
       | Ok it, Ok it' ->
         assert_equal ~msg:(msg "header") ~printer:ident it.Fasta.header it'.Fasta.header ;
         assert_equal ~msg:(msg "sequence") ~printer:ident it.Fasta.sequence it'.Fasta.sequence
       | Error (_,lno), Error (_,lno') ->
         assert_equal ~msg:(msg "error line number") ~printer:Int.to_string lno lno')

let file_contents fn =
  In_channel.with_file fn ~f:(fun ic ->
      Stream.to_list (Fasta.read ic)
    )

let test_parsing_01 () =
  let observed = file_contents "src/tests/data/fasta_01.fa" in
  let expected = [
    Result.Ok { Fasta.header = "sequence 1" ; sequence = "ATATGCGATGACGATGCGCGATG" } ;
    Error ("",6)
  ]
  in
  assert_contents "fasta_01" expected observed

let test_parsing () =
  test_parsing_01 ()

let tests = "Fasta" >::: [
  "Parsing" >:: test_parsing ;
]
