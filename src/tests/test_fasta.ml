open OUnit
open Gzt
open Core.Std
open CFStream

let assert_contents seq_printer label reference value =
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
         assert_equal ~msg:(msg "sequence") ~printer:seq_printer it.Fasta.sequence it'.Fasta.sequence
       | Error (_,lno), Error (_,lno') ->
         assert_equal ~msg:(msg "error line number") ~printer:Int.to_string lno lno')

let file_contents_gen g fn =
  In_channel.with_file fn ~f:(fun ic ->
      Stream.to_list (g ic)
    )

let file_contents = file_contents_gen Fasta.read
let file_contents_ints = file_contents_gen Fasta.read_ints

let ints_printer l = "[" ^ (String.concat ~sep:"; " (List.map l Int.to_string)) ^ "]"

let test_parsing_01 () =
  let observed = file_contents "src/tests/data/fasta_01.fa" in
  let expected = [
    Result.Ok { Fasta.header = "sequence 1" ; sequence = "ATATGCGATGACGATGCGCGATG" } ;
    Error ("",6)
  ]
  in
  assert_contents ident "fasta_01" expected observed

let test_parsing_02 () =
  let observed = file_contents_ints "src/tests/data/fasta_02.fa" in
  let expected = [
    Result.Ok { Fasta.header = "read 01" ; sequence = [ 12 ; 34 ; 1 ; 4 ; 5 ; 4 ; 5 ; 6 ] } ;
    Error ("",6)
  ]
  in
  assert_contents ints_printer "fasta_02" expected observed

let test_parsing () =
  test_parsing_01 () ;
  test_parsing_02 ()

let tests = "Fasta" >::: [
  "Parsing" >:: test_parsing ;
]
