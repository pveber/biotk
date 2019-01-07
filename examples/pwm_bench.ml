open Core_bench
open Gzt

let () = Random.self_init ()

let s = Pwm_stats.random_sequence 1_000 (Pwm.flat_background ());;
let bg = Pwm.flat_background ();;
let pwm = Pwm.random ~len:14 bg;;
let theta = Pwm_stats.TFM_pvalue.score_of_pvalue pwm bg 1e-3;;
Bench.bench Bench.Test.[
    create ~name:"scan" (fun () -> Pwm.scan pwm s theta) ;
    create ~name:"opt_scan" (fun () -> Pwm.opt_scan pwm s theta) ;
    create ~name:"fast_scan" (fun () -> Pwm.fast_scan pwm s theta) ;
    create ~name:"opt_fast_scan" (fun () -> Pwm.opt_fast_scan pwm s theta) ;
  ];;
