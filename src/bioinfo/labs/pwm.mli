(** sorted alignment scores *)
type score_distribution = private float array

val cdf : score_distribution -> float -> float
val quantile : score_distribution -> float -> float

open Guizmin
open Guizmin_bioinfo

val const : id:string -> Biocaml_pwm.t -> Biocaml_pwm.t pipeline
val best_score_distribution_of_fasta : Biocaml_pwm.t pipeline -> Fasta.file -> score_distribution pipeline
val markov0_control_set : Fasta.file -> Fasta.file

(** start, end, strand and score of the match *)
val prediction : ?level:float -> Biocaml_pwm.t pipeline -> Fasta.file -> (int * int * [`sense|`antisense] * float) list list pipeline




















