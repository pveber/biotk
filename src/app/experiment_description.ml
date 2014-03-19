open Sexplib.Std

type t = statement list
and statement =
| Condition of condition
| Sample of sample
| Model of model
and condition = string
and sample = {
  sample_id : string ;
  sample_type : sample_type ;
  sample_exp : experiment ;
  sample_files : string list ;
  sample_model : string ;
  sample_condition : string ;
}
and sample_type = [
| `short_reads of short_read_format
]
and experiment = [
| `whole_cell_extract
| `TF_ChIP of string
| `FAIRE
| `mRNA
]
and short_read_format = [
| `fastq of [ `sanger | `solexa | `phred64 ]
| `sra
]
and model = {
  model_id : string ;
  model_genome : genome ;
}
and genome = [`mm9]
with sexp

let load path =
  Sexplib.Sexp.load_sexp_conv_exn path t_of_sexp

let save cfg path =
  Sexplib.Sexp.save_hum path (sexp_of_t cfg)
