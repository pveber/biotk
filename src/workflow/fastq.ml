open Types

type _ format =
| Sanger : [`sanger] format
| Solexa : [`solexa] format
| Phred64 : [`phred64] format

let sanger = Sanger
let solexa = Solexa
let phred64 = Phred64


type 'a workflow = 'a format file Bistro_workflow.t
