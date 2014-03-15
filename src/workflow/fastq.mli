open Types

type _ format =
| Sanger : [`sanger] format
| Solexa : [`solexa] format
| Phred64 : [`phred64] format

val sanger : [`sanger] format
val solexa : [`solexa] format
val phred64 : [`phred64] format

type 'a workflow = 'a format file Bistro_workflow.t
