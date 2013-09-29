open Guizmin

type report

let report fq =
  d1
    "guizmin.bioinfo.fastQC.report[r2]" []
    fq
    (
      fun env (File fq) path ->
	env.sh "mkdir -p %s" path ;
	env.sh "fastqc --outdir=%s %s" path fq ;
	env.sh "rm %s/*.zip" path ;
	env.sh "mv %s/*_fastqc/* %s" path path ;
	env.sh "rmdir %s/*_fastqc" path
    )

let html_report dir = select dir "fastqc_report.html"
