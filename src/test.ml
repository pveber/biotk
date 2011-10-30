open GuizminAltCore

let p i s = v0 ("guizmin.test.1", [ `int i ; `string s ]) (fun () -> i = int_of_string s)

let p = 
  value pipeline "mon.pipeline.2.0"
    ~(x:int) ?(y:string = "1") z:string 
    uses foo bar* baz -> (
      1 + 1
    )

let f value pipeline = 0
