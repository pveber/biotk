let p = 
  value pipeline "mon.pipeline.2.0"
    ~(x:int) ?(y:string = "1") z:string 
    uses foo bar* baz -> (
      1 + 1
    )

let f value pipeline = 0
