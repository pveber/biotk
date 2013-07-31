include GzmCore

type 'a value

let load_value : 'a value file_path -> 'a = 
  function (File fn) -> GzmUtils.load fn

let ( |- ) f g x = g (f x)

module Console = GzmConsole




















