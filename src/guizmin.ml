include GzmCore
module Utils = GzmUtils

type 'a value

let load_value 
  : 'a value file_path -> 'a 
  = function (File fn) -> GzmUtils.load fn















