type t = private char
val card : int
val to_char : t -> char
val of_char : char -> t option
val of_char_exn : char -> t
val to_int : t -> int
