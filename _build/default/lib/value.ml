open Base
type t = I of int | B of bool [@@deriving equal, sexp_of]