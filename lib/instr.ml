open Base
type t =
  | Iconst of int
  | Add
  | Halt
[@@deriving equal, sexp_of]