open Base
module Spec = Spec_interp
module P = Prog
module V = Value

type result = Spec_interp.result = Terminated of V.t | Trap of string [@@deriving equal, sexp_of]

let run (p:P.t) ~(gas:int) : result =
  Spec.run p ~gas
