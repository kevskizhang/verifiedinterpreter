open Base
module I = Instr

type func = {code : I.t array }

type t = {
  funs : func array;
  main : int; (* entry function index *)
}