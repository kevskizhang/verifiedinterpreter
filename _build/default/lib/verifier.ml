open Base
module P = Prog
module I = Instr

type error = string
let verify (p:P.t) : (unit, error) Result.t =
  Array.iter p.funs ~f:(fun f ->
    if Array.is_empty f.code then
      raise (Failure "empty function");
    if not (phys_equal f.code.(Array.length f.code - 1) I.Halt) then
      raise (Failure "function must end with Halt"));
  Ok ()
