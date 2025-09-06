open Base
open QCheck
module I = Vm_ocaml.Instr
module P = Vm_ocaml.Prog

(* Generate programs consisting of Iconst and Add, ending with Halt.
   Invariant: stack will not underflow when executing Adds. *)
let gen_prog : P.t Gen.t =
  let open Gen in
  let* n_consts = int_range 1 8 in     (* at least 1 const so Halt has a value *)
  let* consts = list_repeat n_consts (int_range (-100) 100) in
  let* n_adds = int_range 0 (n_consts - 1) in
  (* Build instruction list: push all consts, then do n_adds adds, then halt *)
  let code =
    (List.map consts ~f:(fun k -> I.Iconst k))
    @ (List.init n_adds ~f:(fun _ -> I.Add))
    @ [I.Halt]
  in
  let func = { P.code = Array.of_list code } in
  Gen.return { P.funs = [| func |]; main = 0 }

let arb_prog =
  QCheck.make ~print:(fun _ -> "<prog>") gen_prog
