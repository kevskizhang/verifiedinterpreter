open Base
open QCheck
(* open Alcotest *)
module P = Vm_ocaml.Prog
module V = Vm_ocaml.Value
module Spec = Vm_ocaml.Spec_interp
module Vm = Vm_ocaml.Vm
module Ver = Vm_ocaml.Verifier
module G = Prog_gen

let prop_vm_equals_spec =
  Test.make ~name:"vm == spec (Iconst/Add/Halt)" ~count:1000
    G.arb_prog
    (fun prog ->
       match Ver.verify prog with
       | Error _ -> false
       | Ok () ->
         let r1 = Spec.run prog ~gas:1000 in
         let r2 = Vm.run   prog ~gas:1000 in
         Poly.equal r1 r2)

let () =
  let tests = [ QCheck_alcotest.to_alcotest prop_vm_equals_spec ] in
  Alcotest.run "vm-properties" [ ("props", tests) ]
