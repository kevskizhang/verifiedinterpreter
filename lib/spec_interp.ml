open Base
module V = Value
module I = Instr
module P = Prog

type result =
  | Terminated of V.t
  | Trap of string
[@@deriving equal, sexp_of]

let run (p:P.t) ~(gas:int) : result =
  let f = p.funs.(p.main) in
  let code = f.code in
  let rec loop ~pc ~stack ~gas =
    if gas = 0 then Trap "out of gas" else
    if pc < 0 || pc >= Array.length code then Trap "pc out of bounds" else
    match code.(pc) with
    | I.Iconst n -> loop ~pc:(pc+1) ~stack:(V.I n :: stack) ~gas:(gas-1)
    | I.Add ->
        (match stack with
         | V.I b :: V.I a :: rest ->
             loop ~pc:(pc+1) ~stack:(V.I (a + b) :: rest) ~gas:(gas-1)
         | _ -> Trap "stack/type underflow on Add")
    | I.Halt ->
        (match stack with
         | v :: _ -> Terminated v
         | [] -> Trap "halt on empty stack")
  in
  loop ~pc:0 ~stack:[] ~gas
