type t =
  | EInt of eval_desc
  | EBool of eval_desc
  | EVar of eval_desc
  | EPlus of eval_desc * t * t * t
  | EMinus of eval_desc * t * t * t
  | ETimes of eval_desc * t * t * t
  | ELt of eval_desc * t * t * t
  | EIfT of eval_desc * t * t
  | EIfF of eval_desc * t * t
  | ELet of eval_desc * t * t
  | EFun of eval_desc
  | EApp of eval_desc * t * t * t
  | ELetRec of eval_desc * t
  | EAppRec of eval_desc * t * t * t
  | ENil of eval_desc
  | ECons of eval_desc * t * t
  | EMatchNil of eval_desc * t * t
  | EMatchCons of eval_desc * t * t
  | BPlus of binop_desc
  | BMinus of binop_desc
  | BTimes of binop_desc
  | BLt of binop_desc
(* 判断のうちenv |- exp evalto valueまでの記述を表す *)
and eval_desc = { env : Value.env; exp : Exp.t; value : Value.t }
(* 整数演算に関する判断のうち頻出する記述を表す *)
and binop_desc = { lsrc : Value.t; rsrc : Value.t; dst : Value.t }

(* val string_of_eval_desc : eval_desc -> string *)
let string_of_eval_desc { env = env; exp = e; value = v} =
  Value.env_to_string env ^ " |- " ^ Exp.to_string e ^ " evalto " ^ Value.to_string v ^ " by "

(*
 * val to_string_aux : int -> int -> t -> string
 * 指定された幅でインデントしながら導出を文字列で表現する
 *)
let rec to_string_aux indent depth =
  let to_string_aux_with_indent d = to_string_aux indent (depth + indent) d in
  let make_indent depth = String.make depth ' ' in function
  | EInt (desc) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-Int {}"
  | EBool (desc) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-Bool {}"
  | EVar (desc) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-Var {}"
  | EPlus (desc, d1, d2, d3) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-Plus {\n" ^
      to_string_aux_with_indent d1 ^ ";\n" ^
      to_string_aux_with_indent d2 ^ ";\n" ^
      to_string_aux_with_indent d3 ^ "\n" ^
      make_indent depth ^ "}"
  | EMinus (desc, d1, d2, d3) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-Minus {\n" ^
      to_string_aux_with_indent d1 ^ ";\n" ^
      to_string_aux_with_indent d2 ^ ";\n" ^
      to_string_aux_with_indent d3 ^ "\n" ^
      make_indent depth ^ "}"
  | ETimes (desc, d1, d2, d3) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-Times {\n" ^
      to_string_aux_with_indent d1 ^ ";\n" ^
      to_string_aux_with_indent d2 ^ ";\n" ^
      to_string_aux_with_indent d3 ^ "\n" ^
      make_indent depth ^ "}"
  | ELt (desc, d1, d2, d3) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-Lt {\n" ^
      to_string_aux_with_indent d1 ^ ";\n" ^
      to_string_aux_with_indent d2 ^ ";\n" ^
      to_string_aux_with_indent d3 ^ "\n" ^
      make_indent depth ^ "}"
  | EIfT (desc, d1, d2) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-IfT {\n" ^
      to_string_aux_with_indent d1 ^ ";\n" ^
      to_string_aux_with_indent d2 ^ "\n" ^
      make_indent depth ^ "}"
  | EIfF (desc, d1, d2) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-IfF {\n" ^
      to_string_aux_with_indent d1 ^ ";\n" ^
      to_string_aux_with_indent d2 ^ "\n" ^
      make_indent depth ^ "}"
  | ELet (desc, d1, d2) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-Let {\n" ^
      to_string_aux_with_indent d1 ^ ";\n" ^
      to_string_aux_with_indent d2 ^ "\n" ^
      make_indent depth ^ "}"
  | EFun (desc) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-Fun {}"
  | EApp (desc, d1, d2, d3) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-App {\n" ^
      to_string_aux_with_indent d1 ^ ";\n" ^
      to_string_aux_with_indent d2 ^ ";\n" ^
      to_string_aux_with_indent d3 ^ "\n" ^
      make_indent depth ^ "}"
  | ELetRec (desc, d) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-LetRec {\n" ^
      to_string_aux_with_indent d ^ "\n" ^
      make_indent depth ^ "}"
  | EAppRec (desc, d1, d2, d3) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-AppRec {\n" ^
      to_string_aux_with_indent d1 ^ ";\n" ^
      to_string_aux_with_indent d2 ^ ";\n" ^
      to_string_aux_with_indent d3 ^ "\n" ^
      make_indent depth ^ "}"
  | ENil (desc) -> 
      make_indent depth ^ string_of_eval_desc desc ^ "E-Nil {}"
  | ECons (desc, d1, d2) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-Cons {\n" ^
      to_string_aux_with_indent d1 ^ ";\n" ^
      to_string_aux_with_indent d2 ^ "\n" ^
      make_indent depth ^ "}"
  | EMatchNil (desc, d1, d2) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-MatchNil {\n" ^
      to_string_aux_with_indent d1 ^ ";\n" ^
      to_string_aux_with_indent d2 ^ "\n" ^
      make_indent depth ^ "}"
  | EMatchCons (desc, d1, d2) ->
      make_indent depth ^ string_of_eval_desc desc ^ "E-MatchCons {\n" ^
      to_string_aux_with_indent d1 ^ ";\n" ^
      to_string_aux_with_indent d2 ^ "\n" ^
      make_indent depth ^ "}"
  | BPlus { lsrc = lsrc; rsrc = rsrc; dst = dst } ->
      make_indent depth ^ Value.to_string lsrc ^ " plus " ^ Value.to_string rsrc ^ " is " ^ Value.to_string dst ^ " by B-Plus {}"
  | BMinus { lsrc = lsrc; rsrc = rsrc; dst = dst } ->
      make_indent depth ^ Value.to_string lsrc ^ " minus " ^ Value.to_string rsrc ^ " is " ^ Value.to_string dst ^ " by B-Minus {}"
  | BTimes { lsrc = lsrc; rsrc = rsrc; dst = dst } ->
      make_indent depth ^ Value.to_string lsrc ^ " times " ^ Value.to_string rsrc ^ " is " ^ Value.to_string dst ^ " by B-Times {}"
  | BLt { lsrc = lsrc; rsrc = rsrc; dst = dst } ->
      make_indent depth ^ Value.to_string lsrc ^ " less than " ^ Value.to_string rsrc ^ " is " ^ Value.to_string dst ^ " by B-Lt {}"

let to_string = to_string_aux 2 0

(*
 * val eval_and_deriv : Value.env -> Exp.t -> t * Value.t
 * 与えられた式を評価しつつ導出を残す
 *)
let rec eval_and_deriv env exp =
  let make_eval_desc v = { env = env; exp = exp; value = v } in
  match exp with
  | Exp.Int (i) -> 
      let v = Value.Int (i) in
      (EInt (make_eval_desc v), v)
  | Exp.Bool (b) ->
      let v = Value.Bool (b) in
      (EBool (make_eval_desc v), v)
  | Exp.Var (x) ->
      let v = List.assoc x env in
      (EVar (make_eval_desc v), v)
  | Exp.BinOp (e1, op, e2) ->
      let (d1, v1) = eval_and_deriv env e1 in
      let (d2, v2) = eval_and_deriv env e2 in
      begin match v1, op, v2 with
      | Value.Int (i1), Prim.Plus, Value.Int (i2) ->
          let v = Value.Int (i1 + i2) in
          let d3 = BPlus { lsrc = v1; rsrc = v2; dst = v} in
          (EPlus (make_eval_desc v, d1, d2, d3), v)
      | Value.Int (i1), Prim.Minus, Value.Int (i2) ->
          let v = Value.Int (i1 - i2) in
          let d3 = BMinus { lsrc = v1; rsrc = v2; dst = v} in
          (EMinus (make_eval_desc v, d1, d2, d3), v)
      | Value.Int (i1), Prim.Times, Value.Int (i2) ->
          let v = Value.Int (i1 * i2) in
          let d3 = BTimes { lsrc = v1; rsrc = v2; dst = v} in
          (ETimes (make_eval_desc v, d1, d2, d3), v)
      | Value.Int (i1), Prim.Lt, Value.Int (i2) ->
          let v = Value.Bool (i1 < i2) in
          let d3 = BLt { lsrc = v1; rsrc = v2; dst = v} in
          (ELt (make_eval_desc v, d1, d2, d3), v)
      end
  | Exp.If (e1, e2, e3) ->
      let (d1, v1) = eval_and_deriv env e1 in
      begin match v1 with
      | Value.Bool (true) ->
          let (d2, v) = eval_and_deriv env e2 in
          (EIfT (make_eval_desc v, d1, d2), v)
      | Value.Bool (false) ->
          let (d3, v) = eval_and_deriv env e3 in
          (EIfF (make_eval_desc v, d1, d3), v)
      end
  | Exp.Let (x, e1, e2) ->
      let (d1, v1) = eval_and_deriv env e1 in
      let (d2, v) = eval_and_deriv ((x, v1) :: env) e2 in
      (ELet (make_eval_desc v, d1, d2), v)
  | Exp.Fun (x, e) ->
      let v = Value.Fun (env, x, e) in
      (EFun (make_eval_desc v), v)
  | Exp.App (e1, e2) ->
      let (d1, v1) = eval_and_deriv env e1 in
      let (d2, v2) = eval_and_deriv env e2 in
      begin match v1 with
      | Value.Fun (env2, x, e0) ->
          let (d3, v) = eval_and_deriv ((x, v2) :: env2) e0 in
          (EApp (make_eval_desc v, d1, d2, d3), v)
      | Value.Rec (env2, x, y, e0) as v1 ->
          let (d3, v) = eval_and_deriv ((y, v2) :: (x, v1) :: env2) e0 in
          (EAppRec (make_eval_desc v, d1, d2, d3), v)
      end
  | Exp.LetRec (x, y, e1, e2) ->
      let (d1, v) = eval_and_deriv ((x, Value.Rec (env, x, y, e1)) :: env) e2 in
      (ELetRec (make_eval_desc v, d1), v)
  | Exp.Nil ->
      (ENil (make_eval_desc Value.Nil ), Value.Nil)
  | Exp.Cons (e1, e2) ->
      let (d1, v1) = eval_and_deriv env e1 in
      let (d2, v2) = eval_and_deriv env e2 in
      let v = Value.Cons (v1, v2) in
      (ECons (make_eval_desc v, d1, d2), v)
  | Exp.Match (e1, e2, x, y, e3) ->
      let (d1, v1) = eval_and_deriv env e1 in
      begin match v1 with
      | Value.Nil ->
          let (d2, v) = eval_and_deriv env e2 in
          (EMatchNil (make_eval_desc v, d1, d2), v)
      | Value.Cons (v1, v2) ->
          let (d3, v) = eval_and_deriv ((y, v2) :: (x, v1) :: env) e3 in
          (EMatchCons (make_eval_desc v, d1, d3), v)
      end

(* F#とかでも見る関数合成的なやつ *)
let ( >> ) f g x = g (f x)
let of_Exp = eval_and_deriv [] >> fst
