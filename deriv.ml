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
and eval_desc = { env : Value.env; exp : Exp.t; value : Value.t }
and binop_desc = { lsrc : Value.t; rsrc : Value.t; dst : Value.t }

let string_of_eval_desc { env = env; exp = e; value = v} =
  Value.env_to_string env ^ " |- " ^ Exp.to_string e ^ " evalto " ^ Value.to_string v ^ " by "

let rec to_string_aux indent depth = function
  | EInt (desc) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-Int {}"
  | EBool (desc) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-Bool {}"
  | EVar (desc) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-Var {}"
  | EPlus (desc, d1, d2, d3) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-Plus {\n" ^
      to_string_aux indent (depth + indent) d1 ^ ";\n" ^
      to_string_aux indent (depth + indent) d2 ^ ";\n" ^
      to_string_aux indent (depth + indent) d3 ^ "\n" ^
      String.make depth ' ' ^ "}"
  | EMinus (desc, d1, d2, d3) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-Minus {\n" ^
      to_string_aux indent (depth + indent) d1 ^ ";\n" ^
      to_string_aux indent (depth + indent) d2 ^ ";\n" ^
      to_string_aux indent (depth + indent) d3 ^ "\n" ^
      String.make depth ' ' ^ "}"
  | ETimes (desc, d1, d2, d3) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-Times {\n" ^
      to_string_aux indent (depth + indent) d1 ^ ";\n" ^
      to_string_aux indent (depth + indent) d2 ^ ";\n" ^
      to_string_aux indent (depth + indent) d3 ^ "\n" ^
      String.make depth ' ' ^ "}"
  | ELt (desc, d1, d2, d3) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-Lt {\n" ^
      to_string_aux indent (depth + indent) d1 ^ ";\n" ^
      to_string_aux indent (depth + indent) d2 ^ ";\n" ^
      to_string_aux indent (depth + indent) d3 ^ "\n" ^
      String.make depth ' ' ^ "}"
  | EIfT (desc, d1, d2) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-IfT {\n" ^
      to_string_aux indent (depth + indent) d1 ^ ";\n" ^
      to_string_aux indent (depth + indent) d2 ^ "\n" ^
      String.make depth ' ' ^ "}"
  | EIfF (desc, d1, d2) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-IfF {\n" ^
      to_string_aux indent (depth + indent) d1 ^ ";\n" ^
      to_string_aux indent (depth + indent) d2 ^ "\n" ^
      String.make depth ' ' ^ "}"
  | ELet (desc, d1, d2) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-Let {\n" ^
      to_string_aux indent (depth + indent) d1 ^ ";\n" ^
      to_string_aux indent (depth + indent) d2 ^ "\n" ^
      String.make depth ' ' ^ "}"
  | EFun (desc) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-Fun {}"
  | EApp (desc, d1, d2, d3) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-App {\n" ^
      to_string_aux indent (depth + indent) d1 ^ ";\n" ^
      to_string_aux indent (depth + indent) d2 ^ ";\n" ^
      to_string_aux indent (depth + indent) d3 ^ "\n" ^
      String.make depth ' ' ^ "}"
  | ELetRec (desc, d) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-LetRec {\n" ^
      to_string_aux indent (depth + indent) d ^ "\n" ^
      String.make depth ' ' ^ "}"
  | EAppRec (desc, d1, d2, d3) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-AppRec {\n" ^
      to_string_aux indent (depth + indent) d1 ^ ";\n" ^
      to_string_aux indent (depth + indent) d2 ^ ";\n" ^
      to_string_aux indent (depth + indent) d3 ^ "\n" ^
      String.make depth ' ' ^ "}"
  | ENil (desc) -> 
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-Nil {}"
  | ECons (desc, d1, d2) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-Cons {\n" ^
      to_string_aux indent (depth + indent) d1 ^ ";\n" ^
      to_string_aux indent (depth + indent) d2 ^ "\n" ^
      String.make depth ' ' ^ "}"
  | EMatchNil (desc, d1, d2) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-MatchNil {\n" ^
      to_string_aux indent (depth + indent) d1 ^ ";\n" ^
      to_string_aux indent (depth + indent) d2 ^ "\n" ^
      String.make depth ' ' ^ "}"
  | EMatchCons (desc, d1, d2) ->
      String.make depth ' ' ^ string_of_eval_desc desc ^ "E-MatchCons {\n" ^
      to_string_aux indent (depth + indent) d1 ^ ";\n" ^
      to_string_aux indent (depth + indent) d2 ^ "\n" ^
      String.make depth ' ' ^ "}"
  | BPlus (desc) ->
      String.make depth ' ' ^ Value.to_string desc.lsrc ^ " plus " ^ Value.to_string desc.rsrc ^ " is " ^ Value.to_string desc.dst ^ " by B-Plus {}"
  | BMinus (desc) ->
      String.make depth ' ' ^ Value.to_string desc.lsrc ^ " minus " ^ Value.to_string desc.rsrc ^ " is " ^ Value.to_string desc.dst ^ " by B-Minus {}"
  | BTimes (desc) ->
      String.make depth ' ' ^ Value.to_string desc.lsrc ^ " times " ^ Value.to_string desc.rsrc ^ " is " ^ Value.to_string desc.dst ^ " by B-Times {}"
  | BLt (desc) ->
      String.make depth ' ' ^ Value.to_string desc.lsrc ^ " less than " ^ Value.to_string desc.rsrc ^ " is " ^ Value.to_string desc.dst ^ " by B-Lt {}"

let to_string = to_string_aux 2 0

let rec eval_and_deriv env = function
  | Exp.Int (i) -> 
      let v = Value.Int (i) in
      (EInt { env = env; exp = Exp.Int (i); value = v }, v)
  | Exp.Bool (b) ->
      let v = Value.Bool (b) in
      (EBool { env = env; exp = Exp.Bool (b); value = v }, v)
  | Exp.Var (x) ->
      let v = List.assoc x env in
      (EVar { env = env; exp = Exp.Var (x); value = v }, v)
  | Exp.Plus (e1, e2) ->
      let (d1, v1) = eval_and_deriv env e1 in
      let (d2, v2) = eval_and_deriv env e2 in
      let v =
        match v1, v2 with
        | Value.Int (i1), Value.Int (i2) -> Value.Int (i1 + i2) in
      let d3 = BPlus { lsrc = v1; rsrc = v2; dst = v} in
      (EPlus ({ env = env; exp = Exp.Plus (e1, e2); value = v}, d1, d2, d3), v)
  | Exp.Minus (e1, e2) ->
      let (d1, v1) = eval_and_deriv env e1 in
      let (d2, v2) = eval_and_deriv env e2 in
      let v =
        match v1, v2 with
        | Value.Int (i1), Value.Int (i2) -> Value.Int (i1 - i2) in
      let d3 = BMinus { lsrc = v1; rsrc = v2; dst = v} in
      (EMinus({ env = env; exp = Exp.Minus (e1, e2); value = v}, d1, d2, d3), v)
  | Exp.Times (e1, e2) ->
      let (d1, v1) = eval_and_deriv env e1 in
      let (d2, v2) = eval_and_deriv env e2 in
      let v =
        match v1, v2 with
        | Value.Int (i1), Value.Int (i2) -> Value.Int (i1 * i2) in
      let d3 = BTimes { lsrc = v1; rsrc = v2; dst = v} in
      (ETimes ({ env = env; exp = Exp.Times (e1, e2); value = v}, d1, d2, d3), v)
  | Exp.Lt (e1, e2) ->
      let (d1, v1) = eval_and_deriv env e1 in
      let (d2, v2) = eval_and_deriv env e2 in
      let v =
        match v1, v2 with
        | Value.Int (i1), Value.Int (i2) -> Value.Bool (i1 < i2) in
      let d3 = BLt { lsrc = v1; rsrc = v2; dst = v} in
      (ELt ({ env = env; exp = Exp.Lt (e1, e2); value = v}, d1, d2, d3), v)
  | Exp.If (e1, e2, e3) ->
      let (d1, v1) = eval_and_deriv env e1 in
      begin match v1 with
      | Value.Bool (true) ->
          let (d2, v) = eval_and_deriv env e2 in
          (EIfT ({ env = env; exp = Exp.If (e1, e2, e3); value = v}, d1, d2), v)
      | Value.Bool (false) ->
          let (d3, v) = eval_and_deriv env e3 in
          (EIfT ({ env = env; exp = Exp.If (e1, e2, e3); value = v}, d1, d3), v)
      end
  | Exp.Let (x, e1, e2) ->
      let (d1, v1) = eval_and_deriv env e1 in
      let (d2, v) = eval_and_deriv ((x, v1) :: env) e2 in
      (ELet ({ env = env; exp = Exp.Let (x, e1, e2); value = v}, d1, d2), v)
  | Exp.Fun (x, e) ->
      let v = Value.Fun (env, x, e) in
      (EFun { env = env; exp = Exp.Fun (x, e); value = v}, v)
  | Exp.App (e1, e2) ->
      let (d1, v1) = eval_and_deriv env e1 in
      let (d2, v2) = eval_and_deriv env e2 in
      begin match v1 with
      | Value.Fun (env2, x, e0) ->
          let (d3, v) = eval_and_deriv ((x, v2) :: env2) e0 in
          (EApp ({ env = env; exp = Exp.App (e1, e2); value = v }, d1, d2, d3), v)
      | Value.Rec (env2, x, y, e0) as v1 ->
          let (d3, v) = eval_and_deriv ((y, v2) :: (x, v1) :: env2) e0 in
          (EAppRec ({ env = env; exp = Exp.App (e1, e2); value = v }, d1, d2, d3), v)
      end
  | Exp.LetRec (x, y, e1, e2) ->
      let (d1, v) = eval_and_deriv ((x, Value.Rec (env, x, y, e1)) :: env) e2 in
      (ELetRec ({ env = env; exp = Exp.LetRec (x, y, e1, e2); value = v }, d1), v)
  | Exp.Nil ->
      (ENil { env = env; exp = Exp.Nil; value = Value.Nil }, Value.Nil)
  | Exp.Cons (e1, e2) ->
      let (d1, v1) = eval_and_deriv env e1 in
      let (d2, v2) = eval_and_deriv env e2 in
      let v = Value.Cons (v1, v2) in
      (ECons ({ env = env; exp = Exp.Cons (e1, e2); value = v}, d1, d2), v)
  | Exp.Match (e1, e2, x, y, e3) ->
      let (d1, v1) = eval_and_deriv env e1 in
      begin match v1 with
      | Value.Nil ->
          let (d2, v) = eval_and_deriv env e2 in
          (EMatchNil ({ env = env; exp = Exp.Match (e1, e2, x, y, e3); value = v}, d1, d2), v)
      | Value.Cons (v1, v2) ->
          let (d3, v) = eval_and_deriv ((y, v2) :: (x, v1) :: env) e3 in
          (EMatchCons ({ env = env; exp = Exp.Match (e1, e2, x, y, e3); value = v}, d1, d3), v)
      end

let ( >> ) f g x = g (f x)
let of_Exp = eval_and_deriv [] >> fst
