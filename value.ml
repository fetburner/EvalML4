type t =
  | Int of int
  | Bool of bool
  | Fun of env * string * Exp.t
  | Rec of env * string * string * Exp.t
  | Nil
  | Cons of t * t
and env = (string * t) list

let rec to_string = function
  | Int (i) ->
      string_of_int i
  | Bool (b) ->
      string_of_bool b
  | Fun (env, x, e0) ->
      "(" ^ string_of_env env ^ ")[fun " ^ x ^ " -> " ^ Exp.to_string e0 ^ "]"
  | Rec (env, x, y, e0) ->
      "(" ^ string_of_env env ^ ")[rec " ^ x ^ " = fun " ^ y ^ " -> " ^ Exp.to_string e0 ^ "]"
  | Nil ->
      "[]"
  | Cons (e1, e2) ->
      "(" ^ to_string e1 ^ ") :: (" ^ to_string e2 ^ ")"
and string_of_env = function
  | [] -> ""
  | (x0, v0) :: env ->
      List.fold_left (fun acc (x, v) -> x ^ " = " ^ to_string v ^ ", " ^ acc) "" env ^ x0 ^ " = " ^ to_string v0

let rec eval env = function
  | Exp.Int (i) ->
      (* E-Int *)
      Int (i)
  | Exp.Bool (b) ->
      (* E-Int *)
      Bool (b)
  | Exp.Var (x) ->
      (* E-Var *)
      List.assoc x env
  | Exp.Plus (e1, e2) ->
      (* E-Plus, B-Plus *)
      begin match eval env e1, eval env e2 with
      | Int (i1), Int (i2) -> Int (i1 + i2)
      end
  | Exp.Minus (e1, e2) ->
      (* E-Minus, B-Minus *)
      begin match eval env e1, eval env e2 with
      | Int (i1), Int (i2) -> Int (i1 - i2)
      end
  | Exp.Times (e1, e2) ->
      (* E-Times, B-Times *)
      begin match eval env e1, eval env e2 with
      | Int (i1), Int (i2) -> Int (i1 * i2)
      end
  | Exp.Lt (e1, e2) ->
      (* E-Lt, B-Lt *)
      begin match eval env e1, eval env e2 with
      | Int (i1), Int (i2) -> Bool (i1 < i2)
      end
  | Exp.If (e1, e2, e3) ->
      begin match eval env e1 with
      | Bool (true) ->
          (* E-IfT *)
          eval env e2
      | Bool (false) ->
          (* E-IfF *)
          eval env e3
      end
  | Exp.Let (x, e1, e2) ->
      (* E-Let *)
      let v1 = eval env e1 in
      eval ((x, v1) :: env) e2
  | Exp.Fun (x, e) ->
      Fun (env, x, e)
  | Exp.App (e1, e2) ->
      let v2 = eval env e2 in
      begin match eval env e1 with
      | Fun (env2, x, e0) ->
          (* E-App *)
          eval ((x, v2) :: env2) e0
      | Rec (env2, x, y, e0) as v1 ->
          (* E-AppRec *)
          eval ((y, v2) :: (x, v1) :: env2) e0
      end
  | Exp.LetRec (x, y, e1, e2) ->
      (* E-LetRec *)
      eval ((x, Rec (env, x, y, e1)) :: env) e2
  | Exp.Nil ->
      (* E-Nil *)
      Nil
  | Exp.Cons (e1, e2) ->
      (* E-Cons *)
      Cons (eval env e1, eval env e2)
  | Exp.Match (e1, e2, x, y, e3) ->
      begin match eval env e1 with
      | Nil ->
          (* E-MatchNil *)
          eval env e2
      | Cons (v1, v2) ->
          (* E-MatchCons *)
          eval ((y, v2) :: (x, v1) :: env) e3
      end

let rec deriv env e = 
  let v = eval env e in
  string_of_env env ^ " |- " ^ Exp.to_string e ^ " evalto " ^ to_string v ^ " by " ^ 
  match e with
  | Exp.Int (i) ->
      (* E-Int *)
      "E-Int {}"
  | Exp.Bool (b) ->
      (* E-Int *)
      "E-Bool {}"
  | Exp.Var (x) ->
      (* E-Var *)
      "E-Var {}"
  | Exp.Plus (e1, e2) ->
      (* E-Plus *)
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      "E-Plus {\n" ^ 
      deriv env e1 ^ "; \n" ^ 
      deriv env e2 ^ "; \n" ^ 
      to_string v1 ^ " plus " ^ to_string v2 ^ " is " ^ to_string v ^ " by B-Plus {}\n" ^
      "}"
  | Exp.Minus (e1, e2) ->
      (* E-Minus *)
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      "E-Minus {\n" ^ 
      deriv env e1 ^ "; \n" ^ 
      deriv env e2 ^ "; \n" ^ 
      to_string v1 ^ " minus " ^ to_string v2 ^ " is " ^ to_string v ^ " by B-Minus {}\n" ^
      "}"
  | Exp.Times (e1, e2) ->
      (* E-Times *)
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      "E-Times {\n" ^ 
      deriv env e1 ^ "; \n" ^ 
      deriv env e2 ^ "; \n" ^ 
      to_string v1 ^ " times " ^ to_string v2 ^ " is " ^ to_string v ^ " by B-Times {}\n" ^
      "}"
  | Exp.Lt (e1, e2) ->
      (* E-Lt *)
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      "E-Lt {\n" ^ 
      deriv env e1 ^ "; \n" ^ 
      deriv env e2 ^ "; \n" ^ 
      to_string v1 ^ " less than " ^ to_string v2 ^ " is " ^ to_string v ^ " by B-Lt {}\n" ^
      "}"
  | Exp.If (e1, e2, e3) ->
      begin match eval env e1 with
      | Bool (true) ->
          (* E-IfT *)
          "E-IfT {\n" ^
          deriv env e1 ^ "; \n" ^
          deriv env e2 ^ "\n" ^
          "}"
      | Bool (false) ->
          (* E-IfF *)
          "E-IfF {\n" ^
          deriv env e1 ^ "; \n" ^
          deriv env e3 ^ "\n" ^
          "}"
      end
  | Exp.Let (x, e1, e2) ->
      (* E-Let *)
      let v1 = eval env e1 in
      "E-Let {\n" ^
      deriv env e1 ^ ";\n" ^
      deriv ((x, v1) :: env) e2 ^ "\n" ^
      "}"
  | Exp.Fun (x, e) ->
      (* E-Fun *)
      "E-Fun {}"
  | Exp.App (e1, e2) ->
      let v2 = eval env e2 in
      begin match eval env e1 with
      | Fun (env2, x, e0) ->
          (* E-App *)
          "E-App {\n" ^
          deriv env e1 ^ ";\n" ^
          deriv env e2 ^ ";\n" ^
          deriv ((x, v2) :: env2) e0 ^ "\n" ^
          "}"
      | Rec (env2, x, y, e0) as v1 ->
          (* E-AppRec *)
          "E-AppRec {\n" ^
          deriv env e1 ^ ";\n" ^
          deriv env e2 ^ ";\n" ^
          deriv ((y, v2) :: (x, v1) :: env2) e0 ^ "\n" ^
          "}"
      end
  | Exp.LetRec (x, y, e1, e2) ->
      (* E-LetRec *)
      "E-LetRec {\n" ^ 
      deriv ((x, Rec (env, x, y, e1)) :: env) e2 ^ "\n" ^
      "}"
  | Exp.Nil ->
      (* E-Nil *)
      "E-Nil {}"
  | Exp.Cons (e1, e2) ->
      (* E-Cons *)
      "E-Cons {" ^
      deriv env e1 ^ ";\n" ^
      deriv env e2 ^ "\n" ^
      "}"
  | Exp.Match (e1, e2, x, y, e3) ->
      begin match eval env e1 with
      | Nil ->
          (* E-MatchNil *)
          "E-MatchNil {\n" ^
          deriv env e1 ^ ";\n" ^
          deriv env e2 ^ "\n" ^
          "}"
      | Cons (v1, v2) ->
          (* E-MatchCons *)
          "E-MatchCons {\n" ^
          deriv env e1 ^ ";\n" ^
          deriv ((y, v2) :: (x, v1) :: env) e3 ^ "\n" ^
          "}"
      end

let rec of_Exp e =
  print_endline (deriv [] e);
  eval [] e
