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
      "(" ^ env_to_string env ^ ")[fun " ^ x ^ " -> " ^ Exp.to_string e0 ^ "]"
  | Rec (env, x, y, e0) ->
      "(" ^ env_to_string env ^ ")[rec " ^ x ^ " = fun " ^ y ^ " -> " ^ Exp.to_string e0 ^ "]"
  | Nil ->
      "[]"
  | Cons (e1, e2) ->
      "(" ^ to_string e1 ^ ") :: (" ^ to_string e2 ^ ")"
and env_to_string = function
  | [] -> ""
  | (x0, v0) :: env ->
      List.fold_left (fun acc (x, v) -> x ^ " = " ^ to_string v ^ ", " ^ acc) "" env ^ x0 ^ " = " ^ to_string v0
