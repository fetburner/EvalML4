(* ML4の式を表す型 *)
type t =
  | Int of int
  | Bool of bool
  | Var of string
  | BinOp of t * Prim.t * t
  | If of t * t * t
  | Let of string * t * t
  | Fun of string * t
  | App of t * t
  | LetRec of string * string * t * t
  | Nil
  | Cons of t * t
  | Match of t * t * string * string * t

(*
 * val to_string : t -> string
 * 式を文字列で表現する
 * 括弧の辺りは適当
 *)
let rec to_string = function
  | Int (i) ->
      string_of_int i
  | Bool (b) ->
      string_of_bool b
  | Var (x) ->
      x
  | BinOp (e1, op, e2) ->
      "(" ^ to_string e1 ^ ") " ^ Prim.to_string op ^ " (" ^ to_string e2 ^ ")"
  | If (e1, e2, e3) ->
      "if " ^ to_string e1 ^ " then " ^ to_string e2 ^ " else " ^ to_string e3
  | Let (x, e1, e2) ->
      "let " ^ x ^ " = " ^ to_string e1 ^ " in " ^ to_string e2
  | Fun (x, e) ->
      "fun " ^ x ^ " -> " ^ to_string e
  | App (e1, e2) ->
      "(" ^ to_string e1 ^ ") (" ^ to_string e2 ^ ")"
  | LetRec (x, y, e1, e2) ->
      "let rec " ^ x ^ " = fun " ^ y ^ " -> " ^ to_string e1 ^ " in " ^ to_string e2
  | Nil ->
      "[]"
  | Cons (e1, e2) ->
      "(" ^ to_string e1 ^ ") :: (" ^ to_string e2 ^ ")"
  | Match (e1, e2, x, y, e3) ->
      "match " ^ to_string e1 ^ " with [] -> " ^ to_string e2 ^ " | " ^ x ^ " :: " ^ y ^ " -> " ^ to_string e3
