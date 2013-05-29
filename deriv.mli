(* 導出を表す型 *)
type t
(* 導出を文字列で表現する *)
val to_string : ?tab:string -> t -> string
(* 式を評価した時の導出 *)
val of_Exp : ?env:Value.env -> Exp.t -> t
