let rec read_eval_print_loop () =
  print_string "# ";
  flush stdout;
  let exp = Parser.toplevel Lexer.token (Lexing.from_channel stdin) in
  let d = Deriv.of_Exp exp in
  print_string (Deriv.to_string d);
  print_newline ();
  read_eval_print_loop ()

let _ = read_eval_print_loop ()
