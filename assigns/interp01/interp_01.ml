(* UTILITIES *)
let cons x xs = x :: xs
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_upper_case c = 'A' <= c && c <= 'Z'

type 'a parser = char list -> ('a * char list) option

let satisfy f = function
  | c :: cs when f c -> Some (c, cs)
  | _ -> None

let char c = satisfy ((=) c)

let str s cs =
  let rec go = function
    | [], ds -> Some (s, ds)
    | c :: cs, d :: ds when c = d -> go (cs, ds)
    | _ -> None
  in go (explode s, cs)

let map f p cs =
  match p cs with
  | Some (x, cs) -> Some (f x, cs)
  | None -> None

let (>|=) p f = map f p
let (>|) p x = map (fun _ -> x) p

let seq p1 p2 cs =
  match p1 cs with
  | Some (x, cs) -> (
      match p2 cs with
      | Some (y, cs) -> Some ((x, y), cs)
      | None -> None
    )
  | None -> None

let (<<) p1 p2 = map fst (seq p1 p2)
let (>>) p1 p2 = map snd (seq p1 p2)

let map2 f p1 p2 =
  seq p1 p2 >|= fun (x, y) -> f x y

let optional p cs =
  match p cs with
  | Some (x, cs) -> Some (Some x, cs)
  | None -> Some (None, cs)

let rec many p cs =
  match p cs with
  | Some (x, cs) -> (
      match (many p cs) with
      | Some (xs, cs) -> Some (x :: xs, cs)
      | None -> Some ([x], cs)
    )
  | None -> Some ([], cs)

let many1 p = map2 cons p (many p)

let alt p1 p2 cs =
  match p1 cs with
  | Some x -> Some x
  | None ->
    match p2 cs with
    | Some x -> Some x
    | None -> None

let (<|>) = alt

let pure x cs = Some (x, cs)
let fail _ = None

let bind p f cs =
  match p cs with
  | Some (x, cs) -> f x cs
  | None -> None

let (>>=) = bind

let choice ps =
  List.fold_left (<|>) fail ps

let ws = many (satisfy is_blank)
let keyword w = str w << ws

let rec_parser p =
  pure () >>= p

let parse p s =
  match p (explode s) with
  | Some (x, []) -> Some x
  | _ -> None

(* END OF UTILITIES *)

(* ============================================================ *)

(* BEGINNING OF PROJECT CODE *)

type ident = string
type command
  = Drop                   (* drop *)
  | Swap                   (* swap *)
  | Dup                    (* dup *)
  | Trace                  (* . *)
  | Add                    (* + *)
  | Sub                    (* - *)
  | Mul                    (* * *)
  | Div                    (* / *)
  | Lt                     (* < *)
  | Eq                     (* = *)
  | Bind of ident          (* |> ID *)
  | Call of ident          (* # ID *)
  | If of program          (* ? prog ; *)
  | Def of ident * program (* def prog ; *)
  | Ident of ident         (* ID *)
  | Num of int             (* num *)
and program = command list

let parse_ident =  (* TODO *)
  many1 (satisfy is_upper_case) >|= implode
let parse_num = 
  many1 (satisfy is_digit) >|= implode >|= int_of_string
(* You are not required to used this but it may be useful in
   understanding how to use `rec_parser` *)
let rec parse_com () =
  let parse_def =
    map2
      (fun id p -> Def (id, p))
      (keyword "def" >> parse_ident << ws)
      (parse_prog_rec () << char ';')
  in let parse_simple_command = choice [
    keyword "drop" >> pure Drop;
    keyword "swap" >> pure Swap;
    keyword "dup" >> pure Dup;
    keyword "." >> pure Trace;
    keyword "+" >> pure Add;
    keyword "-" >> pure Sub;
    keyword "*" >> pure Mul;
    keyword "/" >> pure Div;
    keyword "<" >> pure Lt;
    keyword "=" >> pure Eq;
  ] in
  let parse_bind = 
    (keyword "|>" >> parse_ident) >>= fun id -> pure (Bind id) in
  let parse_call = 
    (keyword "#" >> parse_ident) >>= fun id -> pure (Call id) in
  let parse_if =
    keyword "?" >> parse_prog_rec () >>= fun prog -> 
    char ';' >> pure (If prog)
  in
  let parse_ident_or_num = 
    (parse_ident >>= fun id -> pure (Ident id)) <|>
    (parse_num >>= fun n -> pure (Num n));
  in
  choice [
    parse_def;
    parse_simple_command;
    parse_bind;
    parse_call;
    parse_if;
    parse_ident_or_num;
  ] <|> fail (* TODO *)
and parse_prog_rec () =
  many ((rec_parser parse_com) << ws)

let parse_prog s = (* TODO *)
  match (ws >> many (parse_com () << ws) << ws) (explode s) with
    | Some (prog, []) -> Some prog
    | _ -> None

let test = parse_prog "drop"
let out = Some [Drop]
let _ = assert (test = out)
(* A VERY SMALL TEST SET *)

let test = parse_prog "     .       "
let out = Some [Trace]
let _ = assert (test = out)


let test = parse_prog "  |> TEST   "
let out = Some [Bind "TEST"]
let _ = assert (test = out)

let test = parse_prog "  23 00345 + |> OK "
let out = Some [Num 23; Num 345; Add; Bind "OK"]
let _ = assert (test = out)

let test = parse_prog "  def NEG 0 - ; 2 #NEG 2 =    \n\n   "
let out = Some [Def ("NEG", [Num 0; Sub]); Num 2; Call "NEG"; Num 2; Eq]
let _ = assert (test = out)

let test = parse_prog "
  def ABS
    dup 0 swap < ?
      0 -
    ;
  ;

  30 0 -
  #ABS
  |> X
"
let out = Some
    [ Def ("ABS", [Dup; Num 0; Swap; Lt; If [Num 0; Sub]])
    ; Num 30; Num 0; Sub
    ;  Call "ABS"
    ;  Bind "X"
    ]
let _ = assert (test = out)
(* EVALUATION *)

type stack = int list
type value
  = Num of int
  | Prog of program
type env = (ident * value) list
type trace = string list

let update_env (environment: env) (identifier: ident) (value: value) : env = (* TODO *)
  let rec update_or_add = function
    | [] -> [(identifier, value)] 
    | (id, _) :: xs when id = identifier -> (identifier, value) :: xs  
    | pair :: xs -> pair :: update_or_add xs 
  in
  update_or_add environment
let fetch_env (environment: env) (identifier: ident) : value option = (* TODO *)
  let rec find_binding = function
    | [] -> None 
    | (id, value) :: _ when id = identifier -> Some value 
    | _ :: xs -> find_binding xs 
  in
  find_binding environment

let rec eval_prog (stack : stack) (env : env) (trace : trace) (prog : program) : trace = (* TODO *)
  match prog with
  | [] -> trace 
  | com :: rest -> match com with
   
    | Drop -> (
      match stack with
      | _ :: tail -> eval_prog tail env trace rest  
      | [] -> ["panic: stack underflow"] 
    )
    | Swap -> (
      match stack with
      | x :: y :: tail -> eval_prog (y :: x :: tail) env trace rest 
      | _ -> ["panic: stack underflow"]  
    )
    | Dup -> (
      match stack with
      | x :: tail -> eval_prog (x :: x :: tail) env trace rest 
      | [] -> ["panic: dup failed due to empty stack"]  

    )
    | Trace -> (
      match stack with
      | x :: tail -> eval_prog tail env ((string_of_int x) :: trace) rest
      | [] -> ["panic: trace failed due to empty stack"]
    )
    | Num n -> (
      match n with
      | _ when n < 0 -> eval_prog ((0 - n) :: stack) env trace rest
      | _ -> eval_prog (n :: stack) env trace rest
    )
    | Add -> (
      match stack with
      | x :: y :: tail -> eval_prog ((x + y) :: tail) env trace rest
      | _ -> ["panic: add failed due to insufficient stack size"]
    )
    | Sub -> (
      match stack with
      | x :: y :: tail -> eval_prog ((x - y) :: tail) env trace rest
      | _ -> ["panic: subtract failed due to insufficient stack size"]
    )
    | Mul -> (
      match stack with
      | x :: y :: tail -> eval_prog ((x * y) :: tail) env trace rest  
      | _ -> ["panic: mul failed due to insufficient stack size"]
    )
    | Div -> (
      match stack with
      | x :: y :: tail when x <> 0 -> eval_prog ((x / y) :: tail) env trace rest
      | _ :: y :: tail when y = 0 -> ["panic: division by zero"]
      | _ -> ["panic: div failed due to insufficient stack size"]
    )
    | Lt -> (
      match stack with
      | x :: y :: tail -> eval_prog (((if x < y then 1 else 0) :: tail)) env trace rest 
      | _ -> ["panic: less than failed due to insufficient stack size"] 
    )
    | Eq -> (
      match stack with
      | x :: y :: tail -> eval_prog (((if x = y then 1 else 0) :: tail)) env trace rest 
      | _ -> ["panic: less than failed due to insufficient stack size"] 
    )
    | Bind ident  -> (
      match stack with
      | x :: tail -> 
          let updated_env = update_env env ident (Num x) 
          in eval_prog tail updated_env trace rest
      | [] -> ["panic: variable assignment failed due to empty stack"]
    )
    | Ident ident -> (
      match fetch_env env ident with
      | Some (Num n) -> eval_prog (n :: stack) env trace rest 
      | Some (Prog _) -> ["panic: identifier bound to a subroutine, not a number"]
      | None -> ["panic: identifier not bound in the environment"]
    )
    | Def (id, sub_prog) -> 
      let updated_env = update_env env id (Prog sub_prog) 
      in eval_prog stack updated_env trace rest  
    | Call ident -> (
      match fetch_env env ident with
      | Some (Prog sub_prog) -> eval_prog stack env trace (sub_prog @ rest) 
      | _ -> ["panic: subroutine call failed, ID not bound to a program or bound to a number"]
    )
    | If sub_prog -> (
      match stack with
      | x :: tail when x <> 0 -> eval_prog tail env trace (sub_prog @ rest) 
      | x :: tail -> eval_prog tail env trace rest 
      | [] -> ["panic: if-statement failed due to empty stack"]
    )
let interp (input : string) : trace option = (* TODO *)
  match parse_prog input with
  | Some prog -> Some(eval_prog [] [] [] prog)
  | _ -> None

(* END OF PROJECT CODE *)

(* ============================================================ *)

(* UNCOMMENT TO RUN INTERPRETER *)

let print_trace t =
  let rec go t =
    match t with
    | [] -> ()
    | x :: t ->
      print_endline x;
      go t
  in go (List.rev t)

let main () =
  let input =
    let rec get_input s =
      try
        get_input (s ^ "\n" ^ read_line ())
      with End_of_file ->
        s
    in get_input ""
  in
  match interp input with
  | None -> print_endline "Parse Error"
  | Some t -> print_trace t

let _ = main () 
