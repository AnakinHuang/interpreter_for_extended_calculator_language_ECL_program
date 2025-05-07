(*******************************************************************
    General-purpose LL(1) parser generator and parse tree generator,
    with (skeleton of) a syntax tree builder and interpreter for an
    extended calculator language.

    (c) Michael L. Scott, 2023
    For use by students in CSC 2/454 at the University of Rochester,
    during the Fall 2023 term.  All other use requires written
    permission of the author.

    If compiled and run, will execute "main()".
    Alternatively, can be "#use"-ed (or compiled and then "#load"-ed)
    into the top-level interpreter.
 *******************************************************************)

 open List;;
 (* The List library includes a large collection of useful functions.
    In the provided code, I've used assoc, exists, filter, find,
    find_opt, fold_left, hd, length, map, and rev.
 *)
 
 open Str;;      (* for regexp and split *)
 (* The Str library provides a few extra string-processing routines.
    In particular, it provides "split" and "regexp", which I use to break
    program input into whitespace-separated words.  Note, however, that
    this library is not automatically available.  If you are using the
    top-level interpreter, you have to say
         #load "str.cma";;
    before you say
         #use "interpreter.ml";;
    If you are generating an executable from the shell, you have to
    include the library name on the command line:
         ocamlc -o interpreter str.cma interpreter.ml
 *)
 
 (*******************************************************************
     Preliminaries.
  *******************************************************************)
 
 (* Surprisingly, compose isn't built in.  It's included in various
    widely used commercial packages, but not in the core libraries. *)
 let compose f g x = f (g x);;
 
 (* is e a member of list l? *)
 let member e l = exists ((=) e) l;;
 
 (* OCaml has a built-in quicksort; this was just an exercise. *)
 let rec sort l =
   let rec partition pivot l left right =
     match l with
     | []        -> (left, right)
     | c :: rest -> if c < pivot
                    then partition pivot rest (c :: left) right
                    else partition pivot rest left (c :: right) in
   match l with
   | []        -> l
   | h :: []   -> l
   | h :: rest -> let (left, right) = partition h rest [] [] in
                  (sort left) @ [h] @ (sort right);;
 
 (* Leave only one of any consecutive identical elements in list. *)
 let rec unique l =
   match l with
   | []             -> l
   | h :: []        -> l
   | a :: b :: rest -> if a = b (* structural eq *)
                       then unique (b :: rest)
                       else a :: unique (b :: rest);;
 
 let unique_sort l = unique (sort l);;
 
 (* Join two strings with a given separator in between
    -- but only if both are non-null. *)
 let str_cat sep a b =
   match (a, b) with
   | (a, "") -> a
   | ("", b) -> b
   | (_, _) -> a ^ sep ^ b;;
 
 (*******************************************************************
     Grammars, Parser Generator, Scanner, and Parser.
 
     For this course we are using a single grammar -- for the extended
     calcular language.  It was easiest for me to build the project,
     however, if I could experiment with changes to the language without
     having to change the parser by hand.  So we have here a complete
     parser generator.
  *******************************************************************)
 
 type symbol_productions = (string * string list list);;
 type grammar = symbol_productions list;;
 type parse_table = (string * (string list * string list) list) list;;
 (*                  nt        predict_set   rhs *)
 
 let calc_gram : grammar =
   [ ("P",  [["SL"; "$$"]])
   ; ("SL", [["S"; "SL"]; []])
   ; ("S",  [ ["id"; ":="; "E"]; ["read"; "id"]; ["write"; "E"]])
   ; ("E",  [["T"; "TT"]])
   ; ("T",  [["F"; "FT"]])
   ; ("TT", [["AO"; "T"; "TT"]; []])
   ; ("FT", [["MO"; "F"; "FT"]; []])
   ; ("AO", [["+"]; ["-"]])
   ; ("MO", [["*"]; ["/"]])
   ; ("F",  [["id"]; ["num"]; ["("; "E"; ")"]])
   ];;
 
 let ecg : grammar =             (* extended calculator grammar *)
   [ ("P",  [["SL"; "$$"]])
   ; ("SL", [["S"; "SL"]; []])
   ; ("S",  [ ["int"; "id"; ":="; "E"]; ["real"; "id"; ":="; "E"]
            ; ["id"; ":="; "E"]; ["read"; "TP"; "id"]; ["write"; "E"]
            ; ["if"; "C"; "SL"; "fi"]; ["do"; "SL"; "od"]; ["check"; "C"]
            ])
   ; ("TP", [["int"]; ["real"]; []])
   ; ("C",  [["E"; "RO"; "E"]])
   ; ("RO", [["=="]; ["!="]; ["<"]; [">"]; ["<="]; [">="]])
   ; ("E",  [["T"; "TT"]])
   ; ("TT", [["AO"; "T"; "TT"]; []])
   ; ("T",  [["F"; "FT"]])
   ; ("FT", [["MO"; "F"; "FT"]; []])
   ; ("F",  [["id"]; ["i_num"]; ["r_num"]; ["("; "E"; ")"]
            ; ["trunc"; "("; "E"; ")"]; ["float"; "("; "E"; ")"]])
   ; ("AO", [["+"]; ["-"]])
   ; ("MO", [["*"]; ["/"]])
   ];;
 
 (* Return all individual productions in grammar. *)
 let productions gram : (string * string list) list =
   let prods (lhs, rhss) = map (fun rhs -> (lhs, rhs)) rhss in
   fold_left (@) [] (map prods gram);;
 
 (* Return all symbols in grammar. *)
 let gsymbols gram : string list =
   unique_sort
     (fold_left (@) [] (map (compose (fold_left (@) []) snd) gram));;
 
 (* Return all elements of l that are not in to_exclude.
    Assume that both lists are sorted. *)
 let list_minus l to_exclude =
   let rec helper rest te rtn =
     match rest with
     | []     -> rtn
     | h :: t -> match te with
                 | []       -> (rev rest) @ rtn
                 | h2 :: t2 -> match Stdlib.compare h h2 with
                               | -1 -> helper t te (h :: rtn)
                               |  0 -> helper t t2 rtn
                               |  _ -> helper rest t2 rtn in
   rev (helper l to_exclude []);;
 
 (* Return just the nonterminals. *)
 let nonterminals gram : string list = map fst gram;;
 
 (* Return just the terminals. *)
 let terminals gram : string list =
     list_minus (gsymbols gram) (unique_sort (nonterminals gram));;
 
 (* Return the start symbol.  Throw exception if grammar is empty. *)
 let start_symbol gram : string = fst (hd gram);;
 
 let is_nonterminal e gram = member e (nonterminals gram);;
 
 let is_terminal e gram = member e (terminals gram);;
 
 let union s1 s2 = unique_sort (s1 @ s2);;
 
 (* Return suffix of lst that begins with first occurrence of sym
    (or [] if there is no such occurrence). *)
 let rec suffix sym lst = 
   match lst with
   | [] -> []
   | h :: t -> if h = sym (* structural eq *)
               then lst else suffix sym t;;
 
 (* Return a list of pairs.
    Each pair consists of a symbol A and a list of symbols beta
    such that for some alpha, A -> alpha B beta. *)
 type right_context = (string * string list) list;;
 let get_right_context (b:string) gram : right_context =
   fold_left (@) []
             (map (fun prod ->
                     let a = fst prod in
                     let rec helper accum rhs =
                       let b_beta = suffix b rhs in
                       match b_beta with
                       | [] -> accum
                       | x :: beta  -> (* assert x = b *)
                                       helper ((a, beta) :: accum) beta in
                     helper [] (snd prod))
                  (productions gram));;
 
 (********
     Parser generator starts here.
 ********)
 
 type symbol_knowledge = string * bool * string list * string list;;
 type knowledge = symbol_knowledge list;;
 let symbol_field (s, e, fi, fo) = s;;
 let eps_field (s, e, fi, fo) = e;;
 let first_field (s, e, fi, fo) = fi;;
 let follow_field (s, e, fi, fo) = fo;;
 
 let initial_knowledge gram : knowledge =
   map (fun a -> (a, false, [], [])) (nonterminals gram);;
 
 let get_symbol_knowledge (a:string) (kdg:knowledge) : symbol_knowledge =
   find (fun (s, e, fi, fo) -> s = a) kdg;;
 
 (* Can word list w generate epsilon based on current estimates?
    if w is an empty list, yes
    if w is a single terminal, no
    if w is a single nonterminal, look it up
    if w is a non-empty list, "iterate" over elements *)
 let rec generates_epsilon (w:string list) (kdg:knowledge) gram : bool =
   match w with
   | [] -> true
   | h :: t -> if is_terminal h gram then false
               else eps_field (get_symbol_knowledge h kdg)
                    && generates_epsilon t kdg gram;;
 
 (* Return FIRST(w), based on current estimates.
    if w is an empty list, return []  [empty set]
    if w is a single terminal, return [w]
    if w is a single nonterminal, look it up
    if w is a non-empty list, "iterate" over elements *)
 let rec first (w:string list) (kdg:knowledge) gram : (string list) =
   match w with
   | [] -> []
   | x :: _ when is_terminal x gram -> [x]
   | x :: rest -> let s = first_field (get_symbol_knowledge x kdg) in
                  if generates_epsilon [x] kdg gram
                  then union s (first rest kdg gram)
                  else s;;
 
 let follow (a:string) (kdg:knowledge) : string list =
   follow_field (get_symbol_knowledge a kdg);;
 
 let rec map3 f l1 l2 l3 =
   match (l1, l2, l3) with
   | ([], [], []) -> []
   | (h1 :: t1, h2 :: t2, h3 :: t3) -> (f h1 h2 h3) :: map3 f t1 t2 t3
   | _ -> raise (Failure "mismatched_lists in map3");;
 
 (* Return knowledge structure for grammar.
    Start with (initial_knowledge grammar) and "iterate",
    until the structure doesn't change.
    Uses (get_right_context B gram), for all nonterminals B,
    to help compute follow sets. *)
 let get_knowledge gram : knowledge =
   let nts : string list = nonterminals gram in
   let right_contexts : right_context list
      = map (fun s -> get_right_context s gram) nts in
   let rec helper kdg =
     let update : symbol_knowledge -> symbol_productions
                    -> right_context -> symbol_knowledge
           = fun old_sym_kdg sym_prods sym_right_context ->
       let my_first s = first s kdg gram in
       let my_eps s = generates_epsilon s kdg gram in
       let filtered_follow p = if my_eps (snd p)
                               then (follow (fst p) kdg)
                               else [] in
       (
         symbol_field old_sym_kdg,       (* nonterminal itself *)
         (eps_field old_sym_kdg)         (* previous estimate *)
             || (fold_left (||) false (map my_eps (snd sym_prods))),
         union (first_field old_sym_kdg) (* previous estimate *)
             (fold_left union [] (map my_first (snd sym_prods))),
         union (union
                 (follow_field old_sym_kdg)  (* previous estimate *)
                 (fold_left union [] (map my_first
                                       (map (fun p ->
                                               match snd p with
                                               | [] -> []
                                               | h :: t -> [h])
                                            sym_right_context))))
               (fold_left union [] (map filtered_follow sym_right_context))
       ) in    (* end of update *)
     let new_kdg = map3 update kdg gram right_contexts in
     (* body of helper: *)
     if new_kdg = kdg then kdg else helper new_kdg in
   (* body of get_knowledge: *)
   helper (initial_knowledge gram);;
 
 let get_parse_table (gram:grammar) : parse_table =
   let kdg = get_knowledge gram in
   map (fun (lhs, rhss) ->
         (lhs, (map (fun rhs ->
                       (union (first rhs kdg gram)
                              (if (generates_epsilon rhs kdg gram)
                               then (follow lhs kdg) else []),
                       rhs))
                    rhss)))
       gram;;
 
 type row_col = int * int;;      (* source location *)
 let complaint (loc:row_col) (msg:string) =
   let (l, c) = loc in
   Printf.sprintf " line %d, col %d: %s" l c msg;;
 
 (* Convert string to list of chars, each tagged with row and column.
    Also return number of lines. *)
 let explode_and_tag (s:string) : (char * row_col) list * int =
   let rec exp i r c l =
     if i = String.length s then l
     else
       let (r2, c2) = if s.[i] = '\n' then (r+1, 1) else (r, c+1) in
       exp (i+1) r2 c2 ((s.[i], (r, c)) :: l) in
   let chars = exp 0 1 1 [] in
   let rows = match chars with
              | [] -> 0
              | (_, (r, _))::t -> r in
   (rev chars, rows)
 
 (* Convert list of char to string.
    (This uses imperative features.  It used to be a built-in.) *)
 let implode (l:char list) : string =
   let res = Bytes.create (length l) in
   let rec imp i l =
     match l with
     | [] -> Bytes.to_string res
     | c :: l -> Bytes.set res i c; imp (i + 1) l in
   imp 0 l;;
 
 (********
     Scanner.  Currently specific to the extended calculator language.
 ********)
 
 type token = string * string * row_col;;
 (*         category * name   * row+column *)
 
 let tokenize (program:string) : token list =
   let (chars, num_lines) = explode_and_tag program in
   let get_id prog =
     let rec gi tok p =
         match p with
         | (c, _) :: rest when (('a' <= c && c <= 'z')
                                || ('A' <= c && c <= 'Z')
                                || ('0' <= c && c <= '9') || (c = '_'))
             -> gi (c :: tok) rest
         | _ -> (implode (rev tok), p) in
     gi [] prog in
   (* get_num matches digit*(.digit*((e|E)(+|-)?digit+)?)?
      We're pickier below -- insist on a digit on at least one side of the . *)
   let get_num prog =        (* integer or real *)
     let get_int prog =          (* eat digit* *)
       let rec gi tok p =
           match p with
           | (c, _) :: rest when ('0' <= c && c <= '9')
               -> gi (c :: tok) rest
           | _ -> (implode (rev tok), p) in
       gi [] prog in
     let get_exp prog =          (* eat (e|E)(+|-|epsilon)digit+ *)
       match prog with
       | (e, eloc) :: r1 when (e = 'e' || e = 'E')
           -> (match r1 with
               | (s, _) :: (d, dloc) :: r2
                     when (s = '+' || s = '-') && ('0' <= d && d <= '9')
                 -> let (pow, r3) = get_int ((d, dloc) :: r2) in
                    ((String.make 1 e) ^ (String.make 1 s) ^ pow, r3)
               | (d, dloc) :: r2 when ('0' <= d && d <= '9')
                 -> let (pow, r3) = get_int ((d, dloc) :: r2) in
                    ((String.make 1 e) ^ pow, r3)
               | _ -> ("error", (e, eloc) :: r1))
       | _ -> ("", prog) in
     let (whole, r1) = get_int prog in
     match r1 with
     | ('.', _) :: r2
         -> let (frac, r3) = get_int r2 in
            let (exp, r4) = get_exp r3 in
            (whole ^ "." ^ frac ^ exp, r4)
     | _ -> (whole, r1) in
   let rec get_error tok prog =
     match prog with
     | [] -> (implode (rev tok), prog)
     | (c, _) :: rest ->
         match c with
         | ';' | ':' | '+' | '-' | '*' | '/' | '(' | ')'
         | '_'| '<' | '>' | '=' | 'a'..'z' | 'A'..'Z' | '0'..'9'
             -> (implode (rev tok), prog)
         | _ -> get_error (c :: tok) rest in
   let rec skip_space prog =
     match prog with
     | [] -> []
     | (c, _) :: rest -> if (c = ' ' || c = '\n' || c = '\r' || c = '\t')
                         then skip_space rest else prog in
   let rec tkize toks prog =
     match prog with
     | []                           -> ((("$$", (num_lines + 1, 0)) :: toks), [])
     | ('\n', _) :: rest
     | ('\r', _) :: rest
     | ('\t', _) :: rest
     | (' ', _)  :: rest            -> tkize toks (skip_space prog)
     | (':', l) :: ('=', _) :: rest -> tkize ((":=", l) :: toks) rest
     | ('+', l) :: rest             -> tkize (("+", l)  :: toks) rest
     | ('-', l) :: rest             -> tkize (("-", l)  :: toks) rest
     | ('*', l) :: rest             -> tkize (("*", l)  :: toks) rest
     | ('/', l) :: rest             -> tkize (("/", l)  :: toks) rest
     | ('(', l) :: rest             -> tkize (("(", l)  :: toks) rest
     | (')', l) :: rest             -> tkize ((")", l)  :: toks) rest
     | ('<', l) :: ('=', _) :: rest -> tkize (("<=", l) :: toks) rest
     | ('<', l) :: rest             -> tkize (("<", l)  :: toks) rest
     | ('>', l) :: ('=', _) :: rest -> tkize ((">=", l) :: toks) rest
     | ('>', l) :: rest             -> tkize ((">", l)  :: toks) rest
     | ('=', l) :: ('=', _) :: rest -> tkize (("==", l) :: toks) rest
     | ('!', l) :: ('=', _) :: rest -> tkize (("!=", l) :: toks) rest
     | (h, l) :: t ->
         match h with
         | '.' | '0'..'9' -> let (nm, rest) = get_num prog in
                             tkize ((nm, l) :: toks) rest
         | 'a'..'z'
         | 'A'..'Z'
         | '_'            -> let (nm, rest) = get_id prog in
                             tkize ((nm, l) :: toks) rest
         | x              -> let (nm, rest) = get_error [x] t in
                             tkize ((nm, l) :: toks) rest in
   let (toks, _) = (tkize [] chars) in
   let categorize tok =
     let (nm, loc) = tok in
     match nm with
     | "check" | "do"   | "float" | "fi"    | "if"    | "int"   
     | "od"    | "read" | "real"  | "trunc" | "write"
     | ":="    | "+"    | "-"     | "*"     | "/"     | "("  | ")"
     | "<"    | "<="    | ">"     | ">="    | "!="    | "==" | "$$"
         -> (nm, nm, loc)
     | _ -> match nm.[0] with
            | '.' -> (try    (* insist on digit on at least one side of . *)
                        if ('0' <= nm.[1] && nm.[1] <= '9')
                          then ("r_num", nm, loc)
                          else ("error", nm, loc)
                      with Invalid_argument(_) -> ("error", nm, loc))
            | '0'..'9' -> if String.contains nm '.'
                             then ("r_num", nm, loc)
                             else ("i_num", nm, loc)
            | 'a'..'z'
            | 'A'..'Z' | '_' -> ("id", nm, loc)
            | _ -> ("error", nm, loc) in
   map categorize (rev toks);;
 
 (*******************************************************************
     Parser.  The main parse routine returns a parse tree (or PT_error if
     the input program is syntactically invalid).  To build that tree it
     employs a simplified version of the "attribute stack" described in
     Section 4.5.2 (pages 50-55) on the PLP companion site.
 
     When it predicts A -> B C D, the parser pops A from the parse stack
     and then, before pushing D, C, and B (in that order), it pushes a
     number (in this case 3) indicating the length of the right hand side.
     It also pushes A into the attribute stack.
 
     When it matches a token, the parser pushes this into the attribute
     stack as well.
 
     Finally, when it encounters a number (say k) in the stack (as opposed
     to a character string), the parser pops k+1 symbols from the
     attribute stack, joins them together into a list, and pushes the list
     back into the attribute stack.
 
     These rules suffice to accumulate a complete parse tree into the
     attribute stack at the end of the parse.
 
     Note that everything is done functionally.  We don't really modify
     the stacks; we pass new versions to tail recursive routines.
  *******************************************************************)
 
 (* Extract grammar from parse-tab, so we can invoke the various routines
    that expect a grammar as argument. *)
 let grammar_of (parse_tab:parse_table) : grammar =
   map (fun p -> (fst p,
                  (fold_left (@)
                             []
                             (map (fun (a, b) -> [b]) (snd p)))))
       parse_tab;;
 
 type parse_tree =   (* among other things, parse_trees are *)
 | PT_error          (* the elements of the attribute stack *)
 | PT_id of (string * row_col)
 | PT_int of (string * row_col)
 | PT_real of (string * row_col)
 | PT_term of (string * row_col)
 | PT_nt of (string * row_col * parse_tree list);;
     
 (* Pop rhs-len + 1 symbols off the attribute stack,
    assemble into a production, and push back onto the stack. *)
 let reduce_1_prod (astack:parse_tree list) (rhs_len:int) : parse_tree list =
   let rec helper atk k prod =
     match (k, atk) with
     | (0, PT_nt(nt, loc, []) :: t) -> PT_nt(nt, loc, prod) :: t
     | (n, h :: t) when n <> 0 -> helper t (k - 1) (h :: prod)
     | _ -> raise (Failure "expected nonterminal at top of astack") in
    helper astack rhs_len [];;
 
 type parse_action = PA_error | PA_prediction of string list;;
 (* Double-index to find prediction (list of RHS symbols) for
    nonterminal nt and terminal t.  Return PA_error if not found. *)
 let get_parse_action (nt:string) (t:string) (parse_tab:parse_table)
     : parse_action =
   let rec helper l =
       match l with
       | [] -> PA_error
       | (fs, rhs) :: rest -> if member t fs then PA_prediction(rhs)
                              else helper rest in
   helper (assoc nt parse_tab);;
 
 type ps_item =      (* elements of parse stack *)
 | PS_end of int
 | PS_sym of string;;
 
 (* Parse program according to grammar.
    [Commented-out code would
        print predictions and matches (imperatively) along the way.]
    Return parse tree if the program is in the language; PT_error if it's not. *)
 let parse (parse_tab:parse_table) (program:string) : parse_tree =
   let die loc msg =
     begin
       let (l, c) = loc in
       (* print to screen in REPL; to stderr when compiled *)
       (if !Sys.interactive then Printf.printf else Printf.eprintf)
         "syntax error at line %d, col %d: %s\n" l c msg;
       PT_error 
     end in
   let gram = grammar_of parse_tab in
   let rec helper pstack tokens astack =
     match pstack with
     | [] ->
         if tokens = [] then
           (* assert: astack is nonempty *)
           hd astack
         else die (0, 0) "extra input beyond end of program"
     | PS_end(n) :: ps_tail ->
         helper ps_tail tokens (reduce_1_prod astack n)
     | PS_sym(tos) :: ps_tail ->
         match tokens with
         | [] -> die (0, 0) "unexpected end of program"
         | (term, nm, loc) :: more_tokens ->
            (* if nm is an individual identifier or number,
               term will be a generic "id" or "i_num" or "r_num" *)
           if is_terminal tos gram then
             if tos = term then
               begin
               (*
                 print_string ("   match " ^ tos);
                 print_string
                     (if tos <> term      (* deep comparison *)
                          then (" (" ^ nm ^ ")") else "");
                 print_newline ();
               *)
                 helper ps_tail more_tokens
                        (( match term with
                           | "id"  -> PT_id(nm, loc)
                           | "i_num" -> PT_int(nm, loc)
                           | "r_num" -> PT_real(nm, loc)
                           | _     -> PT_term(nm, loc) ) :: astack)
               end
                        (* note push of nm into astack *)
             else die loc ("expected " ^ tos ^ "; saw " ^ nm)
           else (* nonterminal *)
             match get_parse_action tos term parse_tab with
             | PA_error -> die loc ("no prediction for " ^ tos
                                    ^ " when seeing " ^ nm)
             | PA_prediction(rhs) ->
                 begin
                 (*
                   print_string ("   predict " ^ tos ^ " ->");
                   print_string (fold_left (fun a b -> a ^ " " ^ b) "" rhs);
                   print_newline ();
                 *)
                   helper ((fold_left (@) [] 
                                     (map (fun s -> [PS_sym(s)]) rhs))
                               @ [PS_end(length rhs)] @ ps_tail)
                          tokens (PT_nt(tos, loc, []) :: astack)
                 end in
   helper [PS_sym(start_symbol gram)] (tokenize program) [];;
 
 let cg_parse_table = get_parse_table calc_gram;;
 
 let ecg_parse_table = get_parse_table ecg;;
 
 (*******************************************************************
     (* NOTICE *)
     
     Everything above this point in the file is complete and (I think)
     usable as-is.  The rest of the file, from here down, is a possible
     skeleton for the code you need to write.  It was extracted from the
     complete working version available as ~cs254/bin/ecl on the caug
     machines.
 
  *******************************************************************)
 
 (* Syntax tree node types.
    We distinguish between statements and expressions.
    Comments below indicate what syntactic element in the source
    is associated with the location [row_col] values.
    Note that each declaration (e.g., "int foo : 3" or "read int foo")
    is turned into a _pair_ of AST nodes -- one for the declaration
    itself and one for the initialization.
 *)
 
 type ast_sl = ast_s list
 and ast_s =
 | AST_error
 | AST_i_dec of (string * row_col)   (* id location *)
 | AST_r_dec of (string * row_col)   (* id location *)
 | AST_read of (string * row_col)    (* id location *)
 | AST_write of (ast_e)
 | AST_assign of (string * ast_e * row_col * row_col)
                                     (* id location, := location *)
 | AST_if of (ast_c * ast_sl)
 | AST_do of ast_sl
 | AST_check of (ast_c * row_col)    (* check location *)
 and ast_e =
 | AST_int of (string * row_col)
 | AST_real of (string * row_col)
 | AST_id of (string * row_col)
 | AST_float of (ast_e * row_col)    (* lparen location *)
 | AST_trunc of (ast_e * row_col)    (* lparen location *)
 | AST_binop of (string * ast_e * ast_e * row_col)
                                     (* op location *)
 and ast_c = (string * ast_e * ast_e * row_col);;
                                     (* op location *)
   
 (* Convert parse tree to syntax tree.
    Walks the parse tree using a collection of mutually recursive subroutines. *)
 let rec ast_ize_prog (p:parse_tree) : ast_sl =
   (* Converts a parse tree into an abstract syntax tree (AST) for a program. 
      It matches the parse tree with the expected structure for a program. If the structure is correct, 
      it converts the statement list into an AST. If the structure is not correct, it raises a failure. *)
   match p with
   | PT_nt("P", _, [sl; PT_term("$$", _)]) -> ast_ize_stmt_list sl
   | _ -> raise (Failure "cannot interpret erroneous tree")
 
 and ast_ize_stmt_list (sl:parse_tree) : ast_sl =
  (* Converts a parse tree into an abstract syntax tree (AST) for a statement list. 
     It matches the parse tree with the expected structures for a statement list. 
     If the structure is correct, it converts each statement into an AST and combines them into a list. 
     If the structure is not correct, it raises a failure. *)
   match sl with
   | PT_nt("SL", _, []) -> []
   | PT_nt("SL", _, [s; sl]) -> (ast_ize_stmt s) @ (ast_ize_stmt_list sl)
   | _ -> raise (Failure "malformed parse tree in ast_ize_stmt_list")
 
 and ast_ize_stmt (s:parse_tree) : ast_sl =
  (* Converts a parse tree into an abstract syntax tree (AST) for a statement. 
     It matches the parse tree with the expected strucres for different typetus of statements 
     (int declaration, real declaration, assignment, read, write, if, do, check). 
     If the structure is correct, it converts the statement into an AST. 
     If the structure is not correct, it raises a failure. *)
   match s with
   | PT_nt("S", _, [PT_term("int", _); PT_id(lhs, vloc); PT_term(":=", aloc); expr])
     -> [AST_i_dec(lhs, vloc); AST_assign(lhs, (ast_ize_expr expr), vloc, aloc)]
   | PT_nt("S", _, [PT_term("real", _); PT_id(lhs, vloc); PT_term(":=", aloc); expr])
     -> [AST_r_dec(lhs, vloc); AST_assign(lhs, (ast_ize_expr expr), vloc, aloc)]
   | PT_nt("S", _, [PT_id(lhs, vloc); PT_term(":=", aloc); expr])
     -> [AST_assign(lhs, (ast_ize_expr expr), vloc, aloc)]
        (* vloc is the place to complain about undeclared lhs;
           aloc (:= sign) is the place to complain about a type clash *)
   | PT_nt("S", _, [PT_term("read", _); PT_nt("TP", _, [PT_term("int", _)]); PT_id(lhs, vloc)])
     -> [AST_i_dec(lhs, vloc); AST_read(lhs, vloc)]
   | PT_nt("S", _, [PT_term("read", _); PT_nt("TP", _, [PT_term("real", _)]); PT_id(lhs, vloc)])
     -> [AST_r_dec(lhs, vloc); AST_read(lhs, vloc)]
   | PT_nt("S", _, [PT_term("read", _); PT_nt("TP", _, []); PT_id(lhs, vloc)])
     -> [AST_read(lhs, vloc)]
   | PT_nt("S", _, [PT_term("write", _); expr])
     -> [AST_write (ast_ize_expr expr)]
   | PT_nt("S", _, [PT_term("if", _); cond; sl; PT_term("fi", _)])
     -> [AST_if((ast_ize_cond cond), (ast_ize_stmt_list sl))]
   | PT_nt("S", _, [PT_term("do", _); sl; PT_term("od", _)])
     -> [AST_do(ast_ize_stmt_list sl)]
   | PT_nt("S", _, [PT_term("check", aloc); cond])
     -> [AST_check(ast_ize_cond cond, aloc)]
   | _ -> raise (Failure "malformed parse tree in ast_ize_stmt")
 
 and ast_ize_expr (e:parse_tree) : ast_e =   (* E, T, or F *)
 (* Converts a parse tree into an abstract syntax tree (AST) for an expression. 
    It matches the parse tree with the expected structures for different types of expressions 
    (E, T, F, id, int, real, trunc, float). If the structure is correct, it converts the expression into an AST. 
    If the structure is not correct, it raises a failure. *)
   match e with
   | PT_nt("E", _, [t; tt]) -> ast_ize_expr_tail (ast_ize_expr t) tt
   | PT_nt("T", _, [f; ft]) -> ast_ize_expr_tail (ast_ize_expr f) ft
   | PT_nt("F", _, [PT_id(lhs, vloc)]) -> AST_id(lhs, vloc)
   | PT_nt("F", _, [PT_int(lhs, vloc)]) -> AST_int(lhs, vloc)
   | PT_nt("F", _, [PT_real(lhs, vloc)]) -> AST_real(lhs, vloc)
   | PT_nt("F", _, [PT_term("(", _); expr; PT_term(")", _)]) -> ast_ize_expr expr
   | PT_nt("F", _, [PT_term("trunc", aloc); PT_term("(", _); expr; PT_term(")", _)]) 
     -> AST_trunc((ast_ize_expr expr), aloc)
   | PT_nt("F", _, [PT_term("float", aloc); PT_term("(", _); expr; PT_term(")", _)]) 
     -> AST_float((ast_ize_expr expr), aloc)
   | _ -> raise (Failure "malformed parse tree in ast_ize_expr")
 
 and ast_ize_expr_tail (lo:ast_e) (tail:parse_tree) : ast_e =   (* TT or FT *)
   (* lo is a left operand for a potential operator in tail *)
   (* Combines Expression together. Converts a parse tree into an abstract syntax tree (AST) for an expression tail. 
      It matches the parse tree with the expected structures for different types of expression tails (TT, FT). 
      If the structure is correct, it converts the expression tail into an AST. 
      If the structure is not correct, it raises a failure. *)
   match tail with
   | PT_nt("TT", _, []) -> lo
   | PT_nt("TT", _, [PT_nt("AO", _, [PT_term(ao, aloc)]); t; tt]) 
     -> AST_binop(ao, lo, (ast_ize_expr_tail (ast_ize_expr t) tt), aloc)
   | PT_nt("FT", _, []) -> lo
   | PT_nt("FT", _, [PT_nt("MO", _, [PT_term(mo, aloc)]); f; ft]) 
     -> AST_binop(mo, lo, (ast_ize_expr_tail (ast_ize_expr f) ft), aloc)
   | _ -> raise (Failure "malformed parse tree in ast_ize_expr_tail")
 
 and ast_ize_cond (c:parse_tree) : ast_c =
  (* Converts a parse tree into an abstract syntax tree (AST) for a condition. 
     It matches the parse tree with the expected structure for a condition. 
     If the structure is correct, it converts the condition into an AST. 
     If the structure is not correct, it raises a failure. *)
   match c with
   | PT_nt("C", _, [expr1; PT_nt("RO", _, [PT_term(ro, aloc)]); expr2]) 
     -> (ro, (ast_ize_expr expr1), (ast_ize_expr expr2), aloc)
   | _ -> raise (Failure "malformed parse tree in ast_ize_cond");;
 
 (*******************************************************************
     AST Pretty-printer.  This should be complete and usable as-is.
  *******************************************************************)
 
 let rec pp_sl (sl:ast_sl) (ind:string) : string =
   match sl with
   | []      -> ""
   | [s]     -> pp_s s ind
   | s :: tl -> pp_s s ind ^ "\n" ^ ind ^ pp_sl tl ind
 
 and pp_s (s:ast_s) (ind:string) : string =
   match s with
   | AST_i_dec(id, _)            -> "(int \"" ^ id ^ "\")"
   | AST_r_dec(id, _)            -> "(real \"" ^ id ^ "\")"
   | AST_read(id, _)             -> "(read \"" ^ id ^ "\")"
   | AST_write(expr)             -> "(write " ^ (pp_e expr) ^ ")"
   | AST_assign(id, expr, _, _)  -> "(:= \"" ^ id ^ "\" " ^ pp_e expr ^ ")"
   | AST_if(cond, sl)            -> "(if " ^ pp_c cond 
                                     ^ "\n" ^ ind ^ "  [ "
                                     ^ (pp_sl sl (ind ^ "    "))
                                     ^ "\n" ^ ind ^ "  ]\n" ^ ind ^ ")"
   | AST_do(sl)                  -> "(do " ^ "\n" ^ ind ^ "  [ "
                                     ^ (pp_sl sl (ind ^ "    "))
                                     ^ "\n" ^ ind ^ "  ]\n" ^ ind ^ ")"
   | AST_check(cond, _)          -> "(check " ^ pp_c cond ^ ")"
   | AST_error                   -> "error"
 
 and pp_e (e:ast_e) : string =
   match e with
   | AST_int(num, _)          -> "\"" ^ num ^ "\""
   | AST_real(num, _)         -> "\"" ^ num ^ "\""
   | AST_id(id, _)            -> "\"" ^ id ^ "\""
   | AST_float(e, _)          -> "(float " ^ (pp_e e) ^ ")"
   | AST_trunc(e, _)          -> "(trunc " ^ (pp_e e) ^ ")"
   | AST_binop(op, lo, ro, _) -> "(" ^ op ^ " " ^ pp_e lo ^ " " ^ pp_e ro ^ ")"
 
 and pp_c (c:ast_c) : string =
   let (op, lo, ro, _) = c in "(" ^ op ^ " " ^ pp_e lo ^ " " ^ pp_e ro ^ ")";;
 
 let pp_p (sl:ast_sl) = print_string ("[ " ^ (pp_sl sl "  ") ^ "\n]\n");;
 
 (*******************************************************************
     Interpreter and C language translator
 
     Imterprets an AST with purely dynamic semantics -- nothing is
     checked ahead of time.  Catches undefined variables, redefinitions,
     and type clashes.  Also divide-by-zero and non-numeric input or
     unexpected end of input on read.  Respects scopes: each variable is
     visible only from its declaration to the end of the innermost
     statement list in which it is declared.
  *******************************************************************)
 
 type value =
 | Ivalue of int
 | Rvalue of float
 | Error of string;;
 
 type 'a stack = 'a list;;
 let push (x:'a) (s:'a stack) : 'a stack = x :: s;;
 let pop (s:'a stack) : 'a option * 'a stack =
   match s with
   | [] -> (None, [])
   | x :: r -> (Some x, r);;
 
 (* Memory is a stack of scopes, with the innermost scope at the top.
    Each scope consists of a list of (name, value) pairs. *)
 type memory = (string * value) list stack;;
 
 let new_scope (mem:memory) : memory = push [] mem;;
 let end_scope (mem:memory) : memory = let (_, mem2) = pop mem in mem2;;
 
 let name_match id = fun (sym, _) -> id = sym;;
 
 let rec lookup_mem (id:string) (loc:row_col) (mem:memory) : value =
   match mem with
   | [] -> Error(complaint loc (id ^ " not found"))
   | scope :: surround ->
       match find_opt (name_match id) scope with
       | Some (_, v) -> v
       | None -> lookup_mem id loc surround;;
 
 let insert_mem (id:string) (v:value) (mem:memory) : (memory * bool) =
   match mem with
   | [] -> raise (Failure "unexpected case")
   | scope :: surround ->
       match find_opt (name_match id) scope with
       | Some _ -> (mem, false)
       | None   -> (((id, v) :: scope) :: surround, true);;
 
 (* Should be called only after verifying (with lookup_mem) that id is
    already present.  Throws exception if not. *)
 let rec update_mem (id:string) (v:value) (mem:memory) : memory =
   match mem with
   | [] -> raise (Failure (id ^ " not present"))
   | scope :: surround ->
       match find_opt (name_match id) scope with
       | Some _ -> ((id, v) :: (filter (fun (sym, _) -> id <> sym) scope))
                    :: surround
       | None   -> scope :: (update_mem id v surround);;
 
 type status =
 | Good
 | Bad       (* run-time error *)
 | Done;;    (* failed check *)
 
 (* Input to a calculator program is just a sequence of numbers.  We use
     the standard Str library to split the input string into whitespace-
     separated words, each of which is subsequently checked for validity. *)
 
 let rec interpret (ast:ast_sl) (full_input:string) : string =
  (* This function interprets an abstract syntax tree (AST) given the full input as a string. 
     It first splits the input into a list of strings, then interprets the statement list in the AST. 
     Depending on the status of the interpretation, it either generates equivalent C code and returns the output, 
     or just returns the output. *)
    let inp = split (regexp "[ \t\n\r]+") full_input in
    let (ok, _, _, outp) = interpret_sl 0 true ast [[]] inp [] in
    match ok with
    | Done -> 
      (c_gen ast "interactive"; (* generating equivalent C code to ouput.c *)
      ((fold_left (str_cat " ") "" outp) ^ "\n"))
    | _ -> ((fold_left (str_cat " ") "" outp) ^ "\n")
 
 (* iter1 indicates whether this is the first time this statement list
    has executed in a fresh scope.  It's false for the second and
    subsequent iterations of a do loop. *)
 and interpret_sl (loop_count:int) (iter1:bool) (sl:ast_sl) (mem:memory)
                  (inp:string list) (outp:string list)
     : status * memory * string list * string list =
     (*  ok?   new_mem   new_input     new_output *)
     (* This function interprets a statement list. 
        It uses a helper function to recursively interpret each statement in the list. *)
     let current_mem = if iter1 then new_scope mem else mem in
     let rec helper sl mem inp outp = 
       (match sl with
       | [] ->
         (match loop_count with
         | 0 -> (Done, (end_scope mem), inp, outp)
         | _ -> (Good, mem, inp, outp))
       | s::sls -> 
         (let (ok, new_mem, new_inp, new_outp)  = interpret_s loop_count iter1 s mem inp outp in
         match ok with
         | Good -> helper sls new_mem new_inp new_outp
         | Bad -> (Bad, [], [], new_outp)
         | Done -> (Done, (end_scope new_mem), new_inp, new_outp)))
     in
     helper sl current_mem inp outp
 
 (* NB: the following routine is complete.  You can call it on any
    statement node and it will figure out what more specific case to invoke. *)
 and interpret_s (loop_count:int) (iter1:bool) (s:ast_s) (mem:memory)
                 (inp:string list) (outp:string list)
     : status * memory * string list * string list = 
     (*  ok?    new_mem  new_input     new_output *)
     (* This function interprets a statement. It matches the statement with the expected types of statements 
        (int declaration, real declaration, read, write, assign, if, do, check, error), 
        and calls the appropriate interpretation function for each type. *)
   match s with
   | AST_i_dec(id, vloc)  -> interpret_dec iter1 id (Ivalue(0)) vloc mem inp outp
   | AST_r_dec(id, vloc)  -> interpret_dec iter1 id (Rvalue(0.0)) vloc mem inp outp
   | AST_read(id, loc)                -> interpret_read id loc mem inp outp
   | AST_write(expr)                  -> interpret_write expr mem inp outp
   | AST_assign(id, expr, vloc, aloc) -> interpret_assign id expr vloc aloc mem inp outp
   | AST_if(cond, sl)                 -> interpret_if loop_count cond sl mem inp outp
   | AST_do(sl)                       -> interpret_do loop_count sl mem inp outp
   | AST_check(cond, cloc)            -> if loop_count > 0
                                           then interpret_check cond mem inp outp
                                           else (Bad, [], [], outp @
                                             [complaint cloc "check not inside a loop"])
   | AST_error                        -> raise (Failure "cannot interpret erroneous tree")
 
 and interpret_dec (iter1:bool) (id:string) (v:value) (vloc:row_col)
                   (mem:memory) (inp:string list) (outp:string list)
     : status * memory * string list * string list =
     (*  ok?    new_mem  new_input     new_output *)
     (* This function interprets a declaration. If it's not the first iteration, 
        it does nothing. If it is the first iteration, it inserts the variable into the memory. 
        If the insertion is successful, it returns the new memory. 
        If the insertion is not successful (because the variable is already declared in this scope), 
        it returns an error. *)
   match iter1 with
   | false -> (Good, mem, inp, outp)
   | true -> 
     (let (new_mem, ok) = insert_mem id v mem in
     match ok with
     | true -> (Good, new_mem, inp, outp)
     | false ->
       (Bad, [], [], outp @ [complaint vloc (id ^ " is already declared in this scope")]))
 
 and interpret_read (id:string) (loc:row_col) (mem:memory)
                    (inp:string list) (outp:string list)
     : status * memory * string list * string list =
     (*  ok?    new_mem  new_input     new_output *)
     (* Interprets a read statement. It first looks up the value of the variable in the memory. 
        If the input is empty, it returns an error. If the input is not empty, 
        it checks if the string contains a dot. If it does, it tries to update the memory with the float value of the string. 
        If it doesn't, it tries to update the memory with the integer value of the string. 
        If the update is successful, it returns the new memory. 
        If the update is not successful (because the input is not numeric or doesn't match the type of the variable), 
        it returns an error. *)
   let old_v = lookup_mem id loc mem in
   match inp with
   | []          -> (Bad, [], [], outp @ [complaint loc "unexpected end of input"])
   | str::rest ->
     if String.contains str '.'
       then
         (match old_v with
         | Ivalue(n) -> (Bad, [], [], outp @ [complaint loc "non-int input"])
         | Rvalue(r) -> 
           (try
             let new_mem = update_mem id (Rvalue(float_of_string str)) mem in 
             (Good, new_mem, rest, outp)
           with Failure _ -> (Bad, [], [], outp @ [complaint loc "non-numeric input"]))
         | Error(s)  -> (Bad, [], [], outp @ [s]))
       else
         (match old_v with
         | Ivalue(n) -> 
           (try
             let new_mem = update_mem id (Ivalue(int_of_string str)) mem in 
             (Good, new_mem, rest, outp)
           with Failure _ -> (Bad, [], [], outp @ [complaint loc "non-numeric input"]))
         | Rvalue(r) -> (Bad, [], [], outp @ [complaint loc "non-real input"])
         | Error(s)  -> (Bad, [], [], outp @ [s]))
 
 (* NB: the following routine is complete. *)
 and interpret_write (expr:ast_e) (mem:memory)
                     (inp:string list) (outp:string list)
     : status * memory * string list * string list =
     (*  ok?    new_mem  new_input     new_output *)
   let (ve, new_mem)  = interpret_expr expr mem in
   match ve with
   | Ivalue(n) -> (Good, new_mem, inp, outp @ [string_of_int n])
   | Rvalue(r) -> (Good, new_mem, inp, outp @ [string_of_float r])
   | Error(s)  -> (Bad, [], [], outp @ [s])
 
 and interpret_assign (lhs:string) (rhs:ast_e) (vloc:row_col) (aloc:row_col)
                      (mem:memory) (inp:string list) (outp:string list)
     : status * memory * string list * string list =
     (*  ok?    new_mem  new_input     new_output *)
     (* Interprets an assignment statement. It first looks up the value of the left-hand side variable in the memory. 
        If the lookup is successful, it interprets the right-hand side expression and updates the memory with the new value. 
        If the types of the old value and the new value match, it returns the new memory. 
        If the types don't match or if there's an error in the interpretation of the expression, it returns an error. *)
   match lookup_mem lhs vloc mem with
   | Error(s) -> (Bad, [], [], outp @ [s])
   | old_v -> 
     (let (ve, new_mem)  = interpret_expr rhs mem in
     match (old_v, ve) with
     | (Ivalue(_), Ivalue(_)) -> (Good, (update_mem lhs ve new_mem), inp, outp)
     | (Rvalue(_), Rvalue(_)) -> (Good, (update_mem lhs ve new_mem), inp, outp)
     | (_, Error(s)) -> (Bad, [], [], outp @ [s])
     | _ -> (Bad, [], [], outp @ [complaint aloc "type clash"]))
 
 and interpret_if (loop_count:int) (cond:ast_c) (sl:ast_sl) (mem:memory)
                  (inp:string list) (outp:string list)
     : status * memory * string list * string list =
     (*  ok?    new_mem  new_input     new_output *)
     (* Interprets an if statement. It first interprets the condition. 
        If the condition evaluates to 0 (either as an integer or a real), 
        it skips the statement list and returns the current memory. 
        If there's an error in the interpretation of the condition, 
        it returns an error. Otherwise, it interprets the statement list. *)
   let (vc, new_mem) = interpret_cond cond mem in
   match vc with
   | Ivalue(0) -> (Good, new_mem, inp, outp)
   | Rvalue(0.0) -> (Good, new_mem, inp, outp)
   | Error(s) -> (Bad, [], [], outp @ [s])
   | _ -> interpret_sl loop_count true sl new_mem inp outp
 
 and interpret_do (loop_count:int) (sl:ast_sl) (mem:memory)
                  (inp:string list) (outp:string list)
     : status * memory * string list * string list =
     (*  ok?    new_mem  new_input     new_output *)
     (* Interprets a do statement. It first interprets the statement list with an incremented loop count. 
        If the interpretation is successful, 
        it enters a loop where it continues to interpret the statement list 
        with an incremented loop count until the interpretation is not successful or a done status is returned. 
        If the initial interpretation is not successful or a done status is returned, it skips the loop. *)
   let (ok, new_mem, new_inp, new_outp) = interpret_sl (loop_count + 1) true sl mem inp outp in
   match ok with
   | Good -> 
     (let rec helper loop_count mem inp outp = 
       let (ok, new_mem, new_inp, new_outp) = interpret_sl loop_count false sl mem inp outp in
       match ok with
       | Good -> helper (loop_count + 1) new_mem new_inp new_outp
       | Bad -> (Bad, [], [], new_outp)
       | Done -> (Good, new_mem, new_inp, new_outp)
     in
     helper (loop_count + 2) new_mem new_inp new_outp)
   | Bad -> (Bad, [], [], new_outp)
   | Done -> (Good, new_mem, new_inp, new_outp)
 
 and interpret_check (cond:ast_c) (mem:memory)
                     (inp:string list) (outp:string list)
     : status * memory * string list * string list =
     (*  ok?    new_mem  new_input     new_output *)
     (* Interprets a check statement. It first interprets the condition. 
        If the condition evaluates to 0 (either as an integer or a real), it returns a done status and the current memory. 
        If there's an error in the interpretation of the condition, it returns an error. 
        Otherwise, it returns a good status and the current memory. *)
   let (vc, new_mem) = interpret_cond cond mem in
   match vc with
   | Ivalue(0) -> (Done, new_mem, inp, outp)
   | Rvalue(0.0) -> (Done, new_mem, inp, outp)
   | Error(s) -> (Bad, [], [], outp @ [s])
   | _ -> (Good, new_mem, inp, outp)
 
 and interpret_expr (expr:ast_e) (mem:memory) : value * memory =
  (* Interprets an expression. Depending on the type of the expression, 
     it either directly returns the value and the current memory, 
     or it interprets a sub-expression and returns a new value and the current memory. 
     If there's a type clash in the interpretation of a float or trunc expression, it returns an error. *)
   match expr with 
   | AST_int(num, loc) ->  (Ivalue(int_of_string num), mem)
   | AST_real(num, loc) -> (Rvalue(float_of_string num), mem)
   | AST_id(id, loc) -> ((lookup_mem id loc mem), mem)
   | AST_float(e, loc) -> 
     (let (ve, new_mem) = interpret_expr e mem in
     match ve with
     | Ivalue(n) -> (Rvalue(float_of_int n), new_mem)
     | Rvalue(r) -> (Error(complaint loc "type clash; int expected"), [])
     | Error(s)  -> (Error(s), []))
   | AST_trunc(e, loc) -> 
     (let (ve, new_mem) = interpret_expr e mem in
     match ve with
     | Rvalue(r) -> (Ivalue(int_of_float r), new_mem)
     | Ivalue(n) -> (Error(complaint loc "type clash; real expected"), [])
     | Error(s)  -> (Error(s), []))
   | AST_binop(op, expr1, expr2, loc) -> interpret_cond (op, expr1, expr2, loc) mem
 
 and interpret_cond ((op:string), (lo:ast_e), (ro:ast_e), (loc:row_col)) (mem:memory)
     : value * memory =
     (* Interprets a condition. It first interprets the left and right operands. 
        Depending on the types of the operands, it performs the operation and returns a new value and the current memory. 
        If there's a type clash, it returns an error. If the operation is not supported, it returns an error. 
        If there's an error in the interpretation of either operand, it returns an error. *)
   let (ve1, new_mem) = interpret_expr lo mem in
   let (ve2, new_mem) = interpret_expr ro new_mem in
   match (ve1, ve2) with
   | (Ivalue(n1), Ivalue(n2)) ->
     (match op with 
     | "<" -> (Ivalue(if n1 < n2 then 1 else 0), new_mem)
     | "<=" -> (Ivalue(if n1 <= n2 then 1 else 0), new_mem)
     | ">" -> (Ivalue(if n1 > n2 then 1 else 0), new_mem)
     | ">=" -> (Ivalue(if n1 >= n2 then 1 else 0), new_mem)
     | "==" -> (Ivalue(if n1 == n2 then 1 else 0), new_mem)
     | "!=" -> (Ivalue(if n1 <> n2 then 1 else 0), new_mem)
     | "+" -> (Ivalue(n1 + n2), new_mem)
     | "-" -> (Ivalue(n1 - n2), new_mem)
     | "*" -> (Ivalue(n1 * n2), new_mem)
     | "/" -> 
       (match n2 with
       | 0 -> (Error(complaint loc "divide by zero"), [])
       | _ -> (Ivalue(n1 / n2), new_mem))
     | _ -> (Error(complaint loc "Unsupported operation"), []))
   | (Rvalue(n1), Rvalue(n2)) ->
     (match op with 
     | "<" -> (Rvalue(if n1 < n2 then 1.0 else 0.0), new_mem)
     | "<=" -> (Rvalue(if n1 <= n2 then 1.0 else 0.0), new_mem)
     | ">" -> (Rvalue(if n1 > n2 then 1.0 else 0.0), new_mem)
     | ">=" -> (Rvalue(if n1 >= n2 then 1.0 else 0.0), new_mem)
     | "==" -> (Rvalue(if n1 == n2 then 1.0 else 0.0), new_mem)
     | "!=" -> (Rvalue(if n1 <> n2 then 1.0 else 0.0), new_mem)
     | "+" -> (Rvalue(n1 +. n2), new_mem)
     | "-" -> (Rvalue(n1 -. n2), new_mem)
     | "*" -> (Rvalue(n1 *. n2), new_mem)
     | "/" -> 
       (match n2 with
       | 0.0 -> (Error(complaint loc "divide by zero"), [])
       | _ -> (Rvalue(n1 /. n2), new_mem))
     | _ -> (Error(complaint loc "Unsupported operation"), []))
   | (Error(s), _) -> (Error(s), [])
   | (_, Error(s)) -> (Error(s), [])
   | _ -> (Error(complaint loc "type clash"), [])
 
 (*******************************************************************
     Extra credit: static analysis
 
     A separate pre-pass over the AST to enforce static semantics.
  *******************************************************************)
 
 and dfs (ast:ast_sl) : status * memory * string list =
  (* The dfs function starts a depth-first search (DFS) on the abstract syntax tree (AST) with an initial loop count of 0, 
    an empty memory, and an empty output list. *)
   dfs_sl 0 ast [[]] []
 
 (* iter1 indicates whether this is the first time this statement list
    has executed in a fresh scope.  It's false for the second and
    subsequent iterations of a do loop. *)
 and dfs_sl (loop_count:int) (sl:ast_sl) (mem:memory) (outp:string list)
     : status * memory * string list =
     (*  ok?   new_mem   new_input     new_output *)
     (* The dfs_sl function performs a DFS on a statement list in the AST. It first creates a new scope in the memory. 
        Then it defines a helper function that iterates over the statement list. If the statement list is empty, 
        it ends the current scope and returns a done status, the current memory, and the output list. 
        If the statement list is not empty, it interprets the first statement and continues with the rest of the statement list. 
        If the interpretation of the statement returns a bad status, it stops and returns a bad status, an empty memory, 
        and the output list. Otherwise, it continues with the rest of the statement list. *)
   let current_mem = new_scope mem in
   let rec helper sl mem outp = 
     (match sl with
     | [] -> (Done, (end_scope mem), outp)
     | s::sls -> 
       (let (ok, new_mem, new_outp)  = dfs_s loop_count s mem outp in
       match ok with
       | Bad -> (Bad, [], new_outp)
       | _ -> helper sls new_mem new_outp))
   in
   helper sl current_mem outp
 
 (* NB: the following routine is complete.  You can call it on any
    statement node and it will figure out what more specific case to invoke. *)
 and dfs_s (loop_count:int) (s:ast_s) (mem:memory) (outp:string list)
     : status * memory * string list = 
     (*  ok?    new_mem  new_input     new_output *)
     (* The dfs_s function performs a DFS on a statement. 
        It matches the statement with the expected types of statements 
        (int declaration, real declaration, read, write, assign, if, do, check, error), 
        and calls the appropriate DFS function for each type. *)
   match s with
   | AST_i_dec(id, vloc)  -> dfs_dec id (Ivalue(0)) vloc mem outp
   | AST_r_dec(id, vloc)  -> dfs_dec id (Rvalue(0.0)) vloc mem outp
   | AST_read(id, loc)                -> dfs_read id loc mem outp
   | AST_write(expr)                  -> dfs_write expr mem outp
   | AST_assign(id, expr, vloc, aloc) -> dfs_assign id expr vloc aloc mem outp
   | AST_if(cond, sl)                 -> dfs_if loop_count cond sl mem outp
   | AST_do(sl)                       -> dfs_do loop_count sl mem outp
   | AST_check(cond, cloc)            -> if loop_count > 0
                                           then dfs_check cond mem outp
                                           else (Bad, [], outp @
                                             [complaint cloc "check not inside a loop"])
   | AST_error                        -> raise (Failure "cannot interpret erroneous tree")
 
 and dfs_dec (id:string) (v:value) (vloc:row_col) (mem:memory) (outp:string list)
     : status * memory * string list =
     (*  ok?    new_mem  new_input     new_output *)
     (* The dfs_dec function performs a DFS on a declaration. It inserts the variable into the memory. 
        If the insertion is successful, it returns a good status, the new memory, and the output list. 
        If the insertion is not successful (because the variable is already declared in this scope), 
        it returns a bad status, an empty memory, and the output list. *)
   let (new_mem, ok) = insert_mem id v mem in
   match ok with
   | true -> (Good, new_mem, outp)
   | false ->
   (Bad, [], outp @ [complaint vloc (id ^ " is already declared in this scope")])
 
 and dfs_read (id:string) (loc:row_col) (mem:memory) (outp:string list)
     : status * memory * string list =
     (*  ok?    new_mem  new_input     new_output *)
     (* The dfs_read function looks up a variable in the memory. 
        If the lookup is successful, it returns a good status, the current memory, and the output list. 
        If the lookup is not successful, it returns a bad status, an empty memory, 
        and the output list with an error message *)
   let old_v = lookup_mem id loc mem in
   match old_v with
   | Error(s)  -> (Bad, [], outp @ [s])
   | _ -> (Good, mem, outp)
 
 (* NB: the following routine is complete. *)
 and dfs_write (expr:ast_e) (mem:memory) (outp:string list)
     : status * memory * string list =
     (*  ok?    new_mem  new_input     new_output *)
     (* Similar to above structure *)
   let (ve, new_mem)  = dfs_expr expr mem in
   match ve with
   | Error(s)  -> (Bad, [], outp @ [s])
   | _ -> (Good, new_mem, outp)
 
 and dfs_assign (lhs:string) (rhs:ast_e) (vloc:row_col) (aloc:row_col)
                      (mem:memory) (outp:string list)
     : status * memory * string list =
     (*  ok?    new_mem  new_input     new_output *)
   match lookup_mem lhs vloc mem with
   | Error(s) -> (Bad, [], outp @ [s])
   | old_v -> 
     (let (ve, new_mem)  = dfs_expr rhs mem in
     match (old_v, ve) with
     | (Ivalue(_), Ivalue(_)) -> (Good, (update_mem lhs ve new_mem), outp)
     | (Rvalue(_), Rvalue(_)) -> (Good, (update_mem lhs ve new_mem), outp)
     | (_, Error(s)) -> (Bad, [], outp @ [s])
     | _ -> (Bad, [], outp @ [complaint aloc "type clash"]))
 
 and dfs_if (loop_count:int) (cond:ast_c) (sl:ast_sl) (mem:memory) (outp:string list)
     : status * memory * string list =
     (*  ok?    new_mem  new_input     new_output *)
   let (vc, new_mem) = dfs_cond cond mem in
   match vc with
   | Error(s) -> (Bad, [], outp @ [s])
   | _ -> dfs_sl loop_count sl new_mem outp
 
 and dfs_do (loop_count:int) (sl:ast_sl) (mem:memory) (outp:string list)
     : status * memory * string list =
     (*  ok?    new_mem  new_input     new_output *)
   let (ok, new_mem, new_outp) = dfs_sl (loop_count + 1) sl mem outp in
   match ok with
   | Bad -> (Bad, [], new_outp)
   | _ -> (Good, new_mem, new_outp)
 
 and dfs_check (cond:ast_c) (mem:memory) (outp:string list)
     : status * memory * string list =
     (*  ok?    new_mem  new_input     new_output *)
   let (vc, new_mem) = dfs_cond cond mem in
   match vc with
   | Error(s) -> (Bad, [], outp @ [s])
   | _ -> (Good, new_mem, outp)
 
 and dfs_expr (expr:ast_e) (mem:memory) : value * memory =
   match expr with 
   | AST_int(num, loc) ->  (Ivalue(int_of_string num), mem)
   | AST_real(num, loc) -> (Rvalue(float_of_string num), mem)
   | AST_id(id, loc) -> ((lookup_mem id loc mem), mem)
   | AST_float(e, loc) -> 
     (let (ve, new_mem) = dfs_expr e mem in
     match ve with
     | Ivalue(n) -> (Rvalue(float_of_int n), new_mem)
     | Rvalue(r) -> (Error(complaint loc "type clash; int expected"), [])
     | Error(s)  -> (Error(s), []))
   | AST_trunc(e, loc) -> 
     (let (ve, new_mem) = dfs_expr e mem in
     match ve with
     | Rvalue(r) -> (Ivalue(int_of_float r), new_mem)
     | Ivalue(n) -> (Error(complaint loc "type clash; real expected"), [])
     | Error(s)  -> (Error(s), []))
   | AST_binop(op, expr1, expr2, loc) -> dfs_cond (op, expr1, expr2, loc) mem
 
 and dfs_cond ((op:string), (lo:ast_e), (ro:ast_e), (loc:row_col)) (mem:memory)
     : value * memory =
   let (ve1, new_mem) = dfs_expr lo mem in
   let (ve2, new_mem) = dfs_expr ro new_mem in
   match (ve1, ve2) with
   | (Ivalue(_), Ivalue(_)) -> (Ivalue(1), new_mem)
   | (Rvalue(_), Rvalue(_)) -> (Rvalue(1.0), new_mem)
   | (Error(s), _) -> (Error(s), [])
   | (_, Error(s)) -> (Error(s), [])
   | _ -> (Error(complaint loc "type clash"), [])
   
 (*******************************************************************
     Extra credit: C language translator
 
     Generating an equavelence C source code output.c
     Similar to interpreter -- it will pass the AST again and 
     convert it to a equavelence C source code and
     save it to the [program_name].c
  *******************************************************************)
 
   and c_gen (ast:ast_sl) filename : unit =
    (* Generates C code from an abstract syntax tree (AST) and saves it to a file. 
       It constructs the main function, generates C code for the AST, and writes the result to a file. *)
     let save_to_file (grog_name:string) (c_source:string) : unit =
       let filename = ((hd (split (regexp "[.]+") grog_name)) ^ ".c") in
       let oc = open_out filename in
       output_string oc c_source;
       close_out oc
     in
     let c_main = "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\nint main() {\n" in
     let inp_buffer = "    char input[1000];\n" in
     let (_, _, outp) = c_gen_sl 1 ast [[]] "" in
     let grog_name = if (Array.length Sys.argv) = 2 then Sys.argv.(1) else filename in
     save_to_file grog_name (c_main ^ inp_buffer ^ (if outp <> "" then (outp ^ "    printf(\"\\n\");\n") else "") ^ "}\n")
   
   and c_gen_sl (loop_count:int) (sl:ast_sl) (mem:memory) (outp:string)
       : status * memory * string =
    (* Generates C code for a list of statements in the AST. It creates a new scope in the memory, 
       then recursively processes each statement. For each statement, it generates the C code and updates the memory. 
       If the statement is processed successfully, it continues with the next statement. If there is an error, 
       it stops processing and returns an error status. Finally, it ends the current scope in the memory and returns the status, 
       the updated memory, and the generated C code. *)
     let current_mem = new_scope mem in
     let rec helper sl mem outp = 
       (match sl with
       | [] -> (Done, (end_scope mem), outp)
       | s::sls -> 
         (let (ok, new_mem, new_outp)  = c_gen_s loop_count s mem outp in
         match ok with
         | Bad -> (Bad, [], "")
         | _ -> helper sls new_mem new_outp))
     in
     helper sl current_mem outp
   
   and c_gen_s (loop_count:int) (s:ast_s) (mem:memory) (outp:string)
       : status * memory * string = 
       (* Generates C code for a given statement in the AST. Depending on the type of statement, 
          it calls the appropriate function to generate the C code. 
          If the statement is erroneous, it raises a failure. *)
     match s with
     | AST_i_dec(id, vloc)  -> c_gen_dec loop_count id (Ivalue(0)) mem outp
     | AST_r_dec(id, vloc)  -> c_gen_dec loop_count id (Rvalue(0.0)) mem outp
     | AST_read(id, loc)                -> c_gen_read loop_count id loc mem outp
     | AST_write(expr)                  -> c_gen_write loop_count expr mem outp
     | AST_assign(id, expr, vloc, aloc) -> c_gen_assign loop_count id expr mem outp
     | AST_if(cond, sl)                 -> c_gen_if loop_count cond sl mem outp
     | AST_do(sl)                       -> c_gen_do loop_count sl mem outp
     | AST_check(cond, cloc)            -> c_gen_check loop_count cond mem outp
     | AST_error                        -> raise (Failure "cannot convert erroneous tree to C")
   
   and c_gen_dec (loop_count:int) (id:string) (v:value) (mem:memory) (outp:string)
       : status * memory * string = 
       (* Generates C code for a variable declaration. Inserts the variable into the memory, 
          then generates C code based on the type of the value (integer or double). 
          If the value is an error, it returns a bad status and an empty memory and output string. *)
     let t = (String.make (loop_count * 4) ' ') in
     let (new_mem, _) = insert_mem id v mem in
     match v with
     | Ivalue(n) -> (Good, new_mem, (outp ^ t ^ "int " ^ id ^ " = " ^ (string_of_int n) ^ ";\n"))
     | Rvalue(r) -> (Good, new_mem, (outp ^ t ^ "double " ^ id ^ " = " ^ (string_of_float r) ^ ";\n"))
     | Error(_) -> (Bad, [], "")
   
   and c_gen_read (loop_count:int) (id:string) (loc:row_col) (mem:memory) (outp:string)
       : status * memory * string =
       (* This function `c_gen_read` generates C code for a read statement in the AST. 
          It matches the type of the value and generates the C code. *)
     let t = (String.make (loop_count * 4) ' ') in
     let read = (t ^ "scanf(\"%999s\", input);\n") in
     let old_v = lookup_mem id loc mem in
     match old_v with
     | Ivalue(_) -> (Good, mem, (outp ^ read ^ t ^ id ^ " = strtol(input, NULL, 10);\n"))
     | Rvalue(_) -> (Good, mem, (outp ^ read ^ t ^ id ^ " = strtod(input, NULL);\n"))
     | Error(_) -> (Bad, [], "")
   
   and c_gen_write (loop_count:int) (expr:ast_e) (mem:memory) (outp:string)
       : status * memory * string =
      (* Generates C code for a write statement. It generates the C code for the expression to be written, 
         then depending on the type of the value (integer or double), it generates a printf statement to print the value. 
         If the value is an error, it returns a bad status and an empty memory and output string. *)
     let t = (String.make (loop_count * 4) ' ') in
     let (ve, stre) = c_gen_expr expr mem in
     match ve with
     | Ivalue(_) -> (Good, mem, (outp ^ t ^ "printf(\"%d \", " ^ stre ^ ");\n"))
     | Rvalue(_) -> (Good, mem, (outp ^ t ^ "printf(\"%f \", " ^ stre ^ ");\n"))
     | Error(_) -> (Bad, [], "")
   
   and c_gen_assign (loop_count:int) (lhs:string) (rhs:ast_e) (mem:memory) (outp:string)
       : status * memory * string =
      (* Generates C code for an assignment statement. It generates the C code for the right-hand side expression, 
         then creates an assignment statement in C with the left-hand side variable and the right-hand side expression. *)
     let (_, stre)  = c_gen_expr rhs mem in
     (Good, mem, (outp ^ (String.make (loop_count * 4) ' ') ^ lhs ^ " = " ^ stre ^ ";\n"))
   
   and c_gen_if (loop_count:int) (cond:ast_c) (sl:ast_sl) (mem:memory) (outp:string)
       : status * memory * string =
      (* Generates C code for a do-while loop. It generates the C code for the statements inside the loop, 
         then combines these into a complete do-while loop in C. *)
     let t = (String.make (loop_count * 4) ' ') in
     let (_, strc) = c_gen_cond cond mem in
     let current_outp = (outp ^ t ^ "if (" ^ strc ^ ") {\n") in
     let (_, new_mem, new_outp) = c_gen_sl (loop_count + 1) sl mem current_outp in
     (Good, new_mem, (new_outp ^ t ^ "}\n"))
   
   and c_gen_do (loop_count:int) (sl:ast_sl) (mem:memory) (outp:string)
       : status * memory * string =
     let t = (String.make (loop_count * 4) ' ') in
     let current_outp = (outp ^ t ^ "while (1) {\n") in
     let (_, new_mem, new_outp) = c_gen_sl (loop_count + 1) sl mem current_outp in
     (Good, new_mem, (new_outp ^ t ^ "}\n"))
   
   and c_gen_check (loop_count:int) (cond:ast_c) (mem:memory) (outp:string)
       : status * memory * string =
      (* Generates C code for a check statement. It generates the C code for the condition, 
         then creates an if statement in C that checks if the condition is not true. 
          If the condition is not true, it breaks out of the current loop. *)
     let t = (String.make (loop_count * 4) ' ') in
     let tt = (String.make ((loop_count + 1) * 4) ' ') in
     let (_, strc) = c_gen_cond cond mem in
     let new_outp = (outp ^ t ^ "if (!(" ^ strc ^ ")) {\n") in
     (Good, mem, (new_outp ^ tt ^ "break;\n" ^ t ^ "}\n"))
   
   and c_gen_expr (expr:ast_e) (mem:memory) : value * string =
    (* Generates C code for an expression in the AST. Depending on the type of expression, 
       it generates the appropriate C code. For an integer or real number, it simply returns the number. For a variable, 
       it looks up the value in the memory. For a float or truncation operation, 
       it generates the C code for the expression and then applies the operation. 
       For a binary operation, it generates the C code for the condition and then applies the operation. *)
     match expr with 
     | AST_int(num, loc) ->  (Ivalue(int_of_string num), num)
     | AST_real(num, loc) -> (Rvalue(float_of_string num), num)
     | AST_id(id, loc) -> ((lookup_mem id loc mem), id)
     | AST_float(e, loc) -> 
       (let (_, stre) = c_gen_expr e mem in
       (Rvalue(1.0), ("(double) " ^ stre)))
     | AST_trunc(e, loc) -> 
       (let (_, stre) = c_gen_expr e mem in
       (Ivalue(1), ("(int) " ^ stre)))
     | AST_binop(op, expr1, expr2, loc) -> 
       (let (vc, strc) = c_gen_cond (op, expr1, expr2, loc) mem in
       (vc, ("(" ^ strc ^ ")")))
   
   and c_gen_cond ((op:string), (lo:ast_e), (ro:ast_e), (loc:row_col)) (mem:memory) : value * string =
    (* Generates C code for a condition in the AST. It generates the C code for the left and right operands, 
       then combines these with the operator to form a complete condition in C. *)
     let (ve, stre1) = c_gen_expr lo mem in
     let (_, stre2) = c_gen_expr ro mem in
     (ve, (stre1 ^ " " ^ op ^ " " ^ stre2));;
 
 (*******************************************************************
     Testing
  *******************************************************************)
 
 let sum_ave_prog =
 "   read int a read int b int sum := a + b
     write sum write float(sum) / 2.0";;
 
 let primes_prog =
 "   read int n
     int cp := 2
     do 
         check n > 0
         int found := 0
         int cf1 := 2
         int cf1s := cf1 * cf1
         do
             check cf1s <= cp
             int cf2 := 2
             int pr := cf1 * cf2
             do
                 check pr <= cp
                 if pr == cp
                     found := 1
                 fi
                 cf2 := cf2 + 1
                 pr := cf1 * cf2
             od
             cf1 := cf1 + 1
             cf1s := cf1 * cf1
         od
         if found == 0
             write cp
             n := n - 1
         fi
         cp := cp + 1
     od";;
 
 let gcd_prog =
 "   read int a
     read int b
     do 
         check a != b
         if a > b
             a := a - b
         fi
         if b > a
             b := b - a
         fi
         if a == b
             write a
         fi
     od";;
 
 let sqrt_prog =
 "   read real d
     real l := d / 2.0
     do
         check l * l > d
         l := l / 2.0
     od
     real h := 2.0 * l
     real err := d - (l * l)
     if err < 0.0 err := 0.0 - err fi
     do
         check err > 1.e-10
         real a := (l + h) / 2.0
         if (a * a) < d l := a fi
         if (a * a) > d h := a fi
         err := d - (l * l)
         if err < 0.0 err := 0.0 - err fi
     od
     write l";;
 
   let ecg_ast prog =
     ast_ize_prog (parse ecg_parse_table prog);;
 
   let ecg_run prog inp =
    let ast = ecg_ast prog in
    let (dfs_ok, _, dfs_outp) = dfs ast in
    match dfs_ok with
    | Done -> interpret ast inp
    | _ -> ((fold_left (str_cat " ") "" dfs_outp) ^ "\n")
     
 
   let sum_ave_parse_tree = parse ecg_parse_table sum_ave_prog;;
   let sum_ave_syntax_tree = ast_ize_prog sum_ave_parse_tree;;
 
   let primes_parse_tree = parse ecg_parse_table primes_prog;;
   let primes_syntax_tree = ast_ize_prog primes_parse_tree;;
 
   let gcd_parse_tree = parse ecg_parse_table gcd_prog;;
   let gcd_syntax_tree = ast_ize_prog gcd_parse_tree;;
 
   let show_ast prog = pp_p (ast_ize_prog (parse ecg_parse_table prog));;
 
 let main () =
 (* Code below expects there to be a single command-line argument, which
    names a file containing an ecg program.  It runs that program, taking
    input from stdin.  It does NOT run interactively: it sucks up _all_
    input and runs only once it reaches end-of-file. *)
 
   let read_prog () =
     if (Array.length Sys.argv) <> 2
       then raise (Failure ("usage: " ^ Sys.argv.(0) ^ " prog_file_name"))
       else
         let ic = open_in Sys.argv.(1) in
         let lines = ref [] in
         try
           while true do
             lines := input_line ic :: !lines;
           done; ""
         with
           End_of_file ->
             String.concat "\n" (rev !lines) in
 
   let read_input () =
     let lines = ref [] in
       try
         while true do
           lines := read_line () :: !lines;
         done; ""
       with
         End_of_file ->
           String.concat "\n" (rev !lines) in
 
   let prog = read_prog() in
   let input = read_input() in
   print_string (ecg_run prog input);;
 
 (* Execute function "main" iff run as a stand-alone program. *)
 if !Sys.interactive then () else main ();;
 