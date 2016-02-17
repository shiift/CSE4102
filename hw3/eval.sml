use "env.sml";
Control.Print.printDepth := 32;

datatype Expr = Bool of bool
              | Int of int
              | Add of Expr * Expr
              | Cond of Expr * Expr * Expr
              | Sym of string
              | Let of (string * Expr) list * Expr
              | Def of string * Expr
              | Fun of Expr * Expr
              | App of Expr * Expr
              | Seq of Expr list
              | Disp of Expr
              | Nothing;

fun e2s (Bool b) = "Bool(" ^ (Bool.toString b) ^ ")"
  | e2s (Int i)  = "Int(" ^ (Int.toString i) ^ ")"
  | e2s (Cond (cond, conseq, alt)) = "If(" ^ (e2s cond) ^ "," ^ (e2s conseq) ^
    "," ^ (e2s alt) ^ ")"
  | e2s (Sym s) = "Sym(" ^ s ^ ")"
  | e2s (Let (l, e)) = "Let(list," ^ (e2s e) ^ ")"
  | e2s (Def (s, e)) = "Def(" ^ s ^ "," ^ (e2s e) ^ ")"
  | e2s (Fun (e, x)) = "Fun(" ^ (e2s e) ^ "," ^ (e2s x) ^ ")"
  | e2s (App (f, p)) = "App(" ^ (e2s f) ^ "," ^ (e2s p) ^ ")"
  | e2s (Seq s) = "Seq(" ^ (e2s_seq_helper s) ^ ")"
  | e2s (Disp d) = "Disp(" ^ (e2s d) ^ ")"
  | e2s (Nothing) = "(null)"
  | e2s x = "(unknown)"
    and e2s_seq_helper (nil:(Expr list)) = "(end)"
      | e2s_seq_helper (s:(Expr list)) = e2s(hd(s)) ^ "," ^ e2s_seq_helper(tl(s));
