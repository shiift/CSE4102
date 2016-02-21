use "env.sml";
Control.Print.printDepth := 32;

exception EvaluationError of string;
exception CondError of string;

(* Expression Datatype with Constructors *)

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

(* Expression to String Function *)

fun e2s (Bool b) = "Bool(" ^ (Bool.toString b) ^ ")"
  | e2s (Int i)  = "Int(" ^ (Int.toString i) ^ ")"
  | e2s (Add (x, y)) = "Add(" ^ (e2s x) ^ "," ^ (e2s y) ^ ")"
  | e2s (Cond (cond, conseq, alt)) = "Cond(" ^ (e2s cond) ^ "," ^ (e2s conseq) ^
    "," ^ (e2s alt) ^ ")"
  | e2s (Sym s) = "Sym(" ^ s ^ ")"
  | e2s (Let (l, e)) = "Let([" ^ (e2s_let_helper l) ^ "]," ^ (e2s e) ^ ")"
  | e2s (Def (s, e)) = "Def(" ^ s ^ "," ^ (e2s e) ^ ")"
  | e2s (Fun (e, x)) = "Fun(" ^ (e2s e) ^ "," ^ (e2s x) ^ ")"
  | e2s (App (f, p)) = "App(" ^ (e2s f) ^ "," ^ (e2s p) ^ ")"
  | e2s (Seq s) = "Seq([" ^ (e2s_seq_helper s) ^ "])"
  | e2s (Disp d) = "Disp(" ^ (e2s d) ^ ")"
  | e2s (Nothing) = "(null)"
    and e2s_let_helper (nil:((string * Expr) list)) = ""
      | e2s_let_helper ((id:string, e:Expr)::tail) =
        if ((length tail) = 0) then id ^ ":" ^ (e2s e)
        else id ^ ":" ^ (e2s e) ^ "," ^ (e2s_let_helper tail)
    and e2s_seq_helper (s:(Expr list)) =
        if ((length s) = 1) then (e2s(hd s))
        else (e2s(hd s)) ^ "," ^ (e2s_seq_helper(tl s));

(* Evaluation Function *)

fun eval env (Sym s) = (env,(env s))
  | eval env (Add (x, y)) =
    (case ((eval env x), (eval env y)) of
          ((_,Int c),(_,Int d)) => (env, Int (c + d))
        | (c, d) => raise EvaluationError "Add")
  | eval env (Cond (cond,cons,alt)) =
    (case (eval env cond) of
          (_,Bool b) => if b then (eval env cons)
                        else (eval env alt)
        | (_) => raise CondError "Cond Requires Bool")
  | eval env (Let (b, e)) =
    let
        val lenv = (eval_let_helper env b)
        val (_, v) = (eval lenv e)
    in
        (env, v)
    end
  | eval env expr = (env, expr)
    and eval_let_helper env [] = env
      | eval_let_helper env ((id, v)::tail) =
        (eval_let_helper (env_bind env id v) tail);
