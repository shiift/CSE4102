(* 
* Name: William Dickson 
* Class: CSE 4102
* Instructor: Jeff Meunier
* Due Date: Feb 21, 2016
*)

use "env.sml";
Control.Print.printDepth := 32;

exception InvalidEvaluationError of string;
exception InvalidCondError of string;
exception InvalidParameterError of string;
exception InvalidApplicationError of string;

(* Expression Datatype with Constructors *)
datatype Expr = Bool of bool (* boolean *)
              | Int of int (* integer *)
              | Add of Expr * Expr (* adds two expressions on evaluation *)
              | Cond of Expr * Expr * Expr (* given in the form (cond, conseq,
                alt *)
              | Sym of string (* symbol that is used to assign names to values
                in the environment *)
              | Let of (string * Expr) list * Expr (* contains a list of (id,
                value) tuples with an expression to evaluate *)
              | Def of string * Expr (* definition that makes a new binding in
                the environment when evaluated *)
              | Fun of Expr * Expr (* function, when evaluated, returns a
                closure *)
              | App of Expr * Expr (* application of a function, when evaluated,
                runs the function *)
              | Seq of Expr list (* a list of expressions *)
              | Disp of Expr (* displays the string format of the expression *)
              | Clos of Expr * Expr * Expr Env (* the function containing the
              environment where the function was defined *)
              | Nothing; (* the null datatype *)

(* Expression to String Function *)

fun e2s (Bool b) = "Bool(" ^ (Bool.toString b) ^ ")"
  | e2s (Int i)  = "Int(" ^ (Int.toString i) ^ ")"
  | e2s (Add (x, y)) = "Add(" ^ (e2s x) ^ "," ^ (e2s y) ^ ")"
  | e2s (Cond (cond, conseq, alt)) = "Cond(" ^ (e2s cond) ^ "," ^ (e2s conseq) ^
    "," ^ (e2s alt) ^ ")"
  | e2s (Sym s) = "Sym(" ^ s ^ ")"
  (* Let needs a helper that displays each id:value pair in the list *)
  | e2s (Let (l, e)) = "Let([" ^ (e2s_let_helper l) ^ "]," ^ (e2s e) ^ ")"
  | e2s (Def (s, e)) = "Def(" ^ s ^ "," ^ (e2s e) ^ ")"
  | e2s (Fun (e, x)) = "Fun(" ^ (e2s e) ^ "," ^ (e2s x) ^ ")"
  | e2s (App (f, p)) = "App(" ^ (e2s f) ^ "," ^ (e2s p) ^ ")"
  (* Seq needs a helper that displays each expression in the list *)
  | e2s (Seq s) = "Seq([" ^ (e2s_seq_helper s) ^ "])"
  | e2s (Disp d) = "Disp(" ^ (e2s d) ^ ")"
  | e2s (Clos (e, x, _)) = "Clos(" ^ (e2s e) ^ "," ^ (e2s x) ^ ")"
  | e2s (Nothing) = "(null)"
    (* Let helper pattern matches on possible formats and recursively displays
    * the id:value pairs through the end of the list *)
    and e2s_let_helper ([]) = ""
      | e2s_let_helper ((id:string, e:Expr)::[]) = (id ^ ":" ^ (e2s e))
      | e2s_let_helper ((id:string, e:Expr)::tail) =
            (id ^ ":" ^ (e2s e) ^ "," ^ (e2s_let_helper tail))
    (* Seq helper pattern matches on possible formats and recursively displays
    * the expressions through the end of the list *)
    and e2s_seq_helper ([]) = ""
      | e2s_seq_helper (head::[]) = (e2s head)
      | e2s_seq_helper (head::tail) =
            (e2s head) ^ "," ^ (e2s_seq_helper tail);

(* Evaluation Function *)

fun eval (env:Expr Env) (Sym s) = (env, (env s))
  (* Add: evaluates the two input expressions, then adds the results *)
  | eval env (Add (x, y)) =
    (case ((eval env x), (eval env y)) of
          ((_,Int c),(_,Int d)) => (env, Int (c + d))
        | (c, d) => raise InvalidEvaluationError "Add")
  (* Cond: evaluates the cond, if the cond returns true, then evaluate cons,
  * else evaluate the alt *)
  | eval env (Cond (cond,cons,alt)) =
    (case (eval env cond) of
          (_,Bool b) => if b then (eval env cons)
                        else (eval env alt)
        | (_) => raise InvalidCondError "Cond Requires Bool")
  (* Let: use the helper function to setup the environment with the bindings
  * given, then evaluate the expression and return the result with the original
  * environment *)
  | eval env (Let (b, e)) =
    let
        val env2 = (eval_let_helper env b)
        val (_,v) = (eval env2 e)
    in
        (env, v)
    end
  (* Def: bind the definition to the environment and return it, return Nothing
  * as the value *)
  | eval env (Def (s, e)) = ((env_bind env s e), Nothing)
  (* Seq: use the helper to evaluate each expression in the sequence *)
  | eval env (Seq s) = (eval_seq_helper env s)
  (* Disp: print out the e2s of the expression and return Nothing *)
  | eval env (Disp d) = ((print ((e2s d) ^ "\n")); (env, Nothing))
  (* Fun: create and return a closure containing the environment at the time of
  * function creation *)
  | eval env (Fun (e, x)) = (env, Clos (e, x, env))
  (* App: find the closure for the function, run the closure with the parameters
  * provided, then return the return value of the function with the original
  * environment *)
  | eval env (App (func, p)) = 
    (case (eval env func) of
          (_, Clos (arg, steps, envc)) =>
            (case arg of
                  (Sym s) =>
                    let
                      val (_, param) = (eval env p)
                      val (_, return) = (eval (env_bind envc s param) steps)
                    in
                      (env, return)
                    end
                | (_) => raise InvalidParameterError "Parameter must be a Symbol")
        | (_) => raise InvalidApplicationError "Application must be applied to a Fun")
  (* catch all for other expression types *)
  | eval env expr = (env, expr)
    (* Let helper binds all of the id:value pairs in the let and returns the new
    * resulting environment *)
    and eval_let_helper env [] = env
      | eval_let_helper env ((id, v)::tail) =
        (eval_let_helper (env_bind env id v) tail)
    (* Seq helper recursively evaluates all of the expressions in the sequence
    * and returns the result of the final expression in the sequence *)
    and eval_seq_helper env [] = (env, Nothing) (* edge case catch *)
      | eval_seq_helper env (head::[]) = (eval env head)
      | eval_seq_helper env (expr::tail) =
        let
          val (e,_) = (eval env expr)
        in
          (eval_seq_helper e tail)
        end;
