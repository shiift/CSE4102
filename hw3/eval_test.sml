(* 
* Name: William Dickson 
* Class: CSE 4102
* Instructor: Jeff Meunier
* Due Date: Feb 21, 2016
*)

use "eval.sml";

val env = env_new () : Expr Env;

val expr = (Bool true);
val display = (Disp expr);
val (env,result) = (eval env expr);
val result = (Disp result); (* Bool true *)
print "\n";

val expr = (Int 100);
val display = (Disp expr);
val (env,result) = (eval env expr);
val result = (Disp result); (* Int 100 *)
print "\n";

val expr = (Add ((Int 100),(Int 200)));
val display = (Disp expr);
val (env,result) = (eval env expr);
val result = (Disp result); (* 100+200=300 *)
print "\n";

val expr = (Cond ((Bool true),(Int 100),(Int 200)));
val display = (Disp expr);
val (env,result) = (eval env expr);
val result = (Disp result); (* Int 100 *)
print "\n";

val expr = (Def ("x", Int 1));
val display = (Disp expr);
val (env,result) = (eval env expr);
val result = (Disp result); (* Binds X to 1 *)
print "\n";

val expr = (Let ([("x",Int 100), ("y",Int 200)], Add ((Sym "x"), (Sym "y"))));
val display = (Disp expr);
val (env,result) = (eval env expr);
val result = (Disp result); (* X+Y = 300 *)
print "\n";

val expr = (Sym "x");
val display = (Disp expr);
val (env,result) = (eval env expr);
val result = (Disp result); (* X=1 *)
print "\n";

val func = (Fun (Sym "x", (Add (Sym "x", Int 1))));
val expr = func;
val display = (Disp expr);
val (env,result) = (eval env expr);
val result = (Disp result); (* Clos (Sym "x"..., fn) *)
print "\n";

val expr = (App (func, Int 100));
val display = (Disp expr);
val (env,result) = (eval env expr);
val result = (Disp result); (* Int 101 *)
print "\n";

val expr = (Seq [Def ("y", Int 500), App(func, Sym "y")]);
val display = (Disp expr);
val (env,result) = (eval env expr);
val result = (Disp result); (* Int 501 *)
print "\n";
