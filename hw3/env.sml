(* 
* Name: William Dickson 
* Class: CSE 4102
* Instructor: Jeff Meunier
* Due Date: Feb 21, 2016
*)

exception NameNotBound of string;

type 'a Env = string -> 'a;

(* env_new: creates a new blank environment *)
fun env_new () : 'a Env = fn (name:string) => raise NameNotBound name;

(* env_bind: binds a name and value to the environment *)
fun env_bind (env: 'a Env) (name:string) (value): 'a Env = 
  fn (input:string) =>
    if input = name then value
    else env input

(* env_lookup: retreives the value of a binding in the environment *)
fun env_lookup (env: 'a Env) (name) = env name

(* Test Cases *)
(*
val e1 = env_new () : int Env;
val e2 = env_bind e1 "x" 100;
val e3 = env_bind e2 "y" 200;
env_lookup e3 "x";
env_lookup e3 "y";
env_lookup e3 "z";
*)
