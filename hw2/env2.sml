(* 
* Name: William Dickson 
* Class: CSE 4102
* Instructor: Jeff Meunier
* Due Date: Feb 9, 2016
*)

exception NameNotBound of string;

type Env = string -> int;

fun env_new () : Env = fn (name:string) => raise NameNotBound name;

fun env_bind (env:Env) (name:string) (value:int): Env = 
  fn (input:string) =>
    if input = name then value
    else env input

fun env_lookup (env:Env) (name:string) = env name

(* Test Cases *)
val e1 = env_new ();
val e2 = env_bind e1 "x" 100;
val e3 = env_bind e2 "y" 200;
env_lookup e3 "x";
env_lookup e3 "y";
env_lookup e3 "z";
