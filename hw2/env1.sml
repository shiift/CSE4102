(* 
* Name: William Dickson 
* Class: CSE 4102
* Instructor: Jeff Meunier
* Due Date: Feb 9, 2016
*)

exception NameNotBound of string;

type Env = (string * int) list;

fun env_new() : Env = nil;

fun env_bind (env:Env) (name:string) (value:int) : Env = ((name, value)::env);

fun env_lookup (nil:Env) (name:string) = raise NameNotBound name
  | env_lookup (env:Env) (name:string) =
  let 
    val (nm:string, vl:int) = hd(env);
    val envs:Env = tl(env);
  in
    if nm = name then
      vl
    else
      env_lookup envs name
  end
  ;

(* Test Cases *)
val e1  =   env_new ();
val e2  =   env_bind    e1  "x" 100;
val e3  =   env_bind    e2  "y" 200;
env_lookup  e3  "x";
env_lookup  e3  "y";
env_lookup  e3  "z";
