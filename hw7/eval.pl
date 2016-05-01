/* Creates a new environment which is stored
as an empty list */
env_new([]).

/* Binds a value to a name in the environment and
return this in the last parameter */
env_bind(Env, Name, Value, Env_) :- Env_ = [Name=Value|Env].

/* Looks for a value for a given name in the environment and
returns the value or throws a name_not_bound exception */
env_lookup([], Name, _) :- throw(name_not_bound(Name)).
env_lookup([Name=Value|_], Name, Value) :- !.
env_lookup([_|Env], Name, Value) :- env_lookup(Env, Name, Value).

% evaluator predicates

% +(X, Y) returns the sum of X and Y
eval(+(X, Y), Env, Return, ReturnEnv) :- !,
	eval(X, Env, EvalX, EnvX),
	eval(Y, EnvX, EvalY, ReturnEnv),
	Return is EvalX + EvalY.

% app(Fun, Arg) applys arguments to a function
eval(app(clos(Param, Body, ClosEnv), Arg), Env, Return, Env) :- !,
	eval(Arg, ClosEnv, ArgVal, ArgEnv),
	env_bind(ArgEnv, Param, ArgVal, BindEnv),
	eval(Body, BindEnv, Return, _).
eval(app(Fun, _), _, _, _) :- throw(not_an_abstraction(Fun)).

/* disp(Expr) evaluates the expression and prints the evaulated
value, the returned values are the unmodified input values */
eval(disp(Expr), Env, Expr, Env) :- !,
	print(Expr).

% fun(Param, Body) creates a closure from a function
eval(fun(Param, Body), Env, clos(Param, Body, Env), Env) :- !.

% if(Cond, Conseq, Alt) evaluates an if, then, else
eval(if(Cond, Conseq, Alt), Env, Return, ReturnEnv) :- !,
	eval(Cond, Env, Boolean, CondEnv),
	(Boolean -> eval(Conseq, CondEnv, Return, ReturnEnv) ;
		eval(Alt, CondEnv, Return, ReturnEnv)).

/* def(Name, Expr) defines a new binding for a name to an expression
(that must be evaluated before binding) */
eval(def(Name, Expr), Env, Return, ReturnEnv) :- !,
	eval(Expr, Env, Return, ExprEnv),
	env_bind(ExprEnv, Name, Return, ReturnEnv).

/* let(Bindings, Body) adds bindings to an environment, evaluates the
body with the environment, then returns the result with the original
environment */
eval(let(Bindings, Body), Env, Return, Env) :- !,
	eval_let(Bindings, Env, LetEnv), % eval_let helper function below
	eval(Body, LetEnv, Return, _).

/* seq(Exprs) evaluates a list of expressions and returns the final
return value as well as the modified environment. */
eval(seq([]), Env, nothing, Env) :- !.
eval(seq([Expr|[]]), Env, Return, ReturnEnv) :- !,
	eval(Expr, Env, Return, ReturnEnv).
eval(seq([Expr|Tail]), Env, Return, ReturnEnv) :- !,
	eval(Expr, Env, _, ExprEnv),
	eval(seq(Tail), ExprEnv, Return, ReturnEnv).

% special cases for built-in control predicates (from documentation)
eval(true, Env, true, Env) :- !.
eval(false, Env, fail, Env) :- !.
eval(repeat, Env, repeat, Env) :- !.
eval(fail, Env, fail, Env) :- !.

/* evaluate atoms and return the bound value to the environment if it
is successful */
eval(X, Env, V, Env) :- atom(X),
	!,
	env_lookup(Env, X, V).

% catch-all
eval(X, Env, X, Env).
% alias for simplicity
eval(Expr, Result) :- eval(Expr, [], Result, _).

/* let helper adds bindings to the environment until it is out of bindings
to add */
eval_let([], Env, Env) :- !.
eval_let([[Name|Expr]|Tail], Env, ReturnEnv) :- !,
	eval(Expr, Env, ExprVal, EvalEnv),
	env_bind(EvalEnv, Name, ExprVal, BindEnv),
	eval_let(Tail, BindEnv, ReturnEnv).