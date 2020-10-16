(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* Lambda calculus ~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
Require Import Arith.

(*
  Some notes from [Pie02]

  To save writing too many parentheses, it exists two conventions when
  writing lambda-terms in linear form.  First, application associates
  to the left -- that is, `s t u` stands for the same tree as `(s t)
  u`.  Second, the bodies of abstraction are taken to extend as far to
  the right as possible, so that, for example, `λx. λy. x y x` stands
  for the same tree as `λx. (λy. ((x y) x))`.

  This representation avoids renaming by using De Bruijn notation.  It
  replaces named variables by natural numbers, where the number `k`
  stands for "the variable bound by the `k`'th enclosing λ". For
  example, the ordinary term `λx.x` corresponds to the nameless term
  `λ.0`, while `λx.λy. x (y x)` corresponds to `λ.λ. 1 (0 1)`.
*)

(* Syntax definition -- p.84 *)
Definition id: Set := nat.

Inductive term: Set :=
| TmVar: id -> term
| TmAbs: term -> term
| TmApp: term -> term -> term.

(* Some definitions *)
Definition c0: term := (TmAbs (TmAbs (TmVar 0))).
Definition c2: term := (TmAbs (TmAbs (TmApp (TmVar 1)(TmApp (TmVar 1) (TmVar 0))))).
Definition plus: term := (TmAbs (TmAbs (TmAbs (TmAbs (TmApp (TmApp (TmVar 3) (TmVar 1)) (TmApp (TmApp (TmVar 2) (TmVar 1)) (TmVar 0))))))).
Definition c2': term := (TmApp (TmApp plus c2) c0).

(*
Shifting as in 6.2.1

Renumber free variables in a term `t` during a substitution so the
free variables remains free after the substitution.  For instance
consider the following substitution:

 ╭───────────────┬─── `2` should be substitued by `λ.(0 1)`
 │               │    in the expression `λ.2`
[2 ↦ λ.(0 1)] λ.2

We could say for `2` that

╭──┬──────── 0 is a bound variable by λ
λ.(0 1)
     ╰────── 1 is a free variable

And it should remain like this after substitution.

When a substitution goes under a λ-abstraction (i.e., `λ.2`), the
context in which the substitution takes place (i.e., `λ.(0 1)`)
becomes one variable longer that the original.  Thus we need to
increment free variables in the substitued term up by one so they
remain free.  But we should not increment bound variable.  So the
result of the previous substitution is:

[2 ↦ λ.(0 1)] λ.2
= λ.λ(0 2)
    ╰─┴╴│──── The first variable *remains* a bound variable to `λ`
        ╰──── The second variable *remains* free

We use the following notation `↑c,d(k)` for increment variables of `k`
by `d` level with `c` is the index of the first free variable.  All
identifiers `k < c` in `t` are bound a so should not be shifted, while
identifiers `k >= c` in `t` are free and should be shifted.

Shift term by `d`
↑c,d(k)     = k              if k < c    [1]
↑c,d(k)     = k + d          if k >= c   [2]
↑c,d(λ.t)   = λ.↑c+1,d(t)               [3]
↑c,d(t1 t2) = ↑c,d(t1) ↑c,d(t2)        [4]

For instance:

↑0,1 `λ.(0 1)` =3⇒ `λ.`↑1,1 `(0 1)`
                =4⇒ `λ.(`↑1,1`0` ↑1,1`1` `)`
                =1⇒ `λ.(0` ↑1,1`1` `)`
                =2⇒ `λ.(0 2)`

We also write ↑1(t) for ↑0,1(t)

Implementation as in 7.2

*)
Fixpoint shift_walk (c: nat) (d: nat) (t: term): term :=
  match t with
  | TmVar k => if lt_dec k c
               then TmVar k        (* k < c  *)
               else TmVar (k + c)  (* k >= c *)
  | TmAbs t => TmAbs (shift_walk (S c) d t)
  | TmApp t1 t2 =>
    let t1' := shift_walk c d t1 in
    let t2' := shift_walk c d t2 in
    TmApp t1' t2'
  end.
Definition shift (d: nat) (t: term)   := shift_walk 0 d t.

Eval compute in (shift 1 (TmAbs (TmApp (TmVar 0) (TmVar 1)))).
Eval compute in (shift 2 (TmAbs (TmAbs (TmApp (TmVar 1)
                                              (TmApp (TmVar 0)
                                                     (TmVar 2)))))).
Eval compute in (shift 2 (TmAbs (TmApp (TmApp (TmVar 0) (TmVar 1))
                                       (TmAbs (TmApp (TmApp (TmVar 0)
                                                            (TmVar 1))
                                                     (TmVar 2)))))).

Fixpoint isval (t: term): bool :=
  match t with
  | TmAbs _ => true
  | _ => false
  end.



Fixpoint eval (t: term): option term :=
  match t with
  | TmApp (TmAbs t11) v2 =>
    if isval v2 then Some (termSubstTop v2 t11)
    else None
  | TmApp t1 t2 =>
    match isval t1 with
    | true =>
      match eval t2 with
      | Some t2' => Some (TmApp t1 t2')
      | _ => None
      end
    | false =>
      match eval t1 with
      | Some t1' => Some (TmApp t1' t2)
      | _ => None
      end
    end
  | _ => None
  end.



  end.
