(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(** * Lambda calculus                          *)
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
Inductive term: Set :=
| TmVar: nat -> term
| TmAbs: term -> term
| TmApp: term -> term -> term.

Notation "% k" := (TmVar k) (at level 20).
Infix "@@" := TmApp (at level 25, left associativity).
Notation "λ∙ t" := (TmAbs t) (at level 35).

(* Some definitions *)
Example c0: term := λ∙ λ∙ %0.
Example c2: term := λ∙ λ∙ %1 @@ (%1 @@ %0).
Example plus: term := λ∙ λ∙ λ∙ λ∙ %3 @@ %1 @@ (%2 @@ %0 @@ %1).
Example c2': term := plus @@ c2 @@ c0.

Require Import Decidable.

(** * n-Terms

- A 0-terms is a term with no free variables
- A 1-terms is a term with at most one free variable
- A n-terms is a term with at most n free variables, numbered
  between 0 and n-1

 *)

(* Fixpoint Terms (t: term) (n: nat): Prop := *)
(*   match t with *)
(*   | TmVar k => if lt_dec k n *)
(*                then True   (* k < n  *) *)
(*                else False   (* k >= n *) *)
(*   | TmAbs t => Terms t (n + 1) *)
(*   | TmApp t1 t2 => *)
(*     let rh := Terms t1 n in *)
(*     let lh := Terms t2 n in *)
(*     if dec_True rh *)
(*     then True *)
(*     else False *)
(*   end. *)

Fixpoint Terms (t: term) (n: nat): Prop :=
  match t with
  | TmVar k => if lt_dec k n
               then True   (* k < n  *)
               else False   (* k >= n *)
  | TmAbs t => Terms t (S n)
  | TmApp t1 t2 =>
    (Terms t1 n) /\ (Terms t2 n)
  end.


(* Example nTermsVar: forall (n k: nat), *)
(*     k < n -> Terms (TmVar k) n. *)
(* Proof. *)
(*   simpl. *)
(*   intros. *)
(*   set (f := lt_dec k n). *)
(*   destruct f. *)
(*   exact I. *)
(*   exact (n0 H). *)
(* Qed. *)

(* Example nTermsAbs: forall (t: term) (n: nat), *)
(*     n > 0 -> Terms t n -> Terms (TmAbs t) (pred n). *)
(* Proof. *)
(*   simpl. *)
(*   intros. *)
(*   rewrite pred_Sn in H0. *)
(*   admit. *)
(* Admitted. *)

(* Example nTermsApp: forall (t1 t2: term) (n: nat), *)
(*     Terms t1 n -> Terms t2 n -> Terms (TmApp t1 t2) n. *)
(* Proof using Type. *)
(*   intros t1 t2 n nT1 nT2. simpl. split. apply nT1. apply nT2. *)
(* Qed. *)



(* (* Definition zeroTerms : Terms (λ∙%0) 0 = True. *) *)
(* (* Definition  zeroTerms':= Terms (λ∙λ∙%0 @@ %1) 0 = True. *) *)
(* (* Definition  zeroTerms3:= Terms (λ∙λ∙%0 @@ %4) 0 = True. *) *)
Eval compute in (Terms (λ∙%0) 0).
Eval compute in (Terms (λ∙λ∙%0 @@ %1) 0).
Eval compute in (Terms (λ∙λ∙%0 @@ %4) 0).

(* Lemma zeroTerms: Terms (λ∙%0) 0 = True. *)
(* Proof using Type. simpl. reflexivity. Qed. *)

(* (* Lemma zeroTerms': Terms (λ∙λ∙%0 @@ %1) 0 = True. *) *)
(* (* Proof. simpl. compute. exfalso. contradict. *) *)

(* Lemma notZeroTerms: Terms (λ∙λ∙ %4) 0 = False. *)
(* Proof. simpl. reflexivity. Qed. *)


(** * Shifting -- 6.2.1

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

We also write ↑1(t) for ↑0,1(t) *)
Fixpoint shift_walk (c: nat) (d: nat) (t: term): term :=
  match t with
  | TmVar k => if lt_dec k c
               then TmVar k        (* k < c  *)
               else TmVar (k + d)  (* k >= c *)
  | TmAbs t => TmAbs (shift_walk (S c) d t)
  | TmApp t1 t2 =>
    let t1' := shift_walk c d t1 in
    let t2' := shift_walk c d t2 in
    TmApp t1' t2'
  end.
Definition shift (d: nat) (t: term)   := shift_walk 0 d t.

Eval compute in (shift 1 (λ∙ %0 @@ %1)).
Eval compute in (shift 2 (λ∙ λ∙ %1 @@ %0 @@ %2)).
Eval compute in (shift 2 (λ∙ %0 @@ %1 @@ (λ∙ %0 @@ %1 @@ %2))).

Eval compute in (eq_nat 2 (1 + 3)).
(** * Substitution -- 6.2.4

The substitution of a term `s` for a variable number `j` in a term
`t`, written `[j ↦ s] t`.

*)
(* Fixpoint substitute_walk (c: nat) (j: nat) (s t: term) : term := *)
(*   match t with *)
(*   | TmVar k => if eq_nat_dec k (j + c) *)
(*                then (shift c s)  (* k = j+c *) *)
(*                else TmVar k      (* k != j  *) *)
(*   | TmAbs t => TmAbs (substitute_walk (S c) j s t) *)
(*   | TmApp t1 t2 => *)
(*     let t1' := substitute_walk c j s t1 in *)
(*     let t2' := substitute_walk c j s t2 in *)
(*     TmApp t1' t2' *)
(*   end. *)
(* Definition substitute (j: nat) (s t: term) := substitute_walk 0 j s t. *)
Fixpoint substitute (j: nat) (s t: term) : term :=
  match t with
  | TmVar k => if eq_nat_dec k j
               then s            (* k = j+c *)
               else TmVar k      (* k != j  *)
  | TmAbs t => TmAbs (substitute (S j) (shift 1 s) t)
  | TmApp t1 t2 =>
    let t1' := substitute j s t1 in
    let t2' := substitute j s t2 in
    TmApp t1' t2'
  end.

(*
  Γ = z ↦ 5, y ↦ 4, x ↦ 3, w ↦ 2, b ↦ 1, a ↦ 0.
*)
(* [b ↦ a] (b (λx.λy.b)) = (a (λx.λy.a)) *)
Eval compute in (substitute 1 (%0) (%1 @@ (λ∙ λ∙ %3))).
(* [b ↦ a (λz.a)] (b (λx.b)) = (a (λz.a) (a (λz.a))) *)
Eval compute in (substitute 1 (%0 @@ (λ∙ %1)) (%1 @@ (λ∙ %2))).
(* [b ↦ a] (λb. b a) = *)
Eval compute in (substitute 1 (%0) (λ∙%0 @@ %1)).
(* [b ↦ a] (λa. b a) *)
Eval compute in (substitute 1 (%0) (λ∙%2 @@ %0)).
(* [x ↦ (λz. z w)](λy.x) = λy.λz. z w *)
Eval compute in (substitute 3 (λ∙ %6 @@ %3) (λ∙%4)).

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
