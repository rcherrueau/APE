Inductive Nat: Type :=
| Zero: Nat
| Succ: Nat -> Nat.

(* Check Nat_rect. *)

Definition zero: Nat := Zero.
Definition one:  Nat := Succ Zero.
Definition two:  Nat := Succ (Succ Zero).

(* Fixpoint plus (n: Nat) (m: Nat): Nat := *)
(*  match n with *)
(*  | Zero => m *)
(*  | Succ p => Succ (p + m) *)
(*  end *)

(* where "n + m" := (plus n m) : Nat. *)
Definition plus: Nat -> Nat -> Nat :=
  fix plus n m :=
  match n with
  | Zero => m
  | Succ p => Succ (plus p m)
  end.

Infix "+" := plus.

Eval compute in (one + two).

Lemma two_is_one_plus_one: two = (one + one).
  apply eq_refl.
Qed.

(* --------------------------------------------------------------- *)
Inductive List (Q: Type): Type :=
| Nil: List Q
| Cons: Q -> List Q -> List Q.

(* `Arguments` makes parameters as implicit. I also see the use of
`Variable Q: Type`. To make Q implicit in the whole file, but I don't
get how it works exactly. *)
Arguments Nil  {Q}.
Arguments Cons {Q} x xs.

Definition append (Q: Type): List Q -> List Q -> List Q :=
  fix append xs ys :=
  match xs with
  | Nil => ys
  | Cons x xs => Cons x (append xs ys)
  end.

(* I have to give the type of `Q`, i.e., `Nat` *)
Eval compute in (append Nat Nil Nil).

(*
I can make it implicit with Arguments, or directly into
the definition.
*)

Definition append' {Q: Type}: List Q -> List Q -> List Q :=
  fix append' xs ys :=
  match xs with
  | Nil => ys
  | Cons x xs => Cons x (append' xs ys)
  end.

Eval compute in (append' Nil Nil).

Infix "++" := append'.


(* --------------------------------------------------------------- *)
(* Can I do a List datatype where Q is implicit *)
Inductive List': Type -> Type :=
| Nil'  {Q: Type}: List' Q
| Cons' {Q: Type}: Q -> List' Q -> List' Q.

(* --------------------------------------------------------------- *)
(*
Numbers strictly less than some bound.

The name comes from "finite state". `Fin two` has exactly two
elements.
- `n` the upper bound
*)
Inductive Fin: Nat -> Type :=
| FZero: forall {k: Nat}, Fin (Succ k)
| FSucc: forall {k: Nat}, Fin k -> Fin (Succ k).

Check (FZero: Fin two).
Check (FSucc FZero: Fin two).

(* Illtyped: Fin two has exactly 2 elements, not 3 *)
(* Check (FSucc (FSucc FZero): Fin two). *)

(* --------------------------------------------------------------- *)
(* Vect: the sized list *)
Inductive Vect (Q: Type): Nat -> Type :=
| VNil: Vect Q Zero
| VCons: forall {n: Nat}, Q -> Vect Q n -> Vect Q (Succ n).

Arguments VNil {Q}.
Arguments VCons {Q} {n} x xs.

Definition vtwo: Vect Nat two := VCons one (VCons one VNil).
(* ill-typed: Vect size supposes 1 element *)
(* Definition vnil: Vect Nat one := vnil. *)

Definition length {Q: Type} {n: Nat}: Vect Q n -> Nat :=
  fix length _ := n.

Eval compute in length vtwo.

(*  the `length` function on vectors in fact calculates the length *)
Lemma lengthCorrect {Q: Type}: forall (len: Nat) (xs: Vect Q len), length xs = len.
  intros len xs.
  destruct xs.
  (* length VNil = Zero *)
  - apply eq_refl.
  (* length (VCons q xs) = Succ n *)
  - apply eq_refl.
Qed.

Definition tail {Q: Type} {n: Nat}: Vect Q (Succ n) -> Vect Q n :=
  fix tail vs :=
  match vs with
  | VCons _ vs => vs
  end.

Eval compute in tail vtwo.

Definition head {Q: Type} {n: Nat}: Vect Q (Succ n) -> Q :=
  fix head vs :=
  match vs with
  | VCons v _ => v
  end.

Eval compute in head vtwo.

Lemma vectMustBeNil {Q: Type}: forall (xs: Vect Q Zero), xs = VNil.
  intros.
  (* destruct xs. *)
  refine (match xs with
          | VNil => _
          | VCons a n => False
          end).
  apply eq_refl.
Qed.


(*

In the previous lemma, we could not use the `destruct xs`. Coq
complains because, `destruct` generates a subgoal for every
constructor of n `Vect`. However, `VCons` doesn't make sens with type
`Vect Q *Zero*`.

Whereas the Idris prover takes advantage of the dependent type system
to enforce the fact that `VCons` is not a possibility. This is not the
case in Coq.

In a general manner, when trying to induct on such a hypothesis (i.e.,
`xs`), one solution is to replace each concrete index (i.e., `Zero`)
with a new variable (e.g., `n0`) together with a constraint that
forces the variable to be equal to the correct concrete value (i.e.,
`n0 = Zero`). `destruct` does something similar: when given a term of
some inductive type with concrete index values, it first replaces the
concrete values with new variables. But, *it doesn't add the equality
constraints*. Thus, Coq forgets about the `Zero` and tries to pattern
match on VCons.

One solution is to do a manual match on `xs` using `refine` and do not
pattern match on `VCons` (or mark it as `False`). Note that, the
underscore leaves a hole that will be filled in later.

Another solution consists in requiering `dependent destuction`. But it
add another axiom, the John Major equality.

see, https://homes.cs.washington.edu/~jrw12/dep-destruct.html

*)

Require Import Program.

Lemma vectMustBeNil' {Q: Type}: forall (xs: Vect Q Zero), xs = VNil.
  intros.
  dependent destruction xs.
  trivial.
Qed.

Lemma vectConsCongruence {Q: Type}:
  forall (n: Nat) (x: Q) (xs: Vect Q n) (ys: Vect Q n),
    (xs = ys) -> (VCons x xs = VCons x ys).
  intros n x xs ys Hypo.
  rewrite Hypo.
  trivial.
Qed.

(*
 https://coq.inria.fr/distrib/current/refman/proof-engine/tactics.html?highlight=congruence#coq:tacn.congruence
*)
Lemma vectConsCongruence' {Q: Type}:
  forall (n: Nat) (x: Q) (xs: Vect Q n) (ys: Vect Q n),
    (xs = ys) -> (VCons x xs = VCons x ys).
  intros n x xs ys Hypo.
  congruence.
Qed.

Lemma vectInjective1 {Q: Type}:
  forall (n: Nat) (x y: Q) (xs ys: Vect Q n),
    (VCons x xs = VCons y ys) -> x = y.
  intros.
  dependent destruction H.
  trivial.
Qed.

Lemma vectInjective1' {Q: Type}:
  forall (n: Nat) (x y: Q) (xs ys: Vect Q n),
    (VCons x xs = VCons y ys) -> x = y.
  intros.
  injection H.
  trivial.
Qed.


Lemma vectInjective2 {Q: Type}:
  forall (n: Nat) (x y: Q) (xs ys: Vect Q n),
    (VCons x xs = VCons y ys) -> xs = ys.
  intros.
  dependent destruction H.
  trivial.
Qed.

Lemma vectInjective {Q: Type}:
  forall (n: Nat) (x y: Q) (xs ys: Vect Q n),
    (VCons x xs = VCons y ys) -> x = y /\ xs = ys.
  intros.
  split.
  (* x = y *)
  - apply (vectInjective1 n x y xs ys).
    trivial.
  (* xs = ys *)
  - apply (vectInjective2 n x y xs ys).
    trivial.
Qed.

(* --------------------------------------------------------------- *)
(* Relational Algebra *)

Require Import String.
Require Import List.

Inductive Ty: Type := NAT | BOOL | TEXT | DATE.

Definition interpTy : Ty -> Set :=
  fix interpTy ty :=
  match ty with
    | NAT => nat
    | BOOL => bool
    | TEXT => string
    | DATE => string
  end.

Definition Attribute: Type := (string * Ty).
Definition Schema: Type := list Attribute.

(*

The NJoin of Relational Algebra requires the `nub` operation on
Schema. `nodup` is equivalent to Idris `nub`. But applying it on a
list of nat always fails with the following error:


  Eval compute in (nodup [1 ; 2 ; 3; 2]).

  The term "[1; 2; 3; 2]" has type "list nat"
  while it is expected to have type
    "forall x y : ?A, {x = y} + {x <> y}".


This is because `nodup` misses an argument to decide on the equality
of the elements of the list.

If you look at the `nodup` definition,


  Hypothesis decA: forall x y : A, {x = y} + {x <> y}.

  Fixpoint nodup (l : list A) : list A :=
    match l with
      | [] => []
      | x::xs => if in_dec decA x xs then nodup xs else x::(nodup xs)
    end.


The hypothesis `decA` is an additional argument to the `nodup`
function. This argument tells Coq how to decide on the equality of two
elements of the input list. For instance, one may use the
`eq_nat_dec` (i.e., decidable equality for nats).

This is normal to put `decA` in the `nodup` definition because Coq
doesn't have a Typeclass mecanisme, such as `Eq A =>` to make the
equality function implicit.

    Require Import Arith.Arith.

    Check eq_nat_dec.
    Check Nat.eq_dec.
    Eval compute in (nodup Nat.eq_dec [1 ; 2 ; 3; 2]).

Now, let's, for now, admit the same thing for attributes.

*)

Definition eq_attribute_dec  (a1 a2: Attribute): {a1 = a2} + {a1 <> a2}.
Admitted.

(*
Also, the Project requires the inclusion of a Schema into another.
*)

(* Definition test_incl1: incl [1] [1]. *)
(*   compute. intros. trivial. *)
(* Defined. *)

(* Require ListDec. *)

(* Definition foo := (incl_decidable [1] [1;2]). *)

(* Definition test_incl2: incl [1] [1;2]. *)
(*   exact (incl_decidable [1] [1;2]). *)
(* Defined. *)

(* And produce a EDSL for Relational Algebra *)

Inductive Pred: Schema -> Ty -> Type :=
| AND:  forall {Δ: Schema}, Pred Δ BOOL -> Pred Δ BOOL -> Pred Δ BOOL
| OR:   forall {Δ: Schema}, Pred Δ BOOL -> Pred Δ BOOL -> Pred Δ BOOL
| Like (a: Attribute): forall {Δ: Schema}, TEXT = (snd a) -> In a Δ -> string -> Pred Δ BOOL
| Equal (a: Attribute) (v: interpTy (snd a)): forall {Δ: Schema}, In a Δ -> Pred Δ BOOL
.

Definition count_col: Attribute := ("COUNT"%string, NAT).

Inductive Query: Schema -> Type :=
| Project (δ: Schema): forall {Δ: Schema}, Query Δ -> incl δ Δ -> Query δ
| Product: forall {Δ Δ': Schema}, Query Δ -> Query Δ' -> Query (Δ ++ Δ')
(* | NJoin: forall {Δ Δ': Schema}, Query Δ -> Query Δ' -> Query (nodup eq_attribute_dec (Δ ++ Δ')) *)
| Count (δ: Schema): forall {Δ: Schema}, Query Δ -> incl δ Δ -> Query (δ ++ [count_col])
| Select: forall {Δ: Schema}, Pred Δ BOOL -> Query Δ -> Query Δ
.

Infix "&&" := AND.
Infix "||" := OR.
Infix "==" := Equal (at level 70, right associativity).
Notation π := Project (only parsing).
Notation σ := Select (only parsing).

(* --------------------------------------------------------------- *)
(* Relational Algebra Test *)

(* Query with schema knows at compile time *)
Definition D: Attribute := ("Date"%string, DATE).
Definition N: Attribute := ("Name"%string, TEXT).
Definition A: Attribute := ("Address"%string, TEXT).

Definition Meeting: Schema := [D ; N ; A].

(* Definition qaddr: Query Meeting -> Query [N ; A] := *)
(*   fix qaddr q := *)
(*     let inclusion_proof := _ (* ltac:(auto) *) *)
(*     in π [N ; A] q inclusion_proof. *)

Definition qaddr: Query Meeting -> Query [N ; A].
  intros q.
  assert (inclusion_proof : incl [N ; A] Meeting).
  (* incl [N ; A] Meeting *)
  -  compute.
     intros.
     admit
  (* - admit. *)
  (* The exact query with the proof *)
  - exact (π [N; A] q inclusion_proof).
Defined.
