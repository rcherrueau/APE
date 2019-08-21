(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* CPDT Chapter 3: Inductive Types                                     *)
(* See, http://adam.chlipala.net/cpdt/                                 *)
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
Require Import List.
Require Import Cpdt.CpdtTactics.
Set Implicit Arguments.
Set Asymmetric Patterns.

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* Proof Terms *)

Check (fun x: nat => x).
(* [: nat -> nat] *)

Check (fun x: True => x).
(* [: True -> True] *)

(* Implication table*)
(* | =>    | True     | False     | *)
(* |-------+----------+-----------| *)
(* | False | True (1) | True (2)  | *)
(* | True  | True (3) | False (4) | *)

Check I.  (* The single proof of True *)
(* [: True] *)

(* Check the Implication table: *)
Check (fun _: False => I). (* 1 *)
Check (fun x: False => x). (* 2 *)
Check (fun _: True => I).  (* 3 *)
(* There is no proof of False for 4 *)

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* Enumerations *)

(* ------------------------------------------------------------------- *)
(* A simple inductive type *)
Inductive unit: Set := tt.
Check unit.  (* [: Set] *)
Check tt.    (* [: unit] *)

(* Prove that `unit` is a singleton type *)
Theorem unit_singleton: forall u: unit, u = tt.
  destruct u.  (* Proceed by case analysis; The only
                  inhabitant is `tt`. *)
  reflexivity.
Qed.

(* We could also start the proof with `induction x`, that will
introduce inductive hypotheses (i.e., "let's assume the proof holds
for n, and now prove that it holds for n+1"). Note there is no such a
hypotheses for the `unit` type. We can check this by looking at the
/induction principle/ of `unit`. *)

Check unit_ind.
(* [: ∀ P : unit -> Prop,
      P tt -> ∀ u : unit, P u]

See the type `Prop`? It is the type for logical proposition. And
the values of such type are proofs. Here the induction principle of
`unit` states that:
- P tt :: If you can present a proof that `P` holds for `tt`
- ∀ u : unit, P u :: Then we are rewarded with a proof that P holds
  for any value `u` of type `unit`.
With `P` a predicate over `unit` values, which was the theorem we try
to prove in the previous proof: > fun u: unit => u = tt

So if I instantiate the `unit_ind` with the previous theorem, it says:
- (fun u: unit => u = tt) tt :: If you can present a proof of `tt =
  tt`.
- ∀ u : unit, P u :: Then I reward you with a proof that `∀ u : unit,
  (fun u: unit => u = tt) u`.
Which was the theorem we tried to prove. *)

(* ------------------------------------------------------------------- *)
(* An inductive type with no inhabitants *)
Inductive Empty_set: Set :=.

(* So we can prove fun theorems about it *)
Theorem the_sky_is_falling: forall x: Empty_set, 2 + 2 = 5.
  intro x. destruct x.
Qed.

(* Why? Let's check its induction principle *)
Check Empty_set_ind.
(* [:∀ (P : Empty_set -> Prop) (e : Empty_set), P e]

Here my predicate over `Empty_set` is the fun theorem `2 + 2 = 5`. And
the induction principle doesn't state an implication like with
`unit_ind`. Here it only says `P e`. In other words, any predicate
over values from the empty set holds vacuously. *)

(* ------------------------------------------------------------------- *)
(* An inductive type with two inhabitants *)
Inductive bool: Set :=
| true
| false.

Check bool_ind.
(* [: ∀ P : bool -> Prop,
      P true -> P false -> ∀ b : bool, P b]

If I show that my predicate holds for true and false, then my predicate holds for any value of type bool.
*)

Definition negb(b: bool): bool :=
  match b with
  | true => false
  | false => true
  end.

(* Prove that `negb` is its own inverse operation *)
Theorem negb_inverse: forall b, negb (negb b) = b.
  destruct b; simpl; reflexivity.
Qed.

Theorem negb_ineq: forall b, ~ (negb b = b).
  destruct b; discriminate.
  (* The discriminate tactic is used to proved that two values of an
  inductive type are not equal, whenever the values are formed with
  different constructors.

  My guess is the tactic relies on `inversion Hypo` under the hood.
  Inversion looks into the constructors of the inductive type and
  tries to find which constructors could lead to the `Hypo`. If there
  is none, then `H -> False`. *)
Restart.
  Locate "~". Print not.
  destruct b; simpl; unfold not.
  - intro H. inversion H.
  - intro H. inversion H.
Qed.

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* Simple Recursive Types *)

(* ------------------------------------------------------------------- *)
(* Natural numbers *)
Inductive nat: Set :=
| Z: nat
| S: nat -> nat.

Definition isZero(n: nat): bool :=
  match n with
  | Z => true
  | _ => false
  end.

Definition pred(n: nat): nat :=
  match n with
  | Z => Z
  | (S n') => n'
  end.

Check nat_ind.
(* [: ∀ P : nat -> Prop,
       P Z ->
       (∀ n : nat, P n -> P (S n)) ->
       ∀ n : nat, P n]

 *)
(*
Look, here I have an inductive hypothesis.
*)
