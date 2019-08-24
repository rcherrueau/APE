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

(* Implication table: A ⇒ B: Prop *)
(* | A\B   | True     | False     | *)
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
the values of such type are proofs. Here, the induction principle of
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

Theorem negb_ineq: forall b, ~ (negb b = b).  (* Similarly, nebg <> b *)
  destruct b; discriminate.
  (* The discriminate tactic is used to proved that two values of an
  inductive type are not equal, whenever the values are formed with
  different constructors.

  My guess is the tactic relies on `inversion Hypo` under the hood.
  Inversion looks into the constructors of the inductive type and
  tries to find which constructors could lead to the `Hypo`. If there
  is none, then `H -> False`. *)
Restart.
  Locate "~". Print not.  (* Similarly, Locate "<>" .*)
  destruct b; simpl; unfold not.
  - intro H. inversion H.
  - intro H. inversion H.
Qed.

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* Simple Recursive Types *)

(* ------------------------------------------------------------------- *)
(* Natural numbers *)
Inductive nat: Set :=
| O: nat
| S: nat -> nat.

Check nat_ind.
(* [: ∀ P : nat -> Prop,
       P O ->
       (∀ n : nat, P n -> P (S n)) ->
       ∀ n : nat, P n]

The first argument is the predicate as usual, then the second argument
tells to prove my predicate for `O`. And look, the third argument `∀ n
: nat, P n -> P (S n)` is a function (or, implication) that should be
"suppose `P` holds for `n`, now prove that it holds for `S n`", which
is an inductive hypothesis!

From there, we can prove theorems by case analysis with `destruct` af
for simpler inductive types, but we can also now get into genuine
inductive theorems. *)

Definition isZero(n: nat): bool :=
  match n with
  | O => true
  | _ => false
  end.

Definition pred(n: nat): nat :=
  match n with
  | O => O
  | (S n') => n'
  end.

Fixpoint plus (n m: nat) : nat :=
  match n with
  | O => m
  | S n' => S (plus n' m)
  end.

(* Some theorems about `plus` can be proved without induction. *)
Theorem O_plus_n : forall (n: nat), plus O n = n.
  intro n. simpl. reflexivity.
  (* Coq computation (e.g., `simpl`) automatically simplify the
    application of `plus`, because unfolding the `plus` a `match`
    expression where the branch to be taken is obvious (i.e, the `O =>
    m`). *)
Restart.
  unfold plus. reflexivity.
Qed.

(* Reversing the order of the arguments, though, this no longer works,
because there is no induction on the second argument in the `plus`
definition. Thus, we need induction. *)
Theorem n_plus_O : forall (n: nat), plus n O = n.
  induction n.
  - reflexivity.  (* O = O *)
  - simpl. rewrite IHn. reflexivity.  (* plus (S n) O = S n *)
Restart.  (* BTW, `crush` handles it very well. *)
  induction n; crush.
Qed.

(* `S` of nat is injective *)
Theorem S_inj : forall n m, S n = S m -> n = m.
  intros n m H. injection H. trivial.
Qed.

(* ------------------------------------------------------------------- *)
(* List of Natural numbers *)

Inductive nat_list: Set :=
| NNil: nat_list
| NCons: nat -> nat_list -> nat_list.

Fixpoint nlength (ns: nat_list): nat :=
  match ns with
  | NNil => O
  | NCons n ns' => S (nlength ns')
  end.

Fixpoint napp (ns1 ns2: nat_list): nat_list :=
  match ns1 with
  | NNil => ns2
  | NCons n ns1' => NCons n (napp ns1' ns2)
  end.

Theorem nlength_napp : forall (ns1 ns2: nat_list),
    nlength(napp ns1 ns2) = plus (nlength ns1) (nlength ns2).
Proof.
  induction ns1.
  - simpl. reflexivity.
  - simpl. intro ns2. rewrite (IHns1 ns2). reflexivity.
Restart.
  induction ns1; crush.
Qed.

(* ------------------------------------------------------------------- *)
(* Binary Tree of Natural Numbers *)

Inductive nat_btree: Set :=
| NLeaf: nat_btree
| NNode: nat_btree -> nat -> nat_btree -> nat_btree.

Fixpoint nsize (tr: nat_btree): nat :=
  match tr with
  | NLeaf => S O
  | NNode l _ r => plus (nsize l) (nsize r)
  end.

Fixpoint nsplice (tr1 tr2: nat_btree): nat_btree :=
  match tr1 with
  | NLeaf => NNode tr2 O NLeaf
  | NNode l n r => NNode (nsplice l tr2) n r
  end.

Theorem plus_assoc: forall n1 n2 n3: nat,
    plus (plus n1 n2) n3 = plus n1 (plus n2 n3).
Proof.
  induction n1.
  - simpl. reflexivity.
  - intros n2 n3. simpl. rewrite IHn1. reflexivity.
Restart.
  induction n1; crush.
Qed.

Hint Rewrite n_plus_O plus_assoc.
Theorem nsize_nsplice : forall (tr1 tr2: nat_btree),
    nsize (nsplice tr1 tr2) = plus (nsize tr2) (nsize tr1).
Proof.
  induction tr1; crush.
Qed.

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* Parameterized Types *)
