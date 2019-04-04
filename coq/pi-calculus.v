(*
List of tactics:
- https://coq.inria.fr/distrib/current/refman/proof-engine/tactics.html
- http://www.cs.cornell.edu/courses/cs3110/2018fa/lec/20-coq-fp/cheatsheet.html#destructandor
*)

Require Import Coq.Setoids.Setoid.

(* Datatype for Process:

Name (out/in/ν *n*.p), parallelism (p | q) and replication (!p) are
intentionally left apart.
*)
Inductive Pi: Type :=
| Out: Pi -> Pi        (* out.P *)
| In: Pi -> Pi         (* in.P  *)
| Nu: Pi -> Pi         (* ν(P)  *)
| Zero: Pi.            (* 0     *)

Check Pi.
Check Pi_ind.
Check Pi_rect.
Check Pi_rec.

(* ------------------------------------------------------------------- *)
(* Structural Congruence (≡)                                           *)
(* ------------------------------------------------------------------- *)

(* ------------------------------------------------------------------- *)
(* Definition:

Struct. Congruence (≡) is the *smallest congruence relation* over Pi.

- *Congruence relation* means the structural congruence is an
  equivalence relation and thus implies reflexivity, transitivity and
  symmetry.

- *Smallest* means it is defined under its operations (constructors).

     p ≡ q
  -----------
  C[p] ≡ C[q]
*)

Inductive SCong: Pi -> Pi -> Prop :=
(*
  Smallest congruence relation.

     p ≡ q
  -----------
  C[p] ≡ C[q]
 *)
(* Congruence *)
| scong_out: forall p q : Pi,              (* p ≡ q ⇒ Out p ≡ Out q *)
   SCong p q -> SCong (Out p) (Out q)
| scong_in: forall p q : Pi,               (* p ≡ q ⇒ In p ≡ In q *)
   SCong p q -> SCong (In p) (In q)
| scong_nu: forall p q : Pi,               (* p ≡ q ⇒ νp ≡ νq *)
   SCong p q -> SCong (Nu p) (Nu q)
(* Reflexivity *)
| scong_refl_zero: SCong Zero Zero         (* 0 ≡ 0 *)
(* Transitivity counterpart *)
| scong_out_trans: forall p q r : Pi,      (* Out p ≡ r ∧ r ≡ Out q ⇒ Out p ≡ Out q *)
    SCong (Out p) r -> SCong r (Out q)
    -> SCong (Out p) (Out q)
| scong_in_trans: forall p q r : Pi,       (* In p ≡ r ∧ r ≡ In q ⇒ In p ≡ In q *)
    SCong (In p) r -> SCong r (In q)
    -> SCong (In p) (In q)
| scong_nu_trans: forall p q r : Pi,       (* ν p ≡ r ∧ r ≡ ν q ⇒ ν p ≡ ν q *)
    SCong (Nu p) r -> SCong r (Nu q)
    -> SCong (Nu p) (Nu q)
(* Symmetry counterpart *)
| scong_out_sym: forall p q : Pi,          (* p ≡ q ⇒ Out q ≡ Out p *)
   SCong p q -> SCong (Out q) (Out p)
| scong_out_trans_sym: forall p q r : Pi,  (* Out p ≡ r ∧ r ≡ Out q ⇒ Out q ≡ Out p *)
    SCong (Out p) r -> SCong r (Out q)
    -> SCong (Out q) (Out p)
| scong_in_sym: forall p q : Pi,           (* p ≡ q ⇒ In q ≡ In p *)
   SCong p q -> SCong (In q) (In p)
| scong_in_trans_sym: forall p q r : Pi,   (* In p ≡ r ∧ r ≡ In q ⇒ In q ≡ In p *)
    SCong (In p) r -> SCong r (In q)
    -> SCong (In q) (In p)
| scong_nu_sym: forall p q : Pi,           (* p ≡ q ⇒ νq ≡ νp *)
   SCong p q -> SCong (Nu q) (Nu p)
| scong_nu_trans_sym: forall p q r : Pi,   (* ν p ≡ r ∧ r ≡ ν q ⇒ ν q ≡ ν p *)
    SCong (Nu p) r -> SCong r (Nu q)
    -> SCong (Nu q) (Nu p)
(* Axioms *)
| scong_res_zero: forall p : Pi,           (* ν0 ≡ 0 *)
    SCong (Nu Zero) p -> SCong Zero p
| scong_res_zero_sym: forall p : Pi,
    SCong (Nu Zero) p -> SCong p Zero.

Infix "≡" := SCong (at level 70, no associativity).

Lemma z_is_nuz:
  Zero ≡ Nu Zero.
Proof.
  assert (Zero ≡ Zero) by exact scong_refl_zero.
  assert (Nu Zero ≡ Nu Zero) by exact (scong_nu Zero Zero H).
  apply (scong_res_zero (Nu Zero) H0).
Qed.

(* ------------------------------------------------------------------- *)
(* Trivial inversion on `H -> False`.

Inversion looks into the constructor of SCong and tries to find which
constructors could lead to H. If there is none, then `H -> False`.

Here is an example for the equivalence relation on Bool:


> Lemma test: true = false -> False.
> Proof.
>   intro Heq. inversion Heq.
> Qed.


Which is equivalent to the more common pattern.

> Lemma test: true = false -> False.
> Proof.
>   discriminate.
> Qed.

*)

Lemma out_isnt_zero: forall p : Pi,
    Out p ≡ Zero -> False.
Proof.
  intros p Hscong.
  (* Inversion looks into the constructor of SCong tries to find which
  constructors could lead to Hscong. In this case there is none, thus
  `Zero ≡ Out p -> False`.*)
  inversion Hscong.
  inversion H.
Qed.

Lemma out_isnt_nu: forall p q : Pi,
    Out p ≡ Nu q -> False.
Proof.
  intros p q Hscong.
  inversion Hscong.
Qed.

Lemma nu_isnt_out: forall p q : Pi,
    Out p ≡ Nu q -> False.
Proof.
  intros p q Hscong.
  inversion Hscong.
Qed.

Lemma zero_isnt_out: forall p : Pi,
    Zero ≡ Out p -> False.
Proof.
  intros p Hscong.
  inversion Hscong.
  inversion H.
Qed.

(* ------------------------------------------------------------------- *)
(* SCong is an equivalence relation *)

(* Reflexivity *)
Lemma scong_refl: reflexive Pi SCong.
  intro p.
  induction p.
  (* out p ≡ out p *)
  - exact (scong_out p p IHp).
  (* in p ≡ in p *)
  - exact (scong_in p p IHp).
  (* νp ≡ νp *)
  - exact (scong_nu p p IHp).
  (* 0 ≡ 0*)
  - exact scong_refl_zero.
Qed.

Lemma scong_sym_out: forall p q : Pi,
    Out p ≡ Out q -> Out q ≡ Out p.
Proof.
  intros p q Hscong.
  inversion Hscong.
  - exact (scong_out_sym p q H1).
  - exact (scong_out_trans_sym p q r H1 H2).
  - exact (scong_out q p H1).
  - exact (scong_out_trans q p r H1 H2).
Qed.

(* Symmetry *)
Lemma scong_sym_in: forall p q : Pi,
    In p ≡ In q -> In q ≡ In p.
Proof.
  intros p q Hscong.
  inversion Hscong.
  - exact (scong_in_sym p q H1).
  - exact (scong_in_trans_sym p q r H1 H2).
  - exact (scong_in  q p H1).
  - exact (scong_in_trans q p r H1 H2).
Qed.

Lemma scong_sym_nu: forall p q : Pi,
    Nu p ≡ Nu q -> Nu q ≡ Nu p.
Proof.
  intros p q Hscong.
  inversion Hscong.
  - exact (scong_nu_sym p q H1).
  - exact (scong_nu_trans_sym p q r H1 H2).
  - exact (scong_nu  q p H1).
  - exact (scong_nu_trans q p r H1 H2).
Qed.

Theorem scong_sym: symmetric Pi SCong.
  intros p q.
  destruct p; destruct q.
  - intro Hscong; apply (scong_sym_out p q Hscong).
  - intro Hscong; inversion Hscong.
  - intro Hscong. inversion Hscong.
  - intro Hscong; inversion Hscong. inversion H.
  - intro Hscong; inversion Hscong.
  - intro Hscong; apply (scong_sym_in p q Hscong).
  - intro Hscong; inversion Hscong.
  - intro Hscong; inversion Hscong. inversion H.
  - intro Hscong; inversion Hscong.
  - intro Hscong; inversion Hscong.
  - intro Hscong; apply (scong_sym_nu p q Hscong).
  - intro Hscong. inversion Hscong.
    apply (scong_res_zero (Nu p) H).
  - intro Hscong. inversion Hscong. inversion H.
  - intro Hscong. inversion Hscong. inversion H.
  - intro Hscong. inversion Hscong.
    apply (scong_res_zero_sym (Nu q) H).
  - intro Hscong. assumption.
Qed.

(* Transitivity *)
Lemma scong_trans_out: forall p q r : Pi,
    Out p ≡ r -> r ≡ Out q -> Out p ≡ Out q.
Proof.
  intros p q r HscongPR HscongRQ.
  exact (scong_out_trans p q r HscongPR HscongRQ).
Qed.

Lemma scong_trans_in: forall p q r : Pi,
    In p ≡ r -> r ≡ In q -> In p ≡ In q.
Proof.
  intros p q r HscongPR HscongRQ.
  exact (scong_in_trans p q r HscongPR HscongRQ).
Qed.

Lemma scong_trans_nu: forall p q r : Pi,
    Nu p ≡ r -> r ≡ Nu q -> Nu p ≡ Nu q.
Proof.
  intros p q r HscongPR HscongRQ.
  exact (scong_nu_trans p q r HscongPR HscongRQ).
Qed.

Theorem scong_trans: transitive Pi SCong.
  intros p r q.
  destruct p; destruct q.
  - intros HscongPR HscongRQ.
    exact (scong_trans_out p q r HscongPR HscongRQ).
  - intros HscongPR HscongRQ. destruct r.
    + inversion HscongRQ.
    + inversion HscongPR.
    + inversion HscongPR.
    + inversion HscongPR. inversion H.
  - intros HscongPR HscongRQ. destruct r.
    + inversion HscongRQ.
    + inversion HscongPR.
    + inversion HscongPR.
    + inversion HscongPR. inversion H.
  - intros HscongPR HscongRQ. destruct r.
    + inversion HscongRQ. inversion H.
    + inversion HscongPR.
    + inversion HscongPR.
    + inversion HscongPR. inversion H.
  - intros HscongPR HscongRQ. destruct r.
    + inversion HscongPR.
    + inversion HscongRQ.
    + inversion HscongPR.
    + inversion HscongPR. inversion H.
  - intros HscongPR HscongRQ.
    exact (scong_trans_in p q r HscongPR HscongRQ).
  - intros HscongPR HscongRQ. destruct r.
    + inversion HscongPR.
    + inversion HscongRQ.
    + inversion HscongPR.
    + inversion HscongPR. inversion H.
  - intros HscongPR HscongRQ. destruct r.
    + inversion HscongPR.
    + inversion HscongRQ. inversion H.
    + inversion HscongPR.
    + inversion HscongPR. inversion H.
  - intros HscongPR HscongRQ. destruct r.
    + inversion HscongPR.
    + inversion HscongRQ.
    + inversion HscongRQ.
    + inversion HscongRQ. inversion H.
  - intros HscongPR HscongRQ. destruct r.
    + inversion HscongPR.
    + inversion HscongPR.
    + inversion HscongRQ.
    + inversion HscongRQ. inversion H.
  - intros HscongPR HscongRQ.
    exact (scong_trans_nu p q r HscongPR HscongRQ).
  - intros HscongPR HscongRQ. destruct r.
    + inversion HscongRQ. inversion H.
    + inversion HscongRQ. inversion H.
    + assert (Zero ≡ Nu Zero) by exact z_is_nuz.
      assert (Nu r ≡ Nu Zero) by exact (scong_trans_nu r Zero Zero HscongRQ H).
      assert (Nu p ≡ Nu Zero) by exact (scong_trans_nu p Zero (Nu r) HscongPR H0).
      assert (Nu Zero ≡ Nu p) by exact (scong_sym (Nu p) (Nu Zero) H1).
      apply (scong_res_zero_sym (Nu p) H2).
    + assert (Zero ≡ Nu Zero) by exact z_is_nuz.
      assert (Nu p ≡ Nu Zero) by exact (scong_trans_nu p Zero Zero HscongPR H).
      assert (Nu Zero ≡ Nu p) by exact (scong_sym (Nu p) (Nu Zero) H0).
      apply (scong_res_zero_sym (Nu p) H1).
  - intros HscongPR HscongRQ. destruct r.
    + inversion HscongPR. inversion H.
    + inversion HscongPR. inversion H.
    + inversion HscongRQ.
    + inversion HscongRQ. inversion H.
  - intros HscongPR HscongRQ. destruct r.
    + inversion HscongPR. inversion H.
    + inversion HscongPR. inversion H.
    + inversion HscongRQ.
    + inversion HscongRQ. inversion H.
  - intros HscongPR HscongRQ. destruct r.
    + inversion HscongRQ.
    + inversion HscongRQ.
    + assert (Nu Zero ≡ Zero) by exact (scong_sym Zero (Nu Zero) z_is_nuz).
      assert (Nu Zero ≡ Nu r) by exact (scong_trans_nu Zero r Zero H HscongPR).
      assert (Nu Zero ≡ Nu q) by exact (scong_trans_nu Zero q (Nu r) H0 HscongRQ).
      apply (scong_res_zero (Nu q) H1).
    + assert (Nu Zero ≡ Zero) by exact (scong_sym Zero (Nu Zero) z_is_nuz).
      assert (Nu Zero ≡ Nu q) by exact (scong_trans_nu Zero q Zero H HscongRQ).
      apply (scong_res_zero (Nu q) H0).
  - intros. exact (scong_refl Zero).
Qed.

(* ------------------------------------------------------------------- *)
(* Setoid:

Make SCong a setoid to get access to reflexivity, symmetry, rewrite
tactics.

See, https://coq.inria.fr/distrib/current/refman/addendum/generalized-rewriting.html

*)
Add Parametric Relation: Pi SCong
    reflexivity proved by scong_refl
    symmetry proved by scong_sym
    transitivity proved by scong_trans
      as scong_rel.

About scong_rel_Reflexive.

Add Parametric Morphism: (Out) with
      signature (SCong) ==> (SCong) as scong_out_mor.
Proof.
  intros p q Hscong.
  exact (scong_out p q Hscong).
Qed.

Add Parametric Morphism: (In) with
      signature (SCong) ==> (SCong) as scong_in_mor.
Proof.
  intros p q Hscong.
  exact (scong_in p q Hscong).
Qed.

Add Parametric Morphism: (Nu) with
      signature (SCong) ==> (SCong) as scong_nu_mor.
Proof.
  intros p q Hscong.
  exact (scong_nu p q Hscong).
Qed.


(* ------------------------------------------------------------------- *)
(* p ≡ q is decidable

In Coq: {p ≡ q} + {~ p ≡ q}

*)

Locate "+". Print sumbool.
Locate "~". Print not.

Axiom znup_is_zp: forall p: Pi,
    Zero ≡ Nu p -> Zero ≡ p.

Lemma nuznup_is_zp: forall p : Pi,
    Nu Zero ≡ Nu p -> Zero ≡ p.
Proof.
  intros p H.
  rewrite <- z_is_nuz in H.
  (* Were I struggle! I don't want to use the previous axiom. But,
  cannot figure out how to remove Nu. *)
  (* apply (znup_is_zp p H). *)
Qed.


(* ------------------------------------------------------------------- *)
(* Some tests                                                          *)

(* ~ ≡ is symmetric *)
Lemma nscong_sym: forall p q : Pi,
    ~ p ≡ q -> ~ q ≡ p.
Proof.
  intros p q _Hscong.
  unfold not.
  intro prfQP.
  refine (_Hscong _).
  symmetry.
  assumption.
Qed.

Lemma eq2scong: forall p q : Pi,
    p = q -> p ≡ q.
Proof.
  intros p q Heq.
  rewrite Heq.
  reflexivity.
Qed.


(* ------------------------------------------------------------------- *)
(* 0 ≡ q                                                               *)

(* 0 ≡ out.p *)
Lemma scong_zero_out_dec: forall p : Pi,
    {Zero ≡ Out p} + {~ Zero ≡ Out p}.
Proof.
  right; unfold not.
  intro Hscong; inversion Hscong; inversion H.
Qed.

(* 0 ≡ in.p *)
Lemma scong_zero_in_dec: forall p : Pi,
    {Zero ≡ In p} + {~ Zero ≡ In p}.
Proof.
  right; unfold not.
  intro Hscong; inversion Hscong; inversion H.
Qed.

(* 0 ≡ ν p *)
Lemma scong_zero_nu_eq_dec: forall p : Pi,
    Zero ≡ p -> Zero ≡ Nu p.
Proof.
  intros p Hscong.
  rewrite <- Hscong.
  exact (z_is_nuz).
Qed.

Lemma scong_zero_nu_neq_dec: forall p : Pi,
    ~ Zero ≡ p -> ~ Zero ≡ Nu p.
Proof.
  intros p _Hscong.
  intro Hscongznup.
  assert (Zero ≡ p) by exact (znup_is_zp p Hscongznup).
  exact (_Hscong H).
Qed.

Lemma scong_zero_nu_hdec: forall p : Pi,
    {Zero ≡ p} + {~ Zero ≡ p} -> {Zero ≡ Nu p} + {~ Zero ≡ Nu p}.
Proof.
  intros p H.
  case H.
  - left. apply (scong_zero_nu_eq_dec p s).
  - right. apply (scong_zero_nu_neq_dec p n).
 Qed.


(* 0 ≡ 0 *)
Lemma scong_zero_zero_dec:
    {Zero ≡ Zero} + {~ Zero ≡ Zero}.
Proof.
  left. reflexivity.
Qed.

Theorem scong_zero_dec: forall p : Pi,
    {Zero ≡ p} + {~ Zero ≡ p}.
Proof.
  intro p; destruct p.
  - exact (scong_zero_out_dec p).
  - exact (scong_zero_in_dec p).
  - exact (scong_zero_nu_dec p).
  - exact scong_zero_zero_dec.
Qed.

(* (* 0 ≡ p | q *) *)
(* Lemma scong_zero_par_dec: forall p q : Pi, *)
(*     {Zero ≡ Par p q} + {~ Zero ≡ Par p q}. *)
(* Proof. *)
(*   intros p q. *)
(*   compare p Zero. *)
(*   * (* 0 ≡ 0 | q *) *)
(*     compare q Zero. *)
(*     + (* 0 ≡ 0 | 0 *) *)
(*       intros qZero pZero. *)
(*       left. *)
(*       rewrite pZero. rewrite qZero. *)
(*       symmetry. rewrite scong_par_zero. *)
(*       reflexivity. *)
(*     + (* 0 ≡ 0 | _0 *) *)
(*       intros _qZero pZero. *)
(*       right. *)
(*       rewrite pZero. rewrite scong_par_commut. rewrite scong_par_zero. *)
(*       (* FIXME: use symmetry unstead. *) *)
(*       symmetry. *)
(*       apply nscong_sym. *)
(*       unfold not. *)
(*       contradiction. *)
(*   * (* 0 ≡ _0 | q *) *)
(*     compare q Zero. *)
(*     + (* 0 ≡ _0 | 0 *) *)
(*       intros qZero _pZero. *)
(*       right. *)
(*       rewrite qZero. rewrite scong_par_zero. *)
(*       (* FIXME: use symmetry unstead. *) *)
(*       apply nscong_sym. *)
(*       unfold not. *)
(*       contradiction. *)
(*     + (* 0 ≡ _0 | _0 *) *)
(*       intros _pZero _qZero. *)
(*       right. *)
(*       discriminate. *)
(* Qed. *)

(* ------------------------------------------------------------------- *)
(* νp ≡ q                                                              *)

(* νp ≡ out(x, m).q *)
Lemma scong_nu_out_dec: forall p q : Pi,
    {Nu p ≡ Out q} + {~ Nu p ≡ Out q}.
Proof.
  right; unfold not.
  intro Hscong; inversion Hscong.
Qed.

(* νp ≡ in(x, m).q *)
Lemma scong_nu_in_dec: forall p q : Pi,
    {Nu p ≡ In q} + {~ Nu p ≡ In q}.
Proof.
  right; unfold not.
  intro Hscong; inversion Hscong.
Qed.

(* νp ≡ νq *)
Lemma scong_nu_nu_eq_dec: forall p q : Pi,
    p ≡ q -> {Nu p ≡ Nu q} + {~ Nu p ≡ Nu q}.
Proof.
  intros p q Hscong.
  left. exact (scong_nu p q Hscong).
Qed.

Lemma scong_nu_nu_neq_dec: forall p q : Pi,
    ~ p ≡ q -> {Nu p ≡ Nu q} + {~ Nu p ≡ Nu q}.
Proof.
  intros p q _Hscong.
  right; unfold not; intro Hsnu.
  inversion Hsnu.
  - exact (_Hscong H1).
  - symmetry in H1. exact (_Hscong H1).
  -
Qed.

Lemma scong_nu_nu_dec': forall p q : Pi,
    {p ≡ q} + {~ p ≡ q} -> {Nu p ≡ Nu q} + {~ Nu p ≡ Nu q}.
Proof.
  intros p q HDscong.
  case HDscong.
  - intro Hscong; exact (scong_nu_nu_eq_dec p q Hscong).
  - intro _Hscong; exact (scong_nu_nu_neq_dec p q _Hscong).
Qed.

Lemma scong_nu_dec_util: forall p q : Pi,
    {Nu p ≡ Nu q} + {~ Nu p ≡ Nu q} -> {p ≡ q} + {~ p ≡ q}.
Proof.
  intros p q Hdec.
  case Hdec.
  - intro Hscong.
    left. inversion Hscong.
    + assumption.
    + symmetry in H1. assumption.
  - intro _Hscong.
    right. unfold not. intro Hspq. set (Hsnu := (scong_nu p q Hspq)).
    exact (_Hscong Hsnu).
Qed.

Lemma scong_nup_nuq_impl_scong_p_q: forall p q : Pi,
    Nu p ≡ Nu q  -> p ≡ q.
Proof.
  intros p q Hscong.
  inversion Hscong.
  - assumption.
  - symmetry. assumption.
Qed.

Lemma scong_nu_nu_dec: forall p q : Pi,
    {Nu p ≡ Nu q} + {~ Nu p ≡ Nu q}.
Proof.
  intros p q.
  admit.
Admitted.

(* νp ≡ q | r *)
(* Lemma scong_nu_par_dec: forall p q r : Pi, *)
(*     {Nu p ≡ Par q r} + {~ Nu p ≡ Par q r}. *)
(* Proof. *)
(*   right; unfold not. *)
(*   intro Hscong; inversion Hscong. *)
(* Qed. *)

(* νp ≡ 0 *)
Lemma scong_nu_zero_dec: forall p : Pi,
    {Nu p ≡ Zero} + {~ Nu p ≡ Zero}.
Proof.
  intro p.
  case (scong_zero_dec (Nu p)).
  - left. symmetry. assumption.
  (* FIXME: use symmetry unstead of nscong_sym. *)
  - right. apply nscong_sym. assumption.
Qed.

Theorem scong_nu_dec: forall p q : Pi,
    {Nu p ≡ q} + {~ Nu p ≡ q}.
Proof.
  intros p q.
  destruct q.
  - exact (scong_nu_out_dec p q).
  - exact (scong_nu_in_dec p q).
  - induction q.



(* ------------------------------------------------------------------- *)
(* out.p ≡ q                                                           *)

(* out.p ≡ out.q *)
Lemma scong_out_out_eq_dec: forall p q : Pi,
    p ≡ q -> {Out p ≡ Out q} + {~ Out p ≡ Out q}.
Proof.
  intros p q Hscong.
  left. exact (scong_out p q Hscong).
Qed.

Lemma scong_out_out_neq_dec: forall p q : Pi,
    ~ p ≡ q -> {Out p ≡ Out q} + {~ Out p ≡ Out q}.
Proof.
  intros p q _Hscong.
  right; unfold not; intro Hsout.
  inversion Hsout.
  - exact (_Hscong H1).
  - symmetry in H1; exact (_Hscong H1).
Qed.

Lemma scong_out_out_dec: forall p q : Pi,
    {p ≡ q} + {~ p ≡ q} -> {Out p ≡ Out q} + {~ Out p ≡ Out q}.
Proof.
  intros p q HDscong.
  case HDscong.
  - intro Hscong; exact (scong_out_out_eq_dec p q Hscong).
  - intro _Hscong; exact (scong_out_out_neq_dec p q _Hscong).
Qed.

(* out.p ≡ in.q *)
Lemma scong_out_in_dec: forall p q : Pi,
    {Out p ≡ In q} + {~ Out p ≡ In q}.
Proof.
  right; unfold not; intro Hscong; inversion Hscong.
Qed.

(* out.p ≡ q | r *)
(* Lemma scong_out_par_dec: forall p q r : Pi, *)
(*     {Out p ≡ Par q r} + {~ Out p ≡ Par q r}. *)
(* Proof. *)
(*   right. discriminate. *)
(* Qed. *)

(* out.p ≡ νq *)
Lemma scong_out_nu_dec: forall p q : Pi,
    {Out p ≡ Nu q} + {~ Out p ≡ Nu q}.
Proof.
  intros p q.
  case (scong_nu_out_dec q p).
  - left. symmetry. assumption.
  (* FIXME: use symmetry unstead of nscong_sym. *)
  - right. apply nscong_sym. assumption.
Qed.

(* out.p ≡ 0 *)
Lemma scong_out_zero_dec: forall p : Pi,
    {Out p ≡ Zero} + {~ Out p ≡ Zero}.
Proof.
  intros p.
  case (scong_zero_dec (Out p)).
  - left. symmetry. assumption.
  (* FIXME: use symmetry unstead of nscong_sym. *)
  - right. apply nscong_sym. assumption.
Qed.

Lemma scong_out_dec: forall p q : Pi,
    {Out p ≡ q} + {~ Out p ≡ q}.
Proof.
  intros p q.
  admit
Admitted.

(* ------------------------------------------------------------------- *)
(* in.p ≡ q                                                            *)

(* in.p ≡ out.q *)
Lemma scong_in_out_dec: forall p q : Pi,
    {In p ≡ Out q} + {~ In p ≡ Out q}.
Proof.
  intros p q.
  case (scong_out_in_dec q p).
  - left. symmetry. assumption.
  (* FIXME: use symmetry unstead of nscong_sym. *)
  - right. apply nscong_sym. assumption.
Qed.

(* in.p ≡ in.q *)
Lemma scong_in_in_eq_dec: forall p q : Pi,
    p ≡ q -> {In p ≡ In q} + {~ In p ≡ In q}.
Proof.
  intros p q Hscong.
  left; exact (scong_in p q Hscong).
Qed.

Lemma scong_in_in_neq_dec: forall p q : Pi,
    ~ p ≡ q -> {In p ≡ In q} + {~ In p ≡ In q}.
Proof.
  intros p q _Hscong.
  right; unfold not; intro Hsin.
  inversion Hsin.
  - exact (_Hscong H1).
  - symmetry in H1. exact (_Hscong H1).
Qed.

Lemma scong_in_in_dec: forall p q : Pi,
    {p ≡ q} + {~ p ≡ q} -> {In p ≡ In q} + {~ In p ≡ In q}.
Proof.
  intros p q HDscong.
  case HDscong.
  - intro Hscong; exact (scong_in_in_eq_dec p q Hscong).
  - intro _Hscong; exact (scong_in_in_neq_dec p q _Hscong).
Qed.

(* (* in.p ≡ q | r *) *)
(* Lemma scong_in_par_dec: forall p q r : Pi, *)
(*     {In p ≡ Par q r} + {~ In p ≡ Par q r}. *)
(* Proof. *)
(*   right. discriminate. *)
(* Qed. *)

(* in.p ≡ νq *)
Lemma scong_in_nu_dec: forall p q : Pi,
    {In p ≡ Nu q} + {~ In p ≡ Nu q}.
Proof.
  intros p q.
  case (scong_nu_in_dec q p).
  - left. symmetry. assumption.
  (* FIXME: use symmetry unstead of nscong_sym. *)
  - right. apply nscong_sym. assumption.
Qed.

(* in.p ≡ 0 *)
Lemma scong_in_zero_dec: forall p : Pi,
    {In p ≡ Zero} + {~ In p ≡ Zero}.
Proof.
  intros p.
  case (scong_zero_dec (In p)).
  - left. symmetry. assumption.
  (* FIXME: use symmetry unstead of nscong_sym. *)
  - right. apply nscong_sym. assumption.
Qed.

(* (* ------------------------------------------------------------------- *) *)
(* (* p | q ≡ r                                                           *) *)

(* (* p | q ≡ out.r *) *)
(* Lemma scong_par_out_dec: forall p q r : Pi, *)
(*     {Par p q ≡ Out r} + {~ Par p q ≡ Out r}. *)
(* Proof. *)
(*   intros p q r. *)
(*   case (scong_out_par_dec r p q). *)
(*   - left. symmetry. assumption. *)
(*   (* FIXME: use symmetry unstead of nscong_sym. *) *)
(*   - right. apply nscong_sym. assumption. *)
(* Qed. *)

(* (* p | q ≡ in.r *) *)
(* Lemma scong_par_in_dec: forall p q r : Pi, *)
(*     {Par p q ≡ In r} + {~ Par p q ≡ In r}. *)
(* Proof. *)
(*   intros p q r. *)
(*   case (scong_in_par_dec r p q). *)
(*   - left. symmetry. assumption. *)
(*   (* FIXME: use symmetry unstead of nscong_sym. *) *)
(*   - right. apply nscong_sym. assumption. *)
(* Qed. *)

(* (* p | q ≡ r | s *) *)
(* Lemma scong_par_par_dec: forall p q r s : Pi, *)
(*     {Par p q ≡ Par r s} + {~ Par p q ≡ Par r s}. *)
(* Proof. *)
(*   intros p q r s. *)
(*   compare p r. *)
(*   - compare q s. *)
(*     * (* p ≡ r & q ≡ s*) *)
(*       intros prfQS prfPR. *)
(*       destruct prfPR. destruct prfQS. *)
(*       left. reflexivity. *)
(*     * (* p ≡ q & ~ q ≡ s *) *)
(*       intros _prfQS prfPR. *)
(*       right. congruence. *)
(*   - compare q s. *)
(*     * (* ~ p ≡ r & q ≡ s*) *)
(*       intros prfQS _prfPR. *)
(*       right. congruence. *)
(*     * (* ~ p ≡ q & ~ q ≡ s *) *)
(*       intros _prfQS _prfPR. *)
(*       right. congruence. *)
(* Qed. *)

(* (* p | q ≡ νr *) *)
(* Lemma scong_par_nu_dec: forall p q r : Pi, *)
(*     {Par p q ≡ Nu r} + {~ Par p q ≡ Nu r}. *)
(* Proof. *)
(*   intros p q r. *)
(*   case (scong_nu_par_dec r p q). *)
(*   - left. symmetry. assumption. *)
(*   (* FIXME: use symmetry unstead of nscong_sym. *) *)
(*   - right. apply nscong_sym. assumption. *)
(* Qed. *)

(* (* p | q ≡ 0 *) *)
(* Lemma scong_par_zero_dec: forall p q : Pi, *)
(*     {Par p q ≡ Zero} + {~ Par p q ≡ Zero}. *)
(* Proof. *)
(*   intros p q. *)
(*   case (scong_zero_par_dec p q). *)
(*   - left. symmetry. assumption. *)
(*   (* FIXME: use symmetry unstead of nscong_sym. *) *)
(*   - right. *)
(*     symmetry. *)
(*     apply nscong_sym. assumption. *)
(* Qed. *)

(* ------------------------------------------------------------------- *)
(* p ≡ q                                                               *)
(* Require Import Coq.Program.Equality. *)

Theorem scong_dec: forall p q : Pi,
    {p ≡ q} + {~ p ≡ q}.
Proof.
  intros p q.
  (* dependent induction p. destruct q.  *)
  induction p.
  - case IHp.
    + intro Hscong.

    intros.
  - case IHq.
    + admit.
    + intro HDscong; exact

Admitted.
