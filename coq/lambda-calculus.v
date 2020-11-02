Require Import Arith.
Require Import ssreflect.
Set Nested Proofs Allowed.

(** * λ-calculus term -- 7.1

Syntax for a λ-calculus with a nameless representation of terms.

Notes from [Pie02]:

- To save writing too many parentheses, it exists two conventions when
  writing lambda-terms in linear form.  First, application associates
  to the left -- that is, `s t u` stands for the same tree as `(s t)
  u`.  Second, the bodies of abstraction are taken to extend as far to
  the right as possible, so that, for example, `λx. λy. x y x` stands
  for the same tree as `λx. (λy. ((x y) x))`.

- The nameless representation avoids renaming by using De Bruijn
  notation.  It replaces named variables by natural numbers, where the
  number `k` stands for "the variable bound by the `k`'th enclosing
  λ". For example, the ordinary term `λx.x` corresponds to the
  nameless term `λ.0`, while `λx.λy. x (y x)` corresponds to `λ.λ. 1
  (0 1)`.
*)
Inductive term: Set :=
| TmVar: nat -> term
| TmAbs: term -> term
| TmApp: term -> term -> term.

Notation "% k" := (TmVar k) (at level 20).
Infix "@@" := TmApp (at level 25, left associativity).
Notation "λ∙ t" := (TmAbs t) (at level 35).

(** ** Some examples -- 6.1.1 *)
(* λs. λz. z *)
Example c0: term := λ∙ λ∙ %0.
(* λs. λz. s (s z) *)
Example c2: term := λ∙ λ∙ %1 @@ (%1 @@ %0).
(* λm. λn. λs. λz. m s (n s z) *)
Example plus: term := λ∙ λ∙ λ∙ λ∙ %3 @@ %1 @@ (%2 @@ %1 @@ %0).
(* (plus c2) c0 *)
Example c2': term := plus @@ c2 @@ c0.
(* (λx. (λx. x)) (λx. x) *)
Example foo: term := (λ∙ (λ∙%0)) @@ (λ∙%0).

Lemma var_inj: forall k l, TmVar k = TmVar l -> k = l.
Proof. intros. injection H. trivial. Qed.

(** * n-Terms -- 6.1.2

The elements of `forall t, (Terms t n)` are terms `t` with at most `n`
free variables.

- A 0-Terms is a term with no free variables.
- A 1-Terms is a term with at most one free variable.
- A n-Terms is a term with at most n free variables, numbered between
  0 and n-1.
*)
Inductive Terms: term -> nat -> Prop :=
| TermsVar: forall k n,
    0 <= k -> k < n ->  Terms (TmVar k) n
| TermsAbs: forall t n,
    Terms t (S n) -> Terms (TmAbs t) n
| TermsApp: forall t1 t2 n,
    Terms t1 n -> Terms t2 n -> Terms (TmApp t1 t2) n.

(** **Unit tests *)
Example unTerms: Terms (%0) 1 := TermsVar 0 1 (Nat.le_refl 0)  Nat.lt_0_1.
Example zeroTerms: Terms (λ∙%0) 0 := TermsAbs (%0) 0 unTerms.
Example zeroTerms': Terms (λ∙λ∙%0 @@ %1) 0.
Proof.
  apply TermsAbs. apply TermsAbs. apply TermsApp.
  - apply TermsVar. Nat.order'. Nat.order'.
  - apply TermsVar. Nat.order'. Nat.order'.
Qed.
Example notZeroTerms: ~ Terms (%0) 0.
Proof. intro EQ. inversion EQ. apply (Nat.lt_irrefl 0 H1). Qed.

(** **Properties *)

(** A term only composed of a var `%k` is obviously a free variable
(there is no lambda abstraction to bind that variable). Hence, that
term could not be a member of `0-Terms` the set of term with only
bound variables. *)
Lemma never_0_terms_var: forall k, ~ Terms (%k) 0.
Proof.
  intros k EQ.
  inversion EQ.
  apply (Nat.nlt_0_r k H1).
Qed.

(** A var `%k` member of an n-Terms is obviously a free variable so
that `n > k`. *)
Lemma terms_var_lt_k_n: forall k n, Terms (%k) n  -> k < n.
Proof. intros. inversion H. trivial. Qed.

(** A var `%k` member of a 1-Terms has only one free-variable such
that `0 <= k < 1` which implies `k = 0`. *)
Lemma un_terms_0_var: forall (k: nat), Terms (%k) 1 <-> k = 0.
  Lemma lt_stric_1: forall n, ~ S n < 1.
  Proof.
    intros n EQ.
    destruct n.
    - apply (Nat.lt_irrefl 1 EQ).
    -
      SearchHead (forall (n m: nat), (S n) < _ -> _).
      apply lt_S_n in EQ.
      apply Nat.lt_lt_0 in EQ.
      SearchHead (forall n, ~ n < n).
      assert (NEQ_0_0 := Nat.lt_irrefl 0).
      apply (NEQ_0_0 EQ).
  Qed.

  Lemma lt_eq_0_1: forall (k: nat), 0 <= k -> k < 1 -> k = 0.
  Proof.
    intros k HSupK HLowK.
    destruct k.
    - reflexivity.
    - assert (HFalse := lt_stric_1 k HLowK).
      contradict HFalse.
  Qed.

Proof.
  split.
  - intros.
    inversion H.
    SearchHead (forall n k, 0 <= k -> k < 1 -> 0 = k).
    exact (lt_eq_0_1 k H1 H2).
  - intros.
    rewrite H.
    exact unTerms.
Qed.

(** A term `t` that has at most `n` free variables also has at most
`(s n)` free variables. *)
Theorem terms_r: forall t n, Terms t n -> Terms t (S n).

  (* Variable case *)
  Lemma terms_var_r: forall k n, Terms (%k) n -> Terms (%k) (S n).
  Proof.
    intros.
    apply TermsVar.
    inversion H.
    - (* 0 <= k *) trivial.
    - (* k < S n*)
      assert (LT := terms_var_lt_k_n k n H).
      SearchHead (forall n m, n < m -> n < S m).
      apply (Nat.lt_lt_succ_r k n LT).
  Qed.

  (* Abstraction case *)
  Lemma terms_abs_r: forall t n, Terms (λ∙ t) n -> Terms (λ∙ t) (S n).
  Proof.
    intros.
    induction H.
    -
      SearchHead (forall n m, n < m -> n < S m).
      assert (LT := Nat.lt_lt_succ_r k n H0).
      exact (TermsVar k (S n) H LT).
    - apply TermsAbs.
      trivial.
    - apply TermsApp.
      trivial. trivial.
  Qed.

  (* Application case *)
  Lemma terms_app_r: forall t1 t2 n, Terms (t1 @@ t2) n -> Terms (t1 @@ t2) (S n).
  Proof.
    intros.
    induction H.
    -
      SearchHead (forall n m, n < m -> n < S m).
      assert (LT := Nat.lt_lt_succ_r k n H0).
      exact (TermsVar k (S n) H LT).
    - apply TermsAbs.
      trivial.
    - apply TermsApp.
      trivial. trivial.
  Qed.

Proof.
  intros.
  destruct t.
  - apply (terms_var_r n0 n H).
  - apply (terms_abs_r t n H).
  - apply (terms_app_r t1 t2 n H).
Qed.


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

and it should remain like this after substitution!

When a substitution goes under a λ-abstraction (i.e., `λ.2`), the
context in which the substitution takes place (i.e., `λ.(0 1)`)
becomes one variable longer that the original.  Thus we need to
increment free variables in the substitued term up by one so they
remain free.  But we should not increment bound variables.  So the
result of the previous substitution is:

[2 ↦ λ.(0 1)] λ.2
= λ.λ(0 2)
    ╰─┴╴│──── The first variable *remains* a bound variable to `λ`
        ╰──── The second variable *remains* free

We use the following notation `↑c,d[t]` for increment variables of `t`
by `d` level with `c` is the index of the first free variable.  All
identifiers `k < c` in `t` are bound a so should not be shifted, while
identifiers `k >= c` in `t` are free and should be shifted.

Shift term by `d`
↑c,d[k]     = k              if k < c    (1)
↑c,d[k]     = k + d          if k >= c   (2)
↑c,d[λ.t]   = λ.↑c+1,d[t]               (3)
↑c,d[t1 t2] = ↑c,d[t1] ↑c,d[t2]        (4)

For instance:

↑0,1[λ.(0 1)] =3⇒ λ.↑1,1[(0 1)]
               =4⇒ λ.(↑1,1[0] ↑1,1[1])
               =1⇒ λ.(0 ↑1,1[1])
               =2⇒ λ.(0 2)
*)
Fixpoint shift_walk (c: nat) (d: nat) (t: term): term :=
  match t with
  | TmVar k => match lt_dec k c with
              | left _ => TmVar k        (* k < c  *)
              | _      => TmVar (k + d)  (* k >= c *)
              end
  | TmAbs t => TmAbs (shift_walk (S c) d t)
  | TmApp t1 t2 =>
    let t1' := shift_walk c d t1 in
    let t2' := shift_walk c d t2 in
    TmApp t1' t2'
  end.


(* We also write ↑1[t] for ↑0,1[t] *)
Definition shift (d: nat) (t: term) := shift_walk 0 d t.

Compute (match lt_dec 0 1 with
         | left _  => true
         | right _ => false
        end).

Eval compute in (shift 1 (λ∙ %0 @@ %1)).
Eval compute in (shift 2 (λ∙ λ∙ %1 @@ %0 @@ %2)).
Eval compute in (shift 2 (λ∙ %0 @@ %1 @@ (λ∙ %0 @@ %1 @@ %2))).


(* Errata  *)
(* p 79, exercise 6.2.3 *)

(*     The statement to be shown is not quite true for d < 0.  To be true for *)
(*     n>=0 and any integral d, it should be "Show that, if t is an n-term and, *)
(*     if d < 0, the free variables of t are all at least |d|, then <shift t up *)
(*     by d above cutoff c> is a (max(n+d,0))-term." *)

Lemma shift_zero: forall t c, shift_walk c 0 t = t.
Proof.
  intros.
  elim: t c.
  - intros. simpl. destruct (lt_dec n c).
    + reflexivity.
    + assert (EQ:=Nat.add_0_r n). rewrite EQ. reflexivity.
  - simpl. congruence.
  - simpl. congruence.
Qed.


(** * Substitution -- 6.2.4

The substitution of a term `s` for a variable number `j` in a term
`t`, written `[j ↦ s] t`.

*)
(* Fixpoint substitute_walk (c: nat) (j: nat) (s t: term) : term := *)
(*   match t with *)
(*   | TmVar k => match eq_nat_dec k (j + c) with *)
(*                | right _ => (shift c s)  (* k = j+c *) *)
(*                | left  _ => TmVar k      (* k != j  *) *)
(*               end *)
(*   | TmAbs t => TmAbs (substitute_walk (S c) j s t) *)
(*   | TmApp t1 t2 => *)
(*     let t1' := substitute_walk c j s t1 in *)
(*     let t2' := substitute_walk c j s t2 in *)
(*     TmApp t1' t2' *)
(*   end. *)
(* Definition substitute (j: nat) (s t: term) := substitute_walk 0 j s t. *)
Fixpoint substitute (j: nat) (s t: term) : term :=
  match t with
  | TmVar k => match eq_nat_dec k j with
               | left  _ => s            (* k =  j *)
               | _       => TmVar k      (* k != j  *)
              end
  | TmAbs t => TmAbs (substitute (S j) (shift 1 s) t)
  | TmApp t1 t2 =>
    let t1' := substitute j s t1 in
    let t2' := substitute j s t2 in
    TmApp t1' t2'
  end.

Compute (match eq_nat_dec 0 0 with
         | left _  => true
         | right _ => false
        end).

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

Fixpoint unshift_walk (c: nat) (t: term): term :=
  match t with
  | TmVar k => match lt_dec k c with
               | left _ => TmVar k         (* k < c  *)
               | _      => TmVar (pred k)  (* k >= c *)
              end
  | TmAbs t => TmAbs (unshift_walk (S c) t)
  | TmApp t1 t2 =>
    let t1' := unshift_walk c t1 in
    let t2' := unshift_walk c t2 in
    TmApp t1' t2'
  end.
Definition unshift t: term := unshift_walk 0 t.

Fixpoint eval1 (t: term) {struct t}: option term :=
  let isval := fun (t: term) =>
                 match t with
                 | TmAbs _ => true
                 | _       => false
                 end
  in match t with
     (* E-AppAbs*)
     | TmApp (TmAbs t12) v2 =>
       if isval v2
       then let t' := substitute 0 (shift 1 v2) t12
            in Some (unshift t')
       else None
     | TmApp t1 t2 =>
       if isval t1
       (* E-App2 *)
       then match eval1 t2 with
            | Some t2' => Some (TmApp t1 t2')
            | _        => None
            end
       (* E-App1 *)
       else match eval1 t1 with
            | Some t1' => Some (TmApp t1' t2)
            | _        => None
            end
     | _ => None
     end.

Fixpoint eval (t: term): term :=
  match t with
    | TmVar k => t
    | TmApp (TmAbs t1) t2 =>
      let v := (eval t2)
      in unshift (substitute 0 (shift 1 v) t1)
    | TmApp t1 t2 => TmApp (eval t1) (eval t2)
    | TmAbs t => TmAbs (eval t)
  end.

Inductive isVal : term -> Set :=
  | here t1 : isVal (TmAbs t1).

Hint Constructors isVal.

Inductive betared1 : term -> term -> Set :=
  | betared1appabs t1 t2 : isVal t2 -> betared1 ((λ∙ t1) @@ t2)  (unshift (substitute 0 (shift 1 t2) t1))
  | betared1app2 t1 t2 t2' : isVal t1 -> betared1 t2 t2' -> betared1 (t1 @@ t2) (t1 @@ t2')
  | betared1app1 t1 t2 t1' : betared1 t1 t1' -> betared1 (t1 @@ t2) (t1' @@ t2).

Hint Constructors betared1.

Compute (eval ((λ∙ %1 @@ %0 @@ %2) @@ (λ∙%0))).

Example test : betared1 ((λ∙ %1 @@ %0 @@ %2) @@ (λ∙%0)) (%0 @@ (λ∙%0) @@ %1).
Proof.
  auto with betared1.



Compute (eval c2).
(* c0 = λs. λz. z *)
(* plus = λm. λn. λs. λz. m s (n s z) *)
(* plus @@ c0 @@ c0 = (λm. λn. λs. λz. m s (n s z)) (λs. λz. z) (λs. λz. z)
                    ~ (λ . λ . λ . λ . 3 1 (2 1 0)) (λ . λ . 0) (λ . λ . 0)
            AppAbs => (λn. λs. λz. (λs. λa. a) s (n s z)) (λs. λz. z)
                    ~ (λ . λ . λ . (λ . λ . 0) 1 (2 1 0)) (λ . λ . 0)
            AppAbs => λs. λz. (λs. λa. a) s ((λs. λa. a) s z)
                    ~ λ . λ . (λ . λ . 0) 1 ((λ . λ . 0) 1 0)

*)
(* plus @@ c0 @@ c0 = (λm. λn. λs. λz. m s (n s z)) (λs. λz. z) (λs. λz. z)
                    ~ (λ . λ . λ . λ . 3 1 (2 1 0)) (λ . λ . 0) (λ . λ . 0)
            AppAbs => (λn. λs. λz. (λs. λa. a) s (n s z)) (λs. λz. z)
                    ~ (λ . λ . λ . (λ . λ . 0) 1 (2 1 0)) (λ . λ . 0)
            AppAbs => λs. λz. (λs. λa. a) s ((λs. λa. a) s z)
                    ~ λ . λ . (λ . λ . 0) 1 ((λ . λ . 0) 1 0)
*)

Example c0': term := (plus @@ c0 @@ c0).
Compute (eval1 c0').
Compute (eval c0').
Compute (eval (eval (eval (eval c0')))).
Compute (eval c2').

Extraction Language Scheme.
Extraction eval1.
