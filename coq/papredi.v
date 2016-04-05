Require Import Utf8_core.
Require Import Peano.

Section Types.

  (* Égalité, Définition équivalente à Eq, utilisé avec la syntax *)
  Inductive paths {A : Type} (a : A) : A -> Type :=
    idpath : paths a a.
  (* Génère paths, paths_inc, paths_rec, paths_rect *)
  (* equivalence:
  Inductive eqq {A : Type} (a : A) : A -> Type :=
    eqq_refl : eqq a a.
  *)

  Check paths_rect. (* AFfiche la preuve *)

  (* Def du Produit, def à la prog *)
  Definition to_prod (A B:Type) : A -> B -> A*B
    := fun (a:A) (b:B) => (a,b). (* definition par prog *)
  (* Previous à la preuve *)
  (* Lemma to_prod (A B:Type) : A -> B -> A*B *)
  (* Proof. *)
  (*   intros a b. exact (a,b). *)
  (* Defined. *)

  (* Preuve que deux couple sont egaux, def à la preuve *)
  Lemma path_prod (A B:Type) (a a':A) (b b':B)
  : a=a' -> b=b' -> (a,b) = (a',b').
  Proof.
    intros p q. destruct p, q. reflexivity. (* definition par preuve *)
    (* intros p q. destruct p, q. exact eqq_refl. *)
  Defined.

  Print path_prod. (* Affiche la definition *)

  (* Somme *)
  Definition to_sum_l (A B:Type) : A -> B -> A+B
    := fun (a:A) (b:B) => inl a.

  Definition to_sum_r (A B:Type) : A -> B -> A+B
    := fun (a:A) (b:B) => inr b.

  Lemma path_sum (A B:Type) (z z' : A + B)
  : match z with
      | inl z0 => match z' with
                    | inl z'0 => z0 = z'0
                    | inr _ => False
                  end
      | inr z0 => match z' with
                    | inl _ => False
                    | inr z'0 => z0 = z'0
                  end
    end -> z = z'.
  Proof.
    intro H. destruct z, z'.
    - destruct H. reflexivity. (* 1er cas *)
    - contradiction.           (* 2eme cas *)
    - contradiction.           (* 3eme cas *)
    - destruct H. reflexivity. (* 4eme cas *)
  Defined.

  (* Flèche: In coq theory, `f = g` isn't provable  *)
  Definition path_arrow (A B:Type) (f g : A -> B)
  : (forall x:A, f x = g x) -> f = g.
  Abort. (* We abort the definition *)

End Types.

(* Produit dépendant, forall x: A, B x *)
(* Some dépendant, {x: A & B x} *)

Section Naturels.

  Inductive Naturel : Type :=
|zero : Naturel
|succ : Naturel -> Naturel.

  (* Check Naturel_rect. *)

  Notation "'un'" := (succ zero).
  Notation "'deux'" := (succ un).
  Notation "'trois'" := (succ deux).
  Notation "'quatre'" := (succ trois).
  Notation "'cinq'" := (succ quatre).
  Notation "'six'" := (succ cinq).
  Notation "'sept'" := (succ six).
  Notation "'huit'" := (succ sept).
  Notation "'neuf'" := (succ huit).
  Notation "'dix'" := (succ neuf).

  Fixpoint Plus (n:Naturel) (m:Naturel) : Naturel
    := match n with
         |zero => m
         |succ n' => succ (Plus n' m)
       end.

  Eval compute in (Plus cinq trois).

  Definition Plus' (n:Naturel) (m:Naturel) : Naturel.
    induction n.
    exact m.
    - apply succ. apply IHn.
  Defined.

  Print Plus'.
  Eval compute in (Plus' cinq trois).

  (* simple: beta reduction, *)

  Lemma Plus_is_Plus' n m : Plus n m = Plus' n m.
    induction n.
    - unfold Plus, Plus'. simpl. reflexivity.
    - unfold Plus, Plus'. simpl.
      apply f_equal.
      unfold Naturel_rec, Naturel_rect. exact IHn.
  Defined.

  Fixpoint Plus_is_Plus'' n m : Plus n m = Plus' n m
    := match n with
         |zero => eq_refl
         |succ n' => f_equal succ (Plus_is_Plus'' n' m)
       end.

  Lemma Plus_is_Plus'_is_Plus_is_Plus'' n m : Plus_is_Plus' n m = Plus_is_Plus'' n m.
    unfold Plus_is_Plus', Plus_is_Plus''.
    induction n.
    - simpl. reflexivity.
    - simpl. apply f_equal.
      exact IHn.
  Qed.

  Fixpoint Mult (n:Naturel) (m:Naturel) : Naturel
    := match n with
         |zero => zero
         |succ n' => Plus m (Mult n' m)
       end.

  Fixpoint Est_pair (n:Naturel) : Type
    := match n with
         |zero => True
         |succ m => Est_impair m
       end

  with Est_impair (n:Naturel) : Type
       := match n with
            |zero => False
            |succ m => Est_pair m
          end.

  (* Eval simpl in (Est_pair huit). Eval simpl in (Est_impair huit).  *)
  (* Eval simpl in (Est_pair sept). Eval simpl in (Est_impair sept). *)

  Definition pair_impair (n:Naturel) : (Est_pair n) -> (Est_impair n -> False).
    intros Hp Hi.
    induction n.
    exact Hi.
    simpl in *.
    exact (IHn Hi Hp).
  Qed.

  (* m est divisible par n *)
  Definition divisible (n m:Naturel) : Type
    := {k:Naturel & n = Mult k m}.

  Lemma cinq_divise_dix : divisible dix cinq.
    unfold divisible.
    exists deux.
    unfold Mult. unfold Plus. reflexivity.
  Qed.

  Lemma cumul_div (p q r : Naturel)
  : divisible r q -> divisible q p -> divisible r p.
    intros H1 H2.
    unfold divisible in *.
    destruct H1 as [k eq_k]. destruct H2 as [l eq_l].
    rewrite eq_k.
    rewrite eq_l.
    (* ??? *)
  Abort.

  Lemma Mult_zero (n:Naturel) : Mult n zero = zero.
    induction n.
    - simpl. reflexivity.
    - simpl. exact IHn.
  Defined.

  Definition distributivite_gauche (n m p:Naturel) : Mult n (Plus m p) = Plus (Mult n m) (Mult n p).
    induction n.
    - simpl. reflexivity.
    - simpl. rewrite IHn. simpl.
  Admitted.

  Definition distributivite_droite (n m p:Naturel) : Mult (Plus m p) n = Plus (Mult m n) (Mult p n).
    induction n.
    - simpl. repeat rewrite Mult_zero. reflexivity.
    -
  Admitted.

  Lemma Mult_assoc (p q r : Naturel)
  : Mult p (Mult q r) = Mult (Mult p q) r.
    induction p.
    - simpl. reflexivity.
    - simpl.
      rewrite distributivite_droite.
      rewrite IHp.
      reflexivity.
  Qed.

  Lemma cumul_div (p q r : Naturel)
  : divisible r q -> divisible q p -> divisible r p.
    intros H1 H2.
    unfold divisible in *.
    destruct H1 as [k eq_k]. destruct H2 as [l eq_l].
    rewrite eq_k.
    rewrite eq_l.
    rewrite Mult_assoc.
    exists (Mult k l). reflexivity.
  Qed.

  (* Exemples de types dépendants avec les nat *)

  (* Local Open Scope nat_scope. *)

  (* nat est le Natureal de coq *)
  Definition plus_petit (n:nat) := {p : nat & p <= n}.
  (* Check plus_petit. *) (* done le type de plus_petit *)

  (* Pi type *)
  Definition to_plus_petit : forall (n:nat), plus_petit n.
    intro n. exists n. exact (le_n n).
  Defined.

End Naturels.

Section Listes.

  Inductive Liste (A:Type) : Type
    := |Vide : Liste A
|Cons : A -> Liste A -> Liste A.

  Arguments Vide {A}.
  Arguments Cons {A} l _.

  Fixpoint longueur {A:Type} (l : Liste A) : Naturel
    := match l with
         |Vide => zero
         |Cons _ l' => succ (longueur l')
       end.

  Definition longueur' {A:Type} (l : Liste A) : Naturel.
    induction l.
    - exact zero.
    - exact (succ IHl).
  Defined.

End Listes.

Section hListes.

  Open Scope list_scope.
  Notation "'un'" := (succ zero).
  Notation "'deux'" := (succ un).
  Notation "'trois'" := (succ deux).
  Notation "'quatre'" := (succ trois).
  Notation "'cinq'" := (succ quatre).
  Notation "'six'" := (succ cinq).
  Notation "'sept'" := (succ six).
  Notation "'huit'" := (succ sept).
  Notation "'neuf'" := (succ huit).
  Notation "'dix'" := (succ neuf).

  Inductive hlist {A:Type} {B:A -> Type} : list A -> Type :=
  | HNil : hlist nil
  | HCons : forall {x : A} {ls : list A}, B x -> hlist ls -> hlist (x :: ls).

  Infix "¨¨" := (HCons) (at level 60, right associativity).

        (* a1  ::   a2   ::   a3   ::   a4   :: ... *)
        (* |        |         |         |           *)
        (* v        v         v         v           *)
        (* x1  ¨¨   x2   ¨¨   x3   ¨¨   x4   ¨¨ ... *)
        (* de types                                 *)
        (* B a1     B a2      B a3      B a4        *)

  Definition example1 : hlist (B:=fun _ => Naturel) (zero :: zero :: zero :: nil).
  (* := HCons deux (HCons trois (HCons cinq HNil))) *)
    apply HCons. exact deux.
    apply HCons. exact trois.
    apply HCons. exact cinq.
    apply HNil.
  Defined.
  Print example1.

  Definition example2 : hlist (B:= fun n:Naturel => match n with |zero => bool |_ => Naturel end)
                              (deux :: zero :: huit :: nil).
    apply HCons. exact zero.
    apply HCons. exact false.
    apply HCons. exact deux.
    apply HNil.
  Defined.
  Print example2.

  Definition example3 : hlist (B := fun n:nat => plus_petit n) (0::4::2::nil).
    apply HCons.
    exists 0. apply le_n.
    apply HCons.
    exists 3. apply le_S. apply le_n.
    apply HCons.
    exists 0. apply le_S. apply le_S. apply le_n.
    apply HNil.
  Defined.
  Print example3.

End hListes.
