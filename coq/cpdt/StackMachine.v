(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* CPDT Chapter 2                                                      *)
(* See, http://adam.chlipala.net/cpdt/                                 *)
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

Require Import Bool Arith Cpdt.CpdtTactics.
Require Import List.
Set Implicit Arguments.
Set Asymmetric Patterns.

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* Arithmetic Expressions Over Natural Numbers *)

(* ------------------------------------------------------------------- *)
(* Source Language *)

(* Inductive type *)
Inductive exp: Set :=
| Const: nat -> exp
| Plus: exp -> exp -> exp
| Times: exp -> exp -> exp.

(* Write down some expressions *)
Definition fortytwo: exp := (Const 42).
Definition exp1 := (Plus (Const 2) (Const 2)).
Definition exp2 := (Times exp1 (Const 7)).

(* Give a denotational semantics of the language. *)
(* Declare *recursive* def with `Fixpoint` *)
Fixpoint expDenote(e: exp): nat :=
  match e with
  | Const n => n
  | Plus e1 e2 => plus (expDenote e1) (expDenote e2)
  | Times e1 e2 => mult (expDenote e1) (expDenote e2)
  end.

(* Test the interpreter *)
Eval simpl in expDenote fortytwo.  (* 42 *)
Eval simpl in expDenote exp1.      (* 4 *)
Eval simpl in expDenote exp2.      (* 28 *)

(* ------------------------------------------------------------------- *)
(* Target Language: a simple stack machine *)

(* A program is a list of instructions: "2,2,plus,7,times"

An instruction tells either:
- To push a constant onto the stack (iConst)
- Or, to pops out two arguments, applies a binary operator to them
  (iPlus/iTimes), and pushes the result back onto the stack.

> prog  |stack    prog  |stack    prog  |stack    prog  |stack
> ------+-----    ------+-----    ------+-----    ------+-----
> 2     |nil      2     |2        plus  |2        7     |4
> 2     |     ~>  plus  |     ~>  7     |2    ~>  times |
> plus  |         7     |         times |
> 7     |         times |
> times |

Think in terms of WebAssembly, see Figure 1
https://www.cl.cam.ac.uk/~caw77/papers/mechanising-and-verifying-the-webassembly-specification.pdf
*)
Inductive instr: Set :=
| iConst: nat -> instr
| iPlus: instr
| iTimes: instr.
Definition prog: Set := list instr.

(* Write down some instructions/programs *)
Definition iFortytwo: instr := iConst 42.
Definition prog1: prog :=
  iConst 2 :: iConst 2 :: iPlus :: nil.
Definition prog2: prog :=
  (iConst 7 :: nil) ++ prog1 ++ (iTimes :: nil).

(* I can also write a program that don't make sense. Here, I try to do
a plus whereas only I have one constant *)
Definition wrongProg: prog := iConst 2 :: iPlus :: nil.

(* We can give instructions meaning as a function from a stack before
to a stack after the execution of the instruction. Running an
instruction results in `None` in case of a stack underflow -- e.g.,
wrongProg -- and in `Some` when the result of the execution is OK --
e.g., prog1, prog2. *)
Definition stack: Set := list nat.
Definition instrDenote(i: instr) (s: stack) : option stack :=
  match i with
  | iConst n => Some (n :: s)
  | iPlus =>
    match s with
      | arg1 :: arg2 :: s' => Some (plus arg1 arg2 :: s')
      | _ => None
    end
  | iTimes =>
    match s with
      | arg1 :: arg2 :: s' => Some (mult arg1 arg2 :: s')
      | _ => None
    end
  end.

Fixpoint progDenote (p: prog) (s: stack): option stack :=
  match p with
    | nil => Some s
    | i :: p' =>
      match instrDenote i s with
        | None => None
        | Some s' => progDenote p' s'
      end
  end.

(* Test the interpreter *)
Eval simpl in instrDenote iFortytwo nil.  (* Some (42 :: nil) *)
Eval simpl in progDenote prog1 nil.       (* Some (4 :: nil) *)
Eval simpl in progDenote prog2 nil.       (* Some (28 :: nil) *)
Eval simpl in progDenote wrongProg nil.   (* None *)

(* The `iPlus/iTimes` of the target lang doesn't explicitly state in
its syntax that it takes two arguments. So I can syntactically write a
wrong program that leads to an underflow on the stack -- e.g.,
`wrongProg`.

Conversely, the source lang explicitly states that `Plus/Times` takes
two expressions as arguments. In other words, it is unmanageable for
the programmer to write a wrong program in the source lang.

So we would like to write a compiler from the source lang to the
target lang. *)

(* ------------------------------------------------------------------- *)
(* Compiler *)

Fixpoint compile (e: exp): prog :=
  match e with
  | Const n => iConst n :: nil
  | Plus e1 e2 => compile e2 ++ compile e1 ++ (iPlus :: nil)
  | Times e1 e2 => compile e2 ++ compile e1 ++ (iTimes :: nil)
  end.

Eval simpl in compile fortytwo.  (* iFortytwo *)
Eval simpl in compile exp1.      (* prog1 *)
Eval simpl in compile exp2.      (* prog2 *)

(* Unit tests *)
Lemma compileFortytwo: compile fortytwo = iFortytwo :: nil.
Proof. reflexivity. Qed.
Lemma compileExp1: compile exp1 = prog1.
Proof. reflexivity. Qed.
Lemma compileExp2: compile exp2 = prog2.
Proof. reflexivity. Qed.

(* So far, so good, but now, how can we be sure the compiler operates
correctly for /all/ input expressions. That is, evaluating a compiled
program produces a good underflow-free program (i.e., always a `Some`
and never a `None`). *)

(* ------------------------------------------------------------------- *)
(* Compiler Correctness *)

Theorem compile_correct: forall e: exp,
    progDenote(compile e) nil = Some (expDenote e :: nil).

  (* Strengthened the induction hypothesis (i.e., quantify over more
  parameters than the initial theorem): Here, quantify over a program
  `p` that is the "continuation" for the expression `e` *)
  Lemma compile_correct': forall (e: exp) (p: list instr) (s: stack),
    progDenote(compile e ++ p) s = progDenote p (expDenote e :: s).
  Proof.
    (* induction e. *)
    (* - intros. *)
    (*   simpl. *)
    (*   reflexivity. *)
    (* - intros. *)
    (*   unfold compile. fold compile. *)
    (*   unfold expDenote. fold expDenote. *)
    (*   SearchRewrite ((_ ++ _) ++ _). *)
    (*   Check app_assoc_reverse. *)
    (*   rewrite app_assoc_reverse. *)
    (*   rewrite IHe2. *)
    (*   rewrite app_assoc_reverse. *)
    (*   rewrite IHe1. *)
    (*   simpl. *)
    (*   reflexivity. *)
    (* - intros. *)
    (*   unfold compile. fold compile. *)
    (*   unfold expDenote. fold expDenote. *)
    (*   rewrite app_assoc_reverse. *)
    (*   rewrite IHe2. *)
    (*   rewrite app_assoc_reverse. *)
    (*   rewrite IHe1. *)
    (*   simpl. *)
    (*   reflexivity. *)
    induction e; crush.
  Qed.
  intros.
  SearchRewrite (_ ++ nil).
  Check app_nil_end.
  rewrite (app_nil_end (compile e)).
  rewrite (compile_correct' e nil nil).
  simpl.
  reflexivity.
Qed.


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* Typed Expressions *)

(* Our previous source language is simple enough so the programmer
cannot write bad expressions. But things get harder with several types
of literals (nat, bool, ...). One may mix nat and bool and write the
following program that doesn't make sense:

> (+ 42 (eq? 2 true))

`eq?' should only take bool expressions and `+' should take nat. So we
need a type system. We can encode it as in OS08.

@InProceedings{OS08,
  author = {Nicolas Oury and Wouter Swierstra},
  title = {The power of Pi},
  booktitle = {Proceeding of the 13th {ACM} {SIGPLAN} international
               conference on Functional programming, {ICFP}
               2008, Victoria, BC, Canada, September 20-28, 2008},
  pages = {39--50},
  year = 2008,
  url = {http://doi.acm.org/10.1145/1411204.1411213},
  doi = {10.1145/1411204.1411213}
}
*)

(* ------------------------------------------------------------------- *)
(* Source Language *)

Inductive type: Set := NAT | BOOL.

(* Indexed type family: an inductive type indexed by values *)
Inductive tbinop: type -> type -> type -> Set :=
| TPlus: tbinop NAT NAT NAT
| TTimes: tbinop NAT NAT NAT
| TEq: forall (t: type), tbinop t t BOOL  (* `∀ t' implem polymorphism *)
| TLt: tbinop NAT NAT BOOL.

Inductive texp: type ->  Set :=
| TNConst: nat -> texp NAT
| TBConst: bool -> texp BOOL
| TBinop: forall (t1 t2 t: type), tbinop t1 t2 t -> texp t1 -> texp t2 -> texp t.

(* Some expressions *)
Definition tFortytwo: texp NAT := TNConst 42.
Definition tTrue: texp BOOL := TBConst true.
Definition tExp1: texp NAT :=
  TBinop TTimes (TBinop TPlus (TNConst 2) (TNConst 2)) (TNConst 7).
Definition tExp2: texp BOOL :=
  TBinop (TEq NAT) (TBinop TPlus (TNConst 2) (TNConst 2)) (TNConst 7).
Definition tExp3: texp BOOL :=
  TBinop TLt (TBinop TPlus (TNConst 2) (TNConst 2)) (TNConst 7).
(* Definition tExpIllTyped := *)
(*   TBinop TPlus (TNConst 42) (TBinop (TEq BOOL) (TNConst 2) tTrue). *)

(* Denotational semantics *)
Definition typeDenote (t: type): Set :=
  match t with
  | NAT => nat
  | BOOL => bool
  end.

Definition tbinopDenote {t1 t2 tres: type} (op: tbinop t1 t2 tres):
    typeDenote t1 -> typeDenote t2 -> typeDenote tres :=
  match op with
  | TPlus => plus
  | TTimes => mult
  | TEq NAT => beq_nat
  | TEq BOOL => eqb
  | TLt => leb
  end.

Fixpoint texpDenote {t: type} (e: texp t): typeDenote t :=
  match e with
    | TNConst n => n
    | TBConst b => b
    | TBinop _ _ _ op e1 e2 => (tbinopDenote op) (texpDenote e1) (texpDenote e2)
  end.

(* Test the interpreter *)
Eval simpl in texpDenote tFortytwo.  (* 42 *)
Eval simpl in texpDenote tTrue.      (* true *)
Eval simpl in texpDenote tExp1.      (* 28 *)
Eval simpl in texpDenote tExp2.      (* false *)
Eval simpl in texpDenote tExp3.      (* true *)

(* Good, thanks to /indexed type family/ we cannot write ill-typed
expression such as `(+ 42 (eq? 2 true))' -- see, `tExpIllTyped'.

But we can use dependent types for almost everything. In the next, we
are going to use theme to force all stack machine programs to be
underflow-free. *)

(* ------------------------------------------------------------------- *)
(* Target Language *)

(* The previous stack type was `list nat' because the stack was only
made of nat. But, the stack now could contain both NAT and BOOL. So
it's a list of `type': one for each element of the stack. *)
Definition tstack: Set := list type.

(* And we indexed the type of an instruction by its stack. So that an
instruction describes how the stack moves: it describes the semantics
of the language regarding the stack. *)
Inductive tinstr: tstack -> tstack -> Set :=
| TiNConst: forall (s: tstack), nat -> tinstr s (NAT :: s)
| TiBConst: forall (s: tstack), bool -> tinstr s (BOOL :: s)
| TiBinop: forall (s: tstack) (t1 t2 tres: type),
    (* Here we do the tricks for underflow-free: we ensure that the
    top of the stack contains an element of type `t1' followed by an
    element of type `t2' -------v *)
    tbinop t1 t2 tres -> tinstr (t1 :: t2 :: s) (tres :: s).

About TiBinop.  (* Arguments t1, t2, tres are implicit *)

Inductive tprog: tstack -> tstack -> Set :=
| TNil: forall (s: tstack), tprog s s
| TCons: forall (s1 s2 s3: tstack),
    tinstr s1 s2 -> tprog s2 s3 -> tprog s1 s3.

About TCons.  (* Arguments s1, s2, s3 are implicit *)

(* Write down some instructions/programs *)
Definition tiFortytwo: tinstr nil (NAT :: nil) :=
  TiNConst nil 42.
Definition tiTrue: tinstr nil (BOOL :: nil) :=
  TiBConst nil true.
Definition tiProg0: tprog nil (NAT :: NAT :: nil) :=  (* 2,2 *)
  TCons (TiNConst nil 2)
        (TCons (TiNConst (NAT :: nil) 2)
               (TNil (NAT :: NAT :: nil))).
Definition tiProg0': tprog nil (NAT :: nil) :=  (* 2,2,+ *)
  TCons (TiNConst nil 2)
        (TCons (TiNConst (NAT :: nil) 2)
               (TCons (TiBinop nil TPlus)
                      (TNil (NAT :: nil)))).
(* Generalize to any stack (i.e., ∀ s) and let Coq infer the value of
the stack (i.e., _) *)
Definition tiProg0s: forall (s: tstack), tprog s (NAT :: s) :=  (* 2,2,+ *)
  fun s =>
    TCons (TiNConst _ 2)
          (TCons (TiNConst _ 2)
                 (TCons (TiBinop _ TPlus) (TNil _))).

Print tiProg0s.
(* fun s : tstack =>
    TCons (TiNConst s 2)
          (TCons (TiNConst (NAT :: s) 2)
                 (TCons (TiBinop s TPlus) (TNil (NAT :: s))))
 *)

Definition tiProg1: tprog nil (NAT :: nil) :=  (* 7,2,2,+,* *)
  TCons (TiNConst _ 7)
        (TCons (TiNConst _ 2)
               (TCons (TiNConst _ 2)
                      (TCons (TiBinop _ TPlus)
                             (TCons (TiBinop _ TTimes) (TNil _))))).

Definition tiProg2: tprog nil (BOOL :: nil) :=  (* 7,2,2,+,= *)
  TCons (TiNConst _ 7)
        (TCons (TiNConst _ 2)
               (TCons (TiNConst _ 2)
                      (TCons (TiBinop _ TPlus)
                             (TCons (TiBinop _ (TEq NAT)) (TNil _))))).

Definition tiProg3: tprog nil (BOOL :: nil) :=  (* 7,2,2,+,< *)
  TCons (TiNConst _ 7)
        (TCons (TiNConst _ 2)
               (TCons (TiNConst _ 2)
                      (TCons (TiBinop _ TPlus)
                             (TCons (TiBinop _ TLt) (TNil _))))).

(* Definition tiProgIllTyped1 :=  (* true,2,=,42,+ *) *)
(*   TCons (TiBConst _ true) *)
(*         (TCons (TiNConst _ 2) *)
(*                (TCons (TiBinop _ (TEq NAT)) *)
(*                       (TCons (TiNConst _ 42) *)
(*                              (TCons (TiBinop _ TPlus) (TNil _))))). *)

(* Underflow-free: *)
(* Definition tiProgIllTyped2: tprog nil (NAT :: nil) :=  (* 7,+ *) *)
(*   TCons (TiNConst _ 7) *)
(*         (TCons (TiBinop _ TPlus) *)
(*                (TNil _)). *)

(* In its previous version, `prog' was a `list instr'. It means we can
concatenate program with `++', as in `prog1 ++ prog2'. But now,
`tprog' is an inductive dependent type who doesn't comes with the
`++'. So we have to define it. *)
Fixpoint tconcat (s1 s2 s3: tstack) (p: tprog s1 s2):  tprog s2 s3 -> tprog s1 s3 :=
(* Here, we have to return an anonymous function `tconcat (p tprog):
tprog -> tprog' instead of the most natural `tconcat (p1 p2: tprog):
tprog' to circumvent problem with Coq type inference.

As general game plan, *always* put inside the `match' branches the
parameters -- i.e., `p': tprog s2 s3' -- whose types depend on the
type of the value being matched -- i.e., `p: tprog s1 s2`

With this tricks Coq can correctly infer that `s2` in `p': tprog s2
s3` is the same as `s1` (because `p` is type `tprog s1 s2` and TNil
`tprog s s` ⇒ `s1 = s2`) thus type of `p'` is `tprog s1 s3`.
 *)
  match p with
  | TNil _ => fun p' => p'
  | TCons _ _ _ i p0 => fun p' => TCons i (tconcat p0 p')
  end.

(* Now, I can write a prog made by the concatenation of two others *)
Definition tiProg4 := tconcat tiProg1 (tiProg0s _).

Print tiProg4.
(* tiProg4 = tconcat tiProg1 (tiProg0s (NAT :: nil)) *)
(*      : tprog nil (NAT :: NAT :: nil) *)

(* Representation of the tstack at runtime. That is, same as tstack
but with the Coq types. This function plays the same role as
`typeDenote'.

In my def, I would like to say something like that:

> Definition vstack: Set := list Set.
> Definition tstackDenote (ts: tstack): vstack := map typeDenote ts.

But it is not allowed. The `vstack` breaks the universe consistency.

My only option here is to transform the list of type into a tuple.
E.g., transform tstack0 into vstack0.

> Definition tstack0: tstack (* or, list type *) :=
>   NAT :: BOOL :: BOOL :: NAT :: nil.
> Definition vstack0: Set :=
>   (nat * (bool * (bool * (nat * unit)))).

This is the job of the following function. It is worth noting the
`%type` that instructs Coq the `*` is the Cartesian product to build
tuple instead of the `mult` operator.
*)
Fixpoint tstackDenote (ts: tstack): Set :=
  match ts with
  | nil => unit
  | t :: ts' => typeDenote t * tstackDenote ts'
  end%type.

Definition tstack0: tstack :=
  NAT :: BOOL :: BOOL :: NAT :: nil.
Eval simpl in tstackDenote tstack0.
(* = (nat * (bool * (bool * (nat * unit))))%type : Set *)


(* Now, I can write the denotational semantics for instruction and program *)
Definition tinstrDenote {ts ts': tstack} (i: tinstr ts ts'): tstackDenote ts -> tstackDenote ts' :=
  match i with
  | TiNConst _ n => fun s => (n, s)
  | TiBConst _ b => fun s => (b, s)
  | TiBinop _ _ _ _ op =>
    fun s => let '(arg1, (arg2, s')) := s in
             ((tbinopDenote op) arg1 arg2, s')
  end.

Fixpoint tprogDenote {ts ts': tstack} (p: tprog ts ts'): tstackDenote ts -> tstackDenote ts' :=
  match p with
  | TNil _ => fun s => s
  | TCons _ _ _ i p' =>
    fun s =>
      let s' := tinstrDenote i s in
      let s'' := (tprogDenote p') s' in
      s''
  end.

Eval simpl in tinstrDenote tiFortytwo tt.  (* (42, tt) : (nat * unit) *)
Eval simpl in tinstrDenote tiTrue tt.  (* (true, tt) : (bool * unit) *)
Eval simpl in tprogDenote tiProg0 tt.  (* (2, (2, tt)) : (nat * nat * unit) *)
Eval simpl in tprogDenote tiProg0' tt.  (* (4, tt) : (nat * unit) *)
Eval simpl in tprogDenote tiProg1 tt.  (* (28, tt) : (nat * unit) *)
Eval simpl in tprogDenote tiProg2 tt.  (* (false, tt) : (bool * unit) *)
Eval simpl in tprogDenote tiProg3 tt.  (* (true, tt) : (bool * unit) *)
Eval simpl in tprogDenote tiProg4 tt.  (* (4, (28, tt)) : (nat * nat * unit) *)

(* ------------------------------------------------------------------- *)
(* Compiler *)

Fixpoint tcompile (t: type) (e: texp t) (s: tstack) : tprog s (t :: s) :=
  match e with
  | TNConst n => TCons (TiNConst s n) (TNil (NAT :: s))
  | TBConst b => TCons (TiBConst s b) (TNil (BOOL :: s))
  | TBinop _ _ _ op exp1 exp2 =>
    let prog2 := tcompile exp2 s in
    let prog1 := tcompile exp1 _ in
    let progOp := TCons (TiBinop _ op) (TNil _) in
    tconcat prog2 (tconcat prog1 progOp)
  end.

(* See at exactly which values are filled in place of `_` *)
Print tcompile.
(*
  match e in (texp t0) return (tprog s (t0 :: s)) with
  | ...
  | @TBinop t1 t2 t0 op exp1 exp2 =>
      let prog2 := tcompile t2 exp2 s in
      let prog1 := tcompile t1 exp1 (t2 :: s) in
      let progOp := TCons (TiBinop s op) (TNil (t0 :: s)) in
      tconcat prog2 (tconcat prog1 progOp)
  end
*)

Eval simpl in tprogDenote ((tcompile tFortytwo) nil) tt.  (* (42, tt) *)
Eval simpl in tprogDenote ((tcompile tTrue) nil) tt.      (* (true, tt) *)
Eval simpl in tprogDenote ((tcompile tExp1) nil) tt.      (* (28, tt) *)
Eval simpl in tprogDenote ((tcompile tExp2) nil) tt.      (* (false, tt) *)
Eval simpl in tprogDenote ((tcompile tExp3) nil) tt.      (* (true, tt) *)


(* Let's turn to prove the compiler /always/ works, which should be
stated at -- based on the previous `Eval` -- "A compiled expression
always results in a pair `(res, tt)` with `res` the result of the
expression evaluation." *)

(* ------------------------------------------------------------------- *)
(* Compiler Correctness *)

Theorem tcompile_correct: forall (t: type) (e: texp t),
    tprogDenote ((tcompile e) nil) tt = (texpDenote e, tt).
Proof.
  crush.
Abort.

(* Strengthened the induction hypothesis: quantify over type `t`,
   expression `e`, the initial stack type `s` and a stack compatible
   with it `vs`. *)
Lemma tcompile_correct': forall (t: type) (e: texp t) (s: tstack) (vs: tstackDenote s),
  tprogDenote ((tcompile e) s) vs = (texpDenote e, vs).
Proof.
  induction e; crush.
  (* We need a similar trick as in `app_assoc_reverse` *)
Abort.

(* Impl `app_assoc_reverse` for `tconcat` *)
Lemma tconcat_correct: forall s1 s2 s3 (p: tprog s1 s2) (p': tprog s2 s3) (vs: tstackDenote s1),
    tprogDenote (tconcat p p') vs = tprogDenote p' (tprogDenote p vs).
Proof.
  induction p; crush.
Qed.
Hint Rewrite tconcat_correct. (* So `crush` tac can use it. *)

Lemma tcompile_correct': forall (t: type) (e: texp t) (s: tstack) (vs: tstackDenote s),
  tprogDenote ((tcompile e) s) vs = (texpDenote e, vs).
Proof.
  induction e; crush.
Qed.
Hint Rewrite tcompile_correct'. (* So `crush` tac can use it. *)

(* Let's prove it! *)
Theorem tcompile_correct: forall (t: type) (e: texp t),
    tprogDenote ((tcompile e) nil) tt = (texpDenote e, tt).
  crush.
Qed.


(* This compiler is a functional program that can be executed. It is
called /program extraction/, which generates OCaml code from Coq. *)

(* ------------------------------------------------------------------- *)
(* Extraction *)
Require Extraction.

(* Generate the OCaml code for `tcompile` *)
Extraction tcompile.

(*
let rec tcompile _ e s =
  match e with
  | TNConst n -> TCons (s, (Cons (NAT, s)), (Cons (NAT, s)), (TiNConst (s, n)), (TNil (Cons (NAT, s))))
  | TBConst b ->
    TCons (s, (Cons (BOOL, s)), (Cons (BOOL, s)), (TiBConst (s, b)), (TNil (Cons (BOOL, s))))
  | TBinop (t1, t2, t, op, exp1, exp2) ->
    let prog2 = tcompile t2 exp2 s in
    let prog1 = tcompile t1 exp1 (Cons (t2, s)) in
    let progOp = TCons ((Cons (t1, (Cons (t2, s)))), (Cons (t, s)), (Cons (t, s)), (TiBinop (s, t1, t2,
      t, op)), (TNil (Cons (t, s))))
    in
    tconcat s (Cons (t2, s)) (Cons (t, s)) prog2
      (tconcat (Cons (t2, s)) (Cons (t1, (Cons (t2, s)))) (Cons (t, s)) prog1 progOp)

We can compile this code with the usual OCaml compiler and obtain an
executable program.
*)
