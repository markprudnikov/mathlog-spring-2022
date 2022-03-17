(* tutorial https://coq.inria.fr/tutorial/1-basic-predicate-calculus *)

(* Write your proof terms in place of underscores below ("_") *)

(* Exercise 1: Prove implication is transitive. What does this logical lemma correspond to in functional programming? *)

Variables A B C : Prop.

Theorem impl_trans: (A -> B) -> (B -> C) -> (A -> C).
Proof.
intros.
exact (H0 (H H1)).
Qed.

Check
  (impl_trans) 
: (A -> B) -> (B -> C) -> (A -> C).


(* Exercise 2: Prove conjunction is associative *)
Theorem conj_ass: (A /\ B) /\ C -> A /\ (B /\ C).
Proof.
  intros.
  destruct H as (H & HC).
  destruct H as (HA & HB).
  split.
  assumption.
  split. 
  assumption.
  assumption.
Qed.

Check
  (conj_ass)
: (A /\ B) /\ C -> A /\ (B /\ C).


(* Exercise 3: Prove disjunction distributes over conjunction: *)
Theorem disj_over_conj: A \/ (B /\ C) -> (A \/ B) /\ (A \/ C).
Proof.
  split.
  - destruct H as [A | [B C]].
    + left. exact A.
    + right. exact B.
  - destruct H as [A | [B C]].
    + left. exact A.
    + right. exact C.
Qed.
Check
  (disj_over_conj)
: A \/ (B /\ C) -> (A \/ B) /\ (A \/ C).

(* Exercise 4: Prove weak form of Peirce's law holds in intuitionistic logic *)

Theorem weak_peirce: ((((A -> B) -> A) -> A) -> B) -> B.
Proof.
  intros.
  apply H. 
  intros.
  apply H0.
  intros.
  apply H.
  intros.
  exact H1.
Qed.

Check
  (weak_peirce)
: ((((A -> B) -> A) -> A) -> B) -> B.

(* Exercise 5: We can always add double negation (but cannot drop it in general) *)
Theorem add_double_negation: A -> ~~A.
Proof.
  intros.
  unfold not.
  intros.
  exact (H0 H).
Qed.

Check
  (add_double_negation)
: A -> ~ ~ A.

(* Exercise 6: Although we can in some special cases like the following: *)
Theorem strange_negation: ~~~A -> ~A.
Proof.
  intros.
  unfold not.
  intros.
  exact (H (add_double_negation H0)).
Qed.

Check
  (strange_negation)
: ~ ~ ~ A -> ~ A.

(* Exercise 7: Prove we cannot add the negation of the law of excluded middle and have a sound logic.
   Keep in mind that "~ A" means "A -> False" *)
Check
  (_)
: ~ ~ (A \/ ~ A).