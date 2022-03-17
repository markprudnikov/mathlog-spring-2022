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
Check
  (_)
: A \/ (B /\ C) -> (A \/ B) /\ (A \/ C).

(* Exercise 4: Prove weak form of Peirce's law holds in intuitionistic logic *)
Check
  (_)
: ((((A -> B) -> A) -> A) -> B) -> B.

(* Exercise 5: We can always add double negation (but cannot drop it in general) *)
Check
  (_)
: A -> ~ ~ A.

(* Exercise 6: Although we can in some special cases like the following: *)
Check
  (_)
: ~ ~ ~ A -> ~ A

(* Exercise 7: Prove we cannot add the negation of the law of excluded middle and have a sound logic.
   Keep in mind that "~ A" means "A -> False" *)
Check
  (_)
: ~ ~ (A \/ ~ A).