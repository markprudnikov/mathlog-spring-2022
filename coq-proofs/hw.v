(* Write your proof terms in place of underscores below ("_") *)

Variables A B C : Prop.

(* Exercise 1: Prove implication is transitive. What does this logical lemma correspond to in functional programming? *)
Check
  (fun f g => 
    fun (x1 : A) => (g (f x1))
  )
: (A -> B) -> (B -> C) -> (A -> C).

(* Exercise 2: Prove conjunction is associative *)
Check
  (fun abc => conj (proj1 (proj1 abc)) (conj (proj2 (proj1 abc)) (proj2 abc)))
: (A /\ B) /\ C -> A /\ (B /\ C).

(* Exercise 3: Prove disjunction distributes over conjunction: *)
Check
  (fun abc => 
    conj 
    (match abc with
    | or_introl a => or_introl a
    | or_intror bc => or_intror (proj1 bc)
    end)
    (match abc with
    | or_introl a => or_introl a
    | or_intror bc => or_intror (proj2 bc)
    end)
  )
: A \/ (B /\ C) -> (A \/ B) /\ (A \/ C).

(* Exercise 4: Prove weak form of Peirce's law holds in intuitionistic logic *)
Check
  (fun abaab => abaab (fun aba => aba (fun a => abaab (fun _ => a))))
: ((((A -> B) -> A) -> A) -> B) -> B.

(* Exercise 5: We can always add double negation (but cannot drop it in general) *)

Check
  (fun a => (fun na => (na a)))
: A -> ~ ~ A.

(* Exercise 6: Although we can in some special cases like the following: *)
Check
  (fun nnna => (fun a => nnna ((fun na => (na a)))))
: ~ ~ ~ A -> ~ A.

(* Exercise 7: Prove we cannot add the negation of the law of excluded middle and have a sound logic.
   Keep in mind that "~ A" means "A -> False" *)
Check
  (
      fun ns => ns (or_intror (fun s : A => ns (or_introl s)) )
  )
: ~ ~ (A \/ ~ A).

(* ns: (A \/ ~A) -> False  *)
(* s : A *)
(* or_introl s : A \/ ~A *)
