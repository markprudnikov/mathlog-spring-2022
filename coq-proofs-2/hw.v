(* Write your proof terms in place of `provide_solution` *)
(* Do not use tactics! *)

Axiom provide_solution : forall {A}, A.

Section Logic.

(** Exercise: show that existential quantifier (the `ex` type) is a more general case of conjunction (the `and` type).
    This is because terms of type `ex` are dependnt pairs, while terms of type `and`
    are non-dependent pairs, i.e. the type of the second component in independent of the
    value of the first one. *)

Definition and_via_ex (A B : Prop) :
  (exists (_ : A), B) <-> A /\ B
:= conj (fun e => conj (ex_proj1 e) (ex_proj2 e)) (fun ab : A /\ B => ex_intro _ (proj1 ab) (proj2 ab)).

(** Exercise: The dual Frobenius rule *)

Definition Frobenius2 :=
  forall (A : Type) (P : A -> Prop) (Q : Prop),
    (forall x, Q \/ P x) <-> (Q \/ forall x, P x).

Definition LEM_iff_Frobenius2 :
  (forall P : Prop, P \/ ~ P) <-> Frobenius2
:= provide_solution.

End Logic.

Section ExtensionalEqualityAndComposition.

Variables A B C D : Type.

Notation "f \o g" := (fun x => f (g x)) (at level 50).
Notation "f =1 g" := (forall x, f x = g x) (at level 70, no associativity).

(** [=1] stands for extensional equality on unary functions *)

(** Exercise : associativity of function composition *)
Definition compA (f : A -> B) (g : B -> C) (h : C -> D) :
  (h \o g) \o f = h \o (g \o f) 
:= eq_refl (fun x => h (g (f x))).

(** Exercise: Reflexivity *)
Definition eqext_refl :
  forall (f : A -> B), f =1 f
:= fun f x => (eq_refl (f x)).

(** Exercise: Symmetry *)
Definition eqext_sym :
  forall (f g : A -> B), f =1 g -> g =1 f (* (forall x, f x = g x) *)
  := fun f g fg a => eq_sym (fg a).
    
(** Exercise: Transitivity *)
Definition eqext_trans :
  forall (f g h : A -> B), f =1 g -> g =1 h -> f =1 h
:= fun f g h fexg gexh x => eq_trans (fexg x) (gexh x).

(** Exercise: left congruence *)
Definition eq_compl :
  forall (f g : A -> B) (h : B -> C),
    f =1 g -> h \o f =1 h \o g
:= provide_solution.

(** Exercise: right congruence *)
Definition eq_compr :
  forall (f g : B -> C) (h : A -> B),
    f =1 g -> f \o h =1 g \o h
:= provide_solution.

End ExtensionalEqualityAndComposition.