From Coq Require Import List Arith.
Import ListNotations.

(* Arithmetic expressions language *)
Inductive expr : Type :=
| Const : nat -> expr
| Plus : expr -> expr -> expr
| Minus : expr -> expr -> expr.

(* Semantics of arithmetic expressions *)
Fixpoint eval (e : expr) : nat :=
  match e with
  | Const n => n
  | Plus e1 e2 => eval e1 + eval e2
  | Minus e1 e2 => eval e1 - eval e2
  end.

(* Stack machine instructions *)
Inductive instr :=
| Push : nat -> instr
| Add
| Sub.

(* Stack programs *)
Definition prog := list instr.

(* Stack *)
Definition stack := list nat.

(* Stack machine semantics *)
Fixpoint run (p : prog) (s : stack) {struct p}: stack :=
  match p with
  | [] => s
  | i :: p' =>
    match i with
    | Push n => run p' (n :: s)
    | Add =>
      match s with
      | a :: b :: s' => run p' (b + a :: s')
      | _ => [] (* if stack underflow -- interrupt
                   execution and return empty stack *)
      end
    | Sub =>
      match s with
      | a :: b :: s' => run p' (b - a :: s')
      | _ => [] (* if stack underflow -- interrupt
                   execution and return empty stack *)
      end
    end
  end.

(* Compilation from arithmetic expressions
   into stack programs *)
Fixpoint compile (e : expr) : prog :=
  match e with
  | Const n => [Push n]
  | Plus e1 e2 =>
    compile e1 ++ compile e2 ++ [Add]
  | Minus e1 e2 =>
    compile e1 ++ compile e2 ++ [Sub]
  end.

Lemma compile_program :
  forall (e : expr) (p : prog) (s : stack),
    run p (eval e :: s) = run (compile e ++ p) s.
Proof.
    induction e as [ | e1 IHe1 e2 IHe2 | e1 IHe1 e2 IHe2 ]; simpl; intros p s.
    - reflexivity.
    - rewrite app_assoc_reverse.
      rewrite <- IHe1.
      rewrite app_assoc_reverse.
      rewrite <- IHe2.
      simpl.
      reflexivity.
    - rewrite app_assoc_reverse.
      rewrite <- IHe1.
      rewrite app_assoc_reverse.
      rewrite <- IHe2.
      simpl.
      reflexivity.
Qed.

Theorem compile_correct_generalized :
  forall e,
    [eval e] = run (compile e) [].
Proof.
    intros e.
    destruct e; simpl.
    - reflexivity.
    - rewrite <- !compile_program. simpl. reflexivity.
    - rewrite <- !compile_program. simpl. reflexivity.
Qed.
