Require Import Bool Arith_base List.

Import ListNotations.

Print nat.

Fixpoint myadd (n m : nat) : nat :=
  match n with
  | O => m
  | S n' => S (myadd n' m)
  end.

Lemma myadd_zero_left
  : forall x, myadd 0 x = x.
Proof.
  intros x.
  simpl.
  reflexivity.
Qed.

Lemma andb_false_left
  : forall x, false && x = false.
Proof.
  intros x.
  simpl.
  reflexivity.
Qed.

Lemma andb_false_right
  : forall x, x && false = false.
Proof.
  intros x.
  destruct x.
  -
    simpl.
    reflexivity.
  -
    simpl.
    reflexivity.
Qed.


Lemma myadd_zero_right
  : forall x, x + 0 = x.
Proof.
  intros x.
  induction x.
  -
    simpl.
    reflexivity.
  -
    simpl.
    rewrite IHx.
    reflexivity.
Qed.

Lemma myadd_succ
  : forall m n, S (m + n) = m + S n.
Proof.
  induction m ;
    intros n ; simpl ;
       try solve [reflexivity].
  -
    rewrite IHm.
    reflexivity.
Qed.

Lemma myadd_comm
  : forall n m, n + m = m + n.
Proof.
  induction n ; intros m ; simpl.
  -
    rewrite myadd_zero_right.
    reflexivity.
  -
    rewrite IHn.
    apply myadd_succ.
Qed.

Fixpoint even (n : nat) : bool :=
 match n with
 | 0 => true
 | 1 => false
 | S (S  n') => even n'
 end.

Search (_ * 0 = 0).

Lemma times2_even
  : forall n, even (2 * n) = true.
Proof.
  induction n.
  -
    rewrite Nat.mul_0_r ; simpl.
    reflexivity.
  -
    rewrite Nat.mul_comm.
    simpl.
    rewrite Nat.mul_comm.
    assumption.
Qed.

Fixpoint repeat {A : Type}
                (n : nat)
                (x : A) : list A :=
  match n with
  | 0 => []
  | S n' => x :: repeat n' x
  end.

Lemma repeat_length {A : Type}
  : forall n (x : A),
    length (repeat n x) = n.
Proof.
  induction n as [| n' IHn']
    ; intros x ; simpl ;
      try solve [reflexivity].
  -
    rewrite IHn'.
    reflexivity.
Qed.

Lemma app_length {A : Type}
  : forall (xs ys : list A),
    length (xs ++ ys) =
      length xs + length ys.
Proof.
  induction xs as [| x' xs' IHxs'] ;
    intros ys ;
    simpl ;
    try solve [reflexivity].
  -
    rewrite IHxs'.
    reflexivity.
Qed.

Inductive Even : nat -> Prop :=
| ev_zero : Even 0
| ev_suc : forall n, Even n -> Even (2 + n).

Definition Ev (n : nat) : Prop := exists m, n = 2 * m.

Lemma Ev_zero : Ev 0.
Proof.
  exists 0 ; reflexivity.
Qed.

Lemma Ev_one : ~ Ev 1.
Proof.
  intro H ; destruct H as [[|] H] ; try discriminate.
  simpl in *.
  rewrite <- plus_n_Sm in H.
  inversion H.
Qed.

Lemma Ev_succ : forall n, Ev n <-> Ev (S (S n)).
Proof.
  intros n ; split ; intros [m H].
  -
    exists (1 + m) ; simpl.
    rewrite H. simpl.
    rewrite Nat.add_0_r in *.
    rewrite plus_n_Sm in *.
    reflexivity.
  -
    destruct m as [| m'] ; simpl in *.
    +
      discriminate.
    +
      rewrite <- plus_n_Sm in H.
      rewrite Nat.add_0_r in H.
      inversion H.
      exists m'. simpl.
      rewrite Nat.add_0_r.
      reflexivity.
Qed.

Lemma times2_Even : forall n, Even (2 * n).
Proof.
  induction n ; simpl.
  - constructor.
  -
    rewrite Nat.add_0_r.
    rewrite plus_n_Sm.
    rewrite Nat.add_comm.
    simpl.
    repeat constructor.
    simpl in *.
    rewrite Nat.add_0_r in *.
    assumption.
Qed.

Lemma Ev_Even : forall n, Even n <-> Ev n.
Proof.
  intros n ; split ; intros H.
  -
    induction H.
    + exists 0 ; reflexivity.
    +
      destruct IHEven as [m Hm].
      rewrite Hm.
      simpl.
      rewrite Nat.add_0_r.
      exists (1 + m) ; simpl.
      rewrite Nat.add_0_r.
      rewrite plus_n_Sm.
      reflexivity.
  -
    destruct H as [m Hm].
    rewrite Hm.
    apply times2_Even.
Qed. 

Lemma even_spec : forall n, even n = true <-> Ev n.
Proof.
  fix even_spec 1.
  intro n ; destruct n ; simpl.
  -
    split ; trivial.
    intro H ; exists 0 ; auto.
  -
    split.
    destruct n. intros ; discriminate.
    intros.
    rewrite Ev_succ.
    admit.
    destruct n.
    intros H. admit.
    intros H.
    rewrite even_spec.
