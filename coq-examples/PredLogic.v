Variable U : Set.
Variable u : U.
Variables P Q R : U -> Prop.

(** exemplo: Sócrates *)

Variable Socrates Joao: U.
Variable homem mortal : U -> Prop.

Theorem example1
  (H1 : forall x, homem x -> mortal x)
  (H2 : homem Socrates) : mortal Socrates.
Proof.
  assert (H3 : homem Socrates -> mortal Socrates).
  -
    apply H1.
  -
    apply H3.
    assumption.
Qed.

Theorem example2
  (H1 : forall x, P x -> Q x)
  (H2 : forall y, Q y -> R y)
  : forall z, P z -> R z.
Proof.
  intros z HPz.
  apply H2.
  apply H1.
  assumption.
Qed.

Theorem example3
 : (forall x, P x /\ Q x) <-> (forall y, P y) /\ (forall z, Q z).
Proof.
  split.
  -
    intros H.
    split.
    +
      intros y.
      assert (H1 : P y /\ Q y) by apply H.
      destruct H1 as [HPy HQy].
      assumption.
    +
      intros z.
      assert (H1 : P z /\ Q z) by apply H.
      destruct H1 as [HPz HQz].
      assumption.
  -
    intros [H1 H2] x ;
      split ;
        [apply H1 | apply H2].
Qed.

Theorem example4
 : (exists x, (P x \/ Q x)) <-> (exists y, P y) \/ (exists z, Q z).
Proof.
  split.
  -
    intros H.
    destruct H as [x0 Hx0].
    destruct Hx0 as [HL | HR].
    +
      left.
      exists x0.
      assumption.
    +
      right ; exists x0 ; assumption.
  -
    intros [[y0 Hy0] | [z0 Hz0]].
    +
      exists y0 ; left ; assumption.
    +
      exists z0 ; right ; assumption.
Qed.

Lemma ex10
  : (forall x : U, P x -> ~ Q x) ->
    ~ exists y : U, P y /\ Q y.
Proof.
  intros H [y [H1 H2]].
  assert (H3 : P y -> ~ Q y) by apply H.
  assert (H4 : ~ Q y) by (apply H3 ; assumption).
  contradiction.
Qed.
