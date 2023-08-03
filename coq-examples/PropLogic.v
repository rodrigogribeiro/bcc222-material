Variables A B C : Prop.

Lemma and_comm (H1 : A /\ B) : B /\ A.
Proof.
  split.
  -
    destruct H1 as [HA HB].
    assumption.
  -
    destruct H1 as [HA HB].
    assumption.
Qed.

Lemma impl_assoc (H1 : A -> B)
                 (H2 : B -> C) : A -> C.
Proof.
  intros HA.
  apply H2.
  apply H1.
  assumption.
Qed.

Lemma impl_and (H : A /\ B -> C)
    : A -> B -> C.
Proof.
  intros HA HB.
  apply H.
  split ; assumption.
Qed.

Lemma or_comm (H : A \/ B) : B \/ A.
Proof.
  destruct H as [HA | HB].
  -
    right ; assumption.
  -
    left ; assumption.
Qed.

Lemma modus_tollens
        (H1 : A -> B)
        (H2 : ~ B) : ~ A.
Proof.
  intros HA.
  unfold not in H2.
  apply H2.
  apply H1.
  assumption.
Qed.


