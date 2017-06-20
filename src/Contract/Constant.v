Require Import FreeSpec.Interp.
Require Import FreeSpec.Contract.

Definition const_contract_requirements
           {I: Interface}
           (requirements: forall (A: Type), I A -> Prop)
  := fun {A: Type} (i: I A) (_: unit) => requirements A i.

Definition const_contract_promises
           {I: Interface}
           (promises: forall (A: Type)
                             (i: I A),
               typeret i -> Prop)
  := fun {A: Type} (i: I A) (a: A) (_: unit) => promises A i a.

Definition constant_contract
           {I: Interface}
           (requirements: forall (A: Type), I A -> Prop)
           (promises: forall (A: Type)
                             (i: I A),
               typeret i -> Prop)
  : Contract unit I :=
  {| abstract := tt
   ; abstract_step := fun (A: Type) (_: I A) (x: unit) => x
   ; requirements := @const_contract_requirements I requirements
   ; promises := @const_contract_promises I promises
   |}.

Definition requirements_preserves_inv
           {I: Interface}
           {State: Type}
           (requirements: forall (A: Type), I A -> Prop)
           (step: @PS I State)
           (inv: State -> Prop)
  := forall {A: Type}
            (i: I A)
            (s: State),
    inv s
    -> requirements A i
    -> inv (snd (step A s i)).

Lemma const_contract_preserves_inv
      {I: Interface}
      {State: Type}
      (requirements: forall (A: Type), I A -> Prop)
      (step: @PS I State)
      (inv: State -> Prop)
  : requirements_preserves_inv requirements step inv
    -> contract_preserves_inv (@const_contract_requirements I requirements)
                              (fun _ _ _ => tt)
                              (fun _ s => inv s)
                              step.
Proof.
  unfold requirements_preserves_inv, contract_preserves_inv.
  intros Hreq.
  intros B i _s s Hinv Hconst.
  apply (Hreq B i s Hinv Hconst).
Qed.

Definition requirements_brings_promises
           {I: Interface}
           {State: Type}
           (requirements: forall (A: Type), I A -> Prop)
           (promises: forall (A: Type), I A -> A -> Prop)
           (step: @PS I State)
           (inv: State -> Prop)
  := forall {A: Type}
            (i: I A)
            (s: State),
    inv s
    -> requirements A i
    -> promises A i (fst (step A s i)).

Lemma const_contract_enforces_promises
      {I: Interface}
      {State: Type}
      (requirements: forall (A: Type), I A -> Prop)
      (promises: forall (A: Type), I A -> A -> Prop)
      (step: @PS I State)
      (inv: State -> Prop)
  : requirements_brings_promises requirements promises step inv
    -> contract_enforces_promises (@const_contract_requirements I requirements)
                                  (@const_contract_promises I promises)
                                  (fun _ _ _ => tt)
                                  (fun _ s => inv s)
                                  step.
Proof.
  unfold requirements_brings_promises, contract_enforces_promises.
  intros Hreq.
  intros B i s _s Hinv Hconst.
  apply (Hreq B i s Hinv Hconst).
Qed.

Lemma const_contract_enforcement
      {I: Interface}
      {State: Type}
      (requirements: forall (A: Type), I A -> Prop)
      (promises: forall (A: Type), I A -> A -> Prop)
      (step: @PS I State)
      (inv: State -> Prop)
      (Hpres: requirements_preserves_inv requirements step inv)
      (Henf: requirements_brings_promises requirements promises step inv)
  : forall (s: State),
    inv s
    -> Enforcer (mkInterp step s) (constant_contract requirements promises).
Proof.
  intros s Hinv.
  apply (stateful_contract_enforcement (constant_contract requirements promises)
                                       (fun _ s => inv s)
                                       step).
  + apply const_contract_preserves_inv.
    exact Hpres.
  + apply const_contract_enforces_promises.
    exact Henf.
  + exact Hinv.
Qed.