'reach 0.1';

const Actor =
      { ...hasRandom,
        leave: Fun([], Bool) };
const Landlord =
      { ...Actor,
        terms: UInt };
const Tenant =
      { ...Actor,
        acceptTerms: Fun([UInt], Null) };

const DEADLINE = 10;
export const l_first = Reach.App(
  {}, [Participant('Tenant', Tenant), Participant('Landlord', Landlord)], (T, L) => {

    L.only(() => {
      const terms = declassify(interact.terms); });
    L.publish(terms)
      .pay(terms);
    commit();

    T.only(() => {
      interact.acceptTerms(terms); });
    T.pay(terms)
      .timeout(DEADLINE, () => closeTo(L, () => null));
    commit();

    L.only(() => {
      const leaveL = declassify(interact.leave()); });
    L.publish(leaveL)
      .timeout(DEADLINE, () => closeTo(T, () => null));

    if ( leaveL ) {
      transfer(2 * terms).to(T);
      commit();
      exit();
    } else {
      commit();

      T.only(() => {
        const leaveT = declassify(interact.leave()); });
      T.publish(leaveT)
        .timeout(DEADLINE, () => closeTo(L, () => null));

      if ( leaveT ) {
        transfer(2 * terms).to(L);
        commit();
        exit();
      } else {
        transfer(terms).to(L);
        transfer(terms).to(T);
        commit();
        exit();
      }
    }
  } );

export const t_first = Reach.App(
  {}, [Participant('Tenant', Tenant), Participant('Landlord', Landlord)], (T, L) => {

    L.only(() => {
      const terms = declassify(interact.terms); });
    L.publish(terms)
      .pay(terms);
    commit();

    T.only(() => {
      interact.acceptTerms(terms); });
    T.pay(terms)
      .timeout(DEADLINE, () => closeTo(L, () => null));
    commit();

    T.only(() => {
      const leaveT = declassify(interact.leave()); });
    T.publish(leaveT)
      .timeout(DEADLINE, () => closeTo(L, () => null));

    if ( leaveT ) {
      transfer(2 * terms).to(L);
      commit();
      exit();
    } else {
      commit();

      L.only(() => {
        const leaveL = declassify(interact.leave()); });
      L.publish(leaveL)
        .timeout(DEADLINE, () => closeTo(T, () => null));

      if ( leaveL ) {
        transfer(2 * terms).to(T);
        commit();
        exit();
      } else {
        transfer(terms).to(L);
        transfer(terms).to(T);
        commit();
        exit();
      }
    }
  } );

export const fair = Reach.App(
  {}, [Participant('Tenant', Tenant), Participant('Landlord', Landlord)], (T, L) => {

    L.only(() => {
      const terms = declassify(interact.terms); });
    L.publish(terms)
      .pay(terms);
    commit();

    T.only(() => {
      interact.acceptTerms(terms); });
    T.pay(terms)
      .timeout(DEADLINE, () => closeTo(L, () => null));

    var [ gleaveT, gleaveL ] = [ false, false ];
    invariant(balance() == 2 * terms);
    while ( ! (gleaveT || gleaveL) ) {
      commit();

      T.only(() => {
        const _leaveT = interact.leave();
        const [_commitT, _saltT] = makeCommitment(interact, _leaveT);
        const commitT = declassify(_commitT); });
      T.publish(commitT)
        .timeout(DEADLINE, () => closeTo(L, () => null));
      commit();

      unknowable(L, T(_leaveT, _saltT));
      L.only(() => {
        const leaveL = declassify(interact.leave()); });
      L.publish(leaveL)
        .timeout(DEADLINE, () => closeTo(T, () => null));
      commit();

      T.only(() => {
        const [saltT, leaveT] = declassify([_saltT, _leaveT]); });
      T.publish(saltT, leaveT);
      checkCommitment(commitT, saltT, leaveT);

      [ gleaveT, gleaveL ] = [ leaveT, leaveL ];
      continue; }

    const [ toL, toT ] =
          ( gleaveT && gleaveT ) ? [ 1, 1 ] :
          ( gleaveT            ) ? [ 2, 0 ] :
          [ 0, 2 ];
    transfer(toL * terms).to(L);
    transfer(toT * terms).to(T);
    commit();
    exit();

  } );
