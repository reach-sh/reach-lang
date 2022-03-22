'reach 0.1';
'use strict';

export const main = Reach.App(() => {
    const D = Participant('Admin', {
        price: UInt,
        deadline: UInt,
        ready: Fun([], Null),
    });
    const A = API('Attendee', {
        iWillGo: Fun([], Bool),
    });
    const C = API('Checkin', {
        theyCame: Fun([Address], Bool),
        timesUp: Fun([], Bool),
    });
    init();

    D.only(() => {
        const price = declassify(interact.price);
        const deadline = declassify(interact.deadline);
        check(price < 100000000);
    });
    D.publish(price, deadline);
    D.interact.ready();
    check(price < 100000000);

    const RSVPs = new Map(Object({
        came: Bool,
    }));

    const [ keepGoing, howMany ] =
        parallelReduce([true, 0])
        .define(() => {

        })
        .invariant(
            true
            && balance() == howMany * price
            && RSVPs.size() == howMany
        )
        .while( keepGoing )
        .api(A.iWillGo,
            () => {
                check(isNone(RSVPs[this]), "they haven't rsvpd");
            },
            () => price,
            (k) => {
                check(isNone(RSVPs[this]), "they haven't rsvpd");
                RSVPs[this] = { came: false };
                k(true);
                return [ keepGoing, howMany + 1 ];
            }
        )
        .api(C.theyCame,
            (who) => {
                check(isSome(RSVPs[who]), "they rsvpd");
                check(this == D, "you are the boss");
            },
            (_) => 0,
            (who, k) => {
                check(isSome(RSVPs[who]), "they rsvpd");
                check(this == D, "you are the boss");
                transfer(price).to(who);
                delete RSVPs[who];
                k(true);
                return [ keepGoing, howMany - 1 ];
            }
        )
        .timeout( absoluteTime(deadline), () => {
            const [ [], k ] = call(C.timesUp);
            k(true);
            return [ false, howMany ]
        });
    const leftovers = howMany;

    transfer(leftovers * price).to(D);
    commit();

    exit();
});