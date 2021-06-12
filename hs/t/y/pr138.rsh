'reach 0.1'
'use strict'

const IAdministrator = {
    getNewEntry: Fun([],Address),
}

export const main = Reach.App({
    deployMode: "constructor" 
    },
    [ Participant('Administrator', IAdministrator) ],
     (Admin) => {
        Admin.publish();
        
        const registry = new Set()

        const isRunning = parallelReduce(true)
            .invariant(balance () == 0)  // No funds movement, should validate always
            .while(isRunning)
            .case(
                Admin,
                /* publish_expr */
                (() => ({
                    msg: declassify(interact.getNewEntry())
                })),
                /* pay_expr */
                ((_) => 0),
                /* consensus_expr */
                ((addr) => {
                    registry.insert(addr);
                    return isRunning;
                })
            )

        commit();
    });
