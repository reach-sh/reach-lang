'reach 0.1';

export const main = Reach.App(() => {
    const A = Participant('Alice', {});
    const E = Events({ sync: [] });
    init();
    A.publish();
    E.sync();
    commit();
});
