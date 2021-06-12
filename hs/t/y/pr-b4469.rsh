'reach 0.1';

export const main = Reach.App(() => {
  const Admin = Participant('Administrator', {});
  deploy();

  Admin.publish();
  commit();

  fork()
    .case(Admin,
      (() => ({ msg: 0 })),
      ((_) => 0),
      ((x) => {
        commit();
        exit();
      }))
    .case(Admin,
      (() => ({ msg: 1 })),
      ((_) => 0),
      ((y) => {
        commit();
        exit();
      }));
});
