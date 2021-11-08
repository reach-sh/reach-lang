'reach 0.1';

export const main = Reach.App(() => {
  const Sigma = Participant('Sigma', {});
  const Betas = ParticipantClass('Betas', {});
  deploy();

  Betas.publish();
  commit();
  exit();
});
