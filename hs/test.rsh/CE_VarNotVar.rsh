'reach 0.1 exe';

const A = newParticipant();
const B = newParticipant();

function main() {
  A.only(() => {
    const y = 2; });
  B.publish(y);
  return 0; }
