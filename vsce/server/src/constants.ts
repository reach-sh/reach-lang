// This should stay synchronized with client/src/constants.ts

// e.g., sendNotification(For.commencedCompilation);
export enum For {
  commencedCompilation = 'comp_commenced',
  completedCompilation = 'comp_completed',
  errorDuringCompilation = 'error_during_compilation',
  compilationCouldntCommence = 'comp_couldnt_commence',
};
