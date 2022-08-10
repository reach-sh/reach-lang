'reach 0.1';

// This is parsed as a `const x = 1.1E-6` stmt followed by `.1` so there's an unintuitive error
const x = 1.1E-6.1;
