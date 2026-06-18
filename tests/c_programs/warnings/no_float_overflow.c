/* In-range floating literals must NOT trigger -Woverflow. This guards both edges of the check:
   values at the extremes of `double` round exactly to themselves, and a literal that legitimately
   equals zero must stay quiet — the underflow warning only fires when the lexeme has a nonzero
   digit, so a true zero (`0.0`, `0e10`) is not mistaken for an underflow. Runs cleanly to 0. */
double max = 1.7976931348623157e308;     /* ~DBL_MAX             — fits */
double min_normal = 2.2250738585072014e-308; /* smallest normal double — fits */
double zero = 0.0;                        /* genuine zero — must not warn */
double zero_exp = 0e10;                   /* genuine zero with exponent — must not warn */

int main(void) { return 0; }
