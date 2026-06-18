/* -Woverflow: floating-point literals that can't be represented in `double`. Unlike integer
   constants (which error when they don't fit even a 64-bit type), an out-of-range floating
   literal is never an error — NCC rounds it like every real implementation and warns instead.
   The program still compiles and runs cleanly to 0.

   gcc rolls both directions into -Woverflow:
     1e400  -> "floating constant exceeds range of 'double'"  (rounds to +infinity)
     1e-400 -> "floating constant truncated to zero"          (underflows to 0.0) */
double huge = 1e400;    /* overflow  -> +infinity */
double tiny = 1e-400;   /* underflow -> 0.0       */

int main(void) { return 0; }
