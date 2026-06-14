/* -Woverflow must NOT fire on unsigned arithmetic: unsigned wraparound is well-defined
   (modular, C 6.2.5p9), unlike signed overflow which is UB. This matches gcc/clang, which
   stay silent here. Folded at compile time in static initializers; runs cleanly to 0.

   Each line wraps under two's complement but is defined for unsigned, so the overflowing_*
   flag from constant folding must be suppressed when the operand type is unsigned. The
   companion file overflow.c covers the signed cases that *should* warn. */
unsigned int  a = 4000000000u + 1000000000u;     /* add:  5e9 wraps mod 2^32 */
unsigned int  b = 3000000000u * 3u;              /* mul:  9e9 wraps mod 2^32 */
unsigned int  c = -1u;                           /* neg:  -1u -> UINT_MAX */
unsigned int  d = 0u - 1u;                       /* sub:  underflow -> UINT_MAX */
unsigned long e = 18446744073709551615UL + 1UL;  /* add:  ULONG_MAX + 1 wraps to 0 */

int main(void) { return 0; }