/* NCC-specific: NaN -> unsigned long is undefined in standard C. At runtime NCC's lowering fails
   the 2^63 range check (NaN is unordered), takes the direct cvttsd2si path, and yields the
   "integer indefinite" bit pattern 0x8000000000000000. Constant folding must give the same value.
   Returns the number of checks that held; expected result is 3. */

static unsigned long s_nan = (unsigned long) (0.0 / 0.0);

int main(void) {
    int passed = 0;
    double zero = 0.0;
    double nan = zero / zero;

    if ((unsigned long) nan == 9223372036854775808ul) passed = passed + 1;
    if (s_nan == 9223372036854775808ul) passed = passed + 1;
    if (s_nan == (unsigned long) nan) passed = passed + 1;

    return passed; /* expect 3 */
}
