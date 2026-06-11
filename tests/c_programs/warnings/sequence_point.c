/* -Wsequence-point: the same object modified more than once between sequence points
   (undefined in standard C). NCC evaluates left-to-right, so these compile with warnings and
   run cleanly to 0; the dedicated test asserts the diagnostics via --validate. */
int two(int a, int b) { return a + b; }

int main(void) {
    int i = 0;
    i = i++;             /* '=' and '++' both modify i */
    int a = i++ + i++;   /* two increments of i in one region */
    two(i++, i++);       /* call args are unsequenced w.r.t. each other */
    return 0;
}
