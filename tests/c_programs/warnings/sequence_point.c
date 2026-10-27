/* -Wsequence-point: the same object modified more than once between sequence points
   (undefined in standard C). NCC evaluates left-to-right, so these compile with warnings and
   run cleanly to 0; the dedicated test asserts there are exactly 7 diagnostics. */
int two(int a, int b) { return a + b; }

int main(void) {
    int i = 0;
    i = i++;             /* '=' and '++' both modify i */
    int a = i++ + i++;   /* two increments of i in one region */
    two(i++, i++);       /* call args are unsequenced w.r.t. each other */

    /* &&, || and ?: sequence their own operands, but not against the rest of the expression,
       and call arguments are unsequenced against it too */
    int j = 0;
    j = i++ + two(i++, 0);           /* argument vs. the enclosing expression */
    j = (i ? i++ : 0) + i++;         /* ?: operand vs. sibling */
    j = (j && i++) + i++;            /* && operand vs. sibling */
    j = two(i++, two(i++, 0));       /* nested call arguments */
    return 0;
}
