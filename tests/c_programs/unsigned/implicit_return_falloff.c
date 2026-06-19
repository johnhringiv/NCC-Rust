/* Regression (pre-existing since ch12 unsigned): a non-main function with an unsigned return type
   that falls off the end used to PANIC the tackifier — the synthetic-return match only handled
   int/long, so unsigned int / unsigned long hit `unreachable!`.

   This must compile, and the synthetic return must emit a real `ret` so control returns to the
   caller instead of falling through into the next function. Per C §6.9.1p12 the fall-off return
   *value* is undefined, so this test never observes it: the fall-off calls are made for their
   control-flow effect and their results are ignored, and the final answer comes only from the
   normal (explicit-return) paths. */
unsigned int counter = 0;

unsigned int bump(int run) {
    if (run) {
        counter = counter + 1u;
        return counter;
    }
    /* run == 0: reaches the closing brace with no return */
}

unsigned long bump_long(int run) {
    if (run) {
        return 7ul;
    }
    /* run == 0: falls off the end */
}

double bump_double(int run) {
    if (run) {
        return 3.5;
    }
    /* run == 0: falls off the end — synthetic return must target XMM0, not RAX */
}

int main(void) {
    bump(0);         /* fall-off path; result ignored (would be UB to use) */
    bump_long(0);    /* fall-off path; result ignored */
    bump_double(0);  /* fall-off path on a double-returning fn; result ignored */
    bump(1);         /* normal path: counter -> 1 */
    bump(1);         /* normal path: counter -> 2 */
    return (int)counter; /* defined: 2 */
}
