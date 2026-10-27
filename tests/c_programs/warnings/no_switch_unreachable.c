/* Switch bodies whose first code is reachable must NOT get -Wswitch-unreachable (matching gcc).
   Runs cleanly to 0. */
int main(void) {
    int x = 1;
    int y = 0;
    switch (x) { int z; case 1: y = 0; }                 /* declaration without initializer */
    switch (x) { lbl: case 1: y = 0; }                   /* labeled statement */
    switch (x) { while (y) { case 1: y = 0; } }          /* loop condition runs after the jump */
    switch (x) { do { case 1: y = 0; } while (y); }
    switch (x) { for (; y; ) { case 1: y = 0; } }        /* `for` with no initializer */
    switch (x) ;                                         /* empty body */
    return y;
}
