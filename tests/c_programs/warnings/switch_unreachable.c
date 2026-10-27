/* Each switch below has code before its first label that can never run, so each must get
   exactly one -Wswitch-unreachable warning (5 total, matching gcc). Runs cleanly to 0. */
int main(void) {
    int x = 1;
    int y = 0;
    switch (x) { ; y = 2; case 1: y = 0; }                     /* empty statement skipped */
    switch (x) { int z = 3; case 1: y = 0; }                   /* initialized declaration */
    switch (x) { if (y) { case 1: y = 0; } }                   /* `if` condition runs first */
    switch (x) { for (y = 0; y < 1; y = y + 1) { case 1: ; } } /* `for` initializer runs first */
    switch (x) { switch (y) { default: ; } case 1: ; }        /* nested switch's expression */
    return 0;
}
