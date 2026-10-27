/* -Wshift-count-overflow in a file-scope initializer (folded at compile time, but still a
   shift whose count is >= the width of int). Runs cleanly to 0. */
int a = 1 << 32;
int main(void) {
    return 0;
}
