/* -Wconstant-conversion: a constant implicitly narrowed to a type that can't hold it.
   `2147483648` is a `long` (too big for `int`); the implicit conversion truncates it to
   `-2147483648`. Folds at compile time, so the program runs cleanly to 0. */
int a = 2147483648;
int main(void) { return 0; }
