/* A malformed for-loop post expression must be a parse error. Dropping it would also drop the
   valid `i++`, so the loop would never end. */
int main(void) {
    int i;
    for (i = 0; i < 10; i++ +)
        ;
    return 0;
}
