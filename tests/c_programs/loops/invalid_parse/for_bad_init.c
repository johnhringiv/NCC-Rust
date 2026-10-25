/* A malformed for-loop init must be a parse error, not silently treated as an empty clause. */
int main(void) {
    int x;
    for (x = ; x < 3; x = x + 1)
        ;
    return 0;
}
