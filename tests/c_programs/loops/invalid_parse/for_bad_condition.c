/* A malformed for-loop condition must be a parse error. Treating it as empty would make the
   loop run forever. */
int main(void) {
    int x;
    for (x = 0; x < ; x = x + 1)
        ;
    return 0;
}
