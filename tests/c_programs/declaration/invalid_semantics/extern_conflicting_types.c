/* Two block-scope `extern` declarations of the same object with different types. */
int main(void) {
    extern long x;
    {
        extern int x;
    }
    return 0;
}
