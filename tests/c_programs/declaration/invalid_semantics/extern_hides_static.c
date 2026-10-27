/* The local `s` hides the file-scope `static int s`, so the block-scope `extern int s` can't see
   it and gets external linkage. The same name then has both internal and external linkage,
   which is an error (C11 6.2.2p4, p7). */
static int s = 4;

int main(void) {
    int s = 1;
    {
        extern int s;
        return s;
    }
}
