/* A block-scope `extern` hiding a local: the inner `x` is a different object (the file-scope
   one below), so the declaration shadows the local, and -Wshadow fires once. Returns 0. */
int main(void) {
    int x = 1;
    {
        extern int x;
        return x - 5;
    }
}

int x = 5;
