int main(void) {
    int i = 2147483646;
    int b = 0;

    while (i > 0) {
        b++;
        i++;
    }
    return b;
}