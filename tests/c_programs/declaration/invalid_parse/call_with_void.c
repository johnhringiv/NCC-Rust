/* `void` only means "no parameters" in a declaration. In a call, `f(void)` is an error. */
int f(void) {
    return 3;
}

int main(void) {
    return f(void);
}
