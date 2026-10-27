/* Cases that must NOT trigger -Wshadow (matching gcc). Runs cleanly to 0. */
int a;
int proto(int a);              /* prototype parameter names don't shadow anything */

int g(void) {
    int g = 3;                 /* a variable shadowing a function */
    return g - 3;
}

int h(int h) {                 /* a parameter shadowing a function */
    return h;
}

int proto(int b) {
    return b;
}

int main(void) {
    return g() + h(0) + proto(0);
}
