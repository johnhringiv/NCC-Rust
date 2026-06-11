// Tests compound assignment with mixed types (int += long).
// The result should be truncated back to int.
int main(void) {
    int x = 10;
    long y = 20L;
    x += y;  // common type is long, but result stored back as int
    return x; // expected: 30
}
