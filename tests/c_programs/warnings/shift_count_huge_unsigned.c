/* A huge unsigned shift count is too wide, not negative: expect -Wshift-count-overflow only.
   NCC masks shift counts, so this runs; the result isn't checked. */
int main(void) {
    int x = 1 << 18446744073709551615ul;
    long y = 1l << 4294967295u;
    return 0;
}
