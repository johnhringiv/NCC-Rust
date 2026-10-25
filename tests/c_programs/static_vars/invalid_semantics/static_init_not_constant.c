/* A static local's initializer must be a constant expression. The error must name `b` as
   written, not the resolver's internal `b.N`. */
int main(void) {
    int a = 1;
    static int b = a;
    return b;
}
