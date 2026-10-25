/* `%` needs integer operands, in a static initializer as anywhere else. The error must describe
   `%`, not `~`. */
static int x = 5.0 % 2;

int main(void) {
    return x;
}
