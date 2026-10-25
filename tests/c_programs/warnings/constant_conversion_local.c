/* -Wconstant-conversion also covers automatic locals: the conversion happens at run time, but a
   constant initializer's value is known at compile time. Runs cleanly to 0. */
int main(void) {
    int x = 2147483648; /* long constant truncated to int */
    return x - x;
}
