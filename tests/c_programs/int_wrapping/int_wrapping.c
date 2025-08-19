int main(void) {
    int int_max = 2147483647; // + 1 to go to zero
    int int_min = -2147483648; //-1 to go to int max

    int wrapped_max = int_max + 1; // wraps to zero
    int wrapped_min = int_min -1; // wraps to int max

    return int_min + 1 + wrapped_max + 3 + - wrapped_min - 4 + int_max - int_min;
}