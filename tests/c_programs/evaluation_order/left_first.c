int main(void) {
    int b = 0;
    // if left is evaluated first right will be true
    if (b++ || b)
        return 5;
    return 7;
}