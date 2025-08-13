int main(void) {
    int i = 5;
    int a = i;     // a = 5
    int b = i++;   // b = 5, i becomes 6
    int c = i;     // c = 6
    return a + b + c;  // Should be 5 + 5 + 6 = 16
}