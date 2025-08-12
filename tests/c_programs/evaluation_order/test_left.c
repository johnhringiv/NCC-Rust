int main(void) {
    int i = 5;
    int result = i + i++;  // Should be 5 + 5 = 10
    return result;
}