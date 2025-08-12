int main(void) {
    int i = 5;
    // Undefined in standard C: i is modified and read without sequence point
    // With left-to-right: i (5) + i++ (5, then i becomes 6) + i (6) = 16
    // With other orders: could be 15, 16, 17, or 18
    int result = i + i++ + i;
    return result;
}