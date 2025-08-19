// Test conditional break after inner loop

int main(void) {
    int result = 0;
    
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 2; j++) {
            result++;
        }
        // Break AFTER inner loop completes
        if (i == 0) {
            break;
        }
    }
    
    return result;  // Should be 2
}