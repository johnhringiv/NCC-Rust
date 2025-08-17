// Test for empty for-loop components
// Tests various combinations of missing for-loop components

int main(void) {
    int count = 0;
    int i = 5;
    
    // Test 1: Missing initialization only
    for (; i < 10; i++) {
        count++;
    }
    
    // Test 2: Missing post-expression only
    for (int j = 0; j < 3;) {
        count++;
        j++;
    }
    
    // Test 3: Missing condition only (with break to avoid infinite loop)
    for (int k = 0; ; k++) {
        count++;
        if (k >= 2) {
            break;
        }
    }
    
    // Test 4: Missing init and post
    int m = 0;
    for (; m < 2;) {
        count++;
        m++;
    }
    
    // Test 5: Missing init and condition (with break)
    int n = 0;
    for (;; n++) {
        count++;
        if (n >= 1) {
            break;
        }
    }
    
    // Test 6: Missing condition and post (with break)
    for (int p = 0;;) {
        count++;
        p++;
        if (p >= 2) {
            break;
        }
    }
    
    // count should be: 5 + 3 + 3 + 2 + 2 + 2 = 17
    return count;
}