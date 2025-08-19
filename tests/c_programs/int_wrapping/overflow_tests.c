int main(void) {
    int int_max = 2147483647;
    int int_min = -2147483648;

    // Test INT_MAX overflow
    int b = int_max + 1;  // Should wrap to INT_MIN
    
    // Test INT_MIN underflow
    int d = int_min - 1;  // Should wrap to INT_MAX
    
    // Test multiplication overflow
    int e = 1073741824;  // 2^30
    int f = e * 2;       // Should be INT_MIN
    
    // Test negative multiplication overflow
    int g = -1073741824;
    int h = g * 2;       // Should be INT_MIN
    
    // Test edge cases with arithmetic
    int n = 2147483640;
    int o = n + 10;  // Wraps to negative
    
    // Test subtraction underflow
    int p = -2147483640;
    int q = p - 10;  // Wraps to positive

    return (b == int_min) + (d == int_max) + (f == int_min) + (h == int_min) +  (o < 0) + (q > 0);
}