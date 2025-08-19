int main(void) {
    int x = 18;
    int result = 0;
    
    // Test switch with valid complex constant expressions
    switch(x) {
        // Basic arithmetic
        case 2 + 3:  // 5
            result = 1;
            break;
            
        // Multiplication and division
        case 2 * 6:  // 12
            result = 3;
            break;
            
        case 20 / 5:  // 4
            result = 4;
            break;
            
        // Modulo
        case 17 % 7:  // 3
            result = 5;
            break;
            
        // Bitwise operations
        case 15 & 7:  // 7
            result = 7;
            break;
            
        case 1 << 3:  // 8
            result = 9;
            break;
            
        case 64 >> 2:  // 16
            result = 10;
            break;
            
        // Unary operations
        case -(-10):  // 10
            result = 11;
            break;
            
        case ~(~20):  // 20
            result = 12;
            break;
            
        case !0:  // 1
            result = 13;
            break;
            
        case !5:  // 0
            result = 14;
            break;
            
        // Complex nested expressions
        case ((8 >> 1) + 2) * 3:  // 18
            result = 16;
            break;
            
        case (15 & 12) | (3 ^ 1):  // 14
            result = 17;
            break;
            
        // Edge cases
        case 2147483647:  // INT_MAX
            result = 26;
            break;
            
        case -2147483647 - 1:  // INT_MIN (avoiding direct -2147483648)
            result = 27;
            break;
            
        // Complex bitwise combination
        case (255 << 8) | 66:  // 65280 | 66 = 65346
            result = 28;
            break;
            
        default:
            result = 100;
            break;
    }
    
    // Another switch to test that same values are OK in different switches
    switch(x) {
        case 1 + 1:  // 2
            result = 200;
            break;
            
        case 3 * 3:  // 9
            result = 201;
            break;
            
        case 100 / 5:  // 20
            result = 202;
            break;
            
        case (1 << 4) - 1:  // 15
            result = 203;
            break;
            
        case ~(-4):  // 3 (bitwise NOT of -4)
            result = 204;
            break;
    }
    
    return result;
}