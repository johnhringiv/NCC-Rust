int main(void) {
    int x = 5;
    int result = 0;
    
    // Test duplicate case values - should trigger errors
    switch(x) {
        // Direct duplicate
        case 5:
            result = 1;
            break;
            
        case 5:  // ERROR: Duplicate case value
            result = 2;
            break;
            
        // Duplicate via arithmetic expressions
        case 2 + 3:  // 5
            result = 3;
            break;
            
        case 10 - 5:  // 5 - ERROR: Duplicate (same as 2+3)
            result = 4;
            break;
            
        case 20 / 4:  // 5 - ERROR: Another duplicate
            result = 5;
            break;
            
        // Duplicate via bitwise operations
        case 8 | 4:  // 12
            result = 6;
            break;
            
        case 2 * 6:  // 12 - ERROR: Duplicate (same as 8|4)
            result = 7;
            break;
            
        // Duplicate unary results
        case !0:  // 1
            result = 8;
            break;
            
        case 5 > 3:  // 1 - ERROR: Duplicate (same as !0)
            result = 9;
            break;
            
        case 4 == 4:  // 1 - ERROR: Another duplicate
            result = 10;
            break;
            
        case 10 >= 10:  // 1 - ERROR: Another duplicate
            result = 11;
            break;
            
        case 5 && 3:  // 1 - ERROR: Another duplicate
            result = 12;
            break;
            
        // Duplicate zeros
        case !5:  // 0
            result = 13;
            break;
            
        case 3 < 2:  // 0 - ERROR: Duplicate (same as !5)
            result = 14;
            break;
            
        case 5 != 5:  // 0 - ERROR: Another duplicate
            result = 15;
            break;
            
        case 9 <= 8:  // 0 - ERROR: Another duplicate
            result = 16;
            break;
            
        case 0 || 0:  // 0 - ERROR: Another duplicate
            result = 17;
            break;
            
        // Complex expression duplicates
        case (2 + 3) * 2:  // 10
            result = 18;
            break;
            
        case -(-10):  // 10 - ERROR: Duplicate
            result = 19;
            break;
            
        case (5 - 3) * 5:  // 10 - ERROR: Another duplicate
            result = 20;
            break;
            
        // Multiple defaults
        default:
            result = 100;
            break;
            
        default:  // ERROR: Duplicate default
            result = 101;
            break;
            
        default:  // ERROR: Another duplicate default
            result = 102;
            break;
    }
    
    return result;
}