int main(void) {
    int x = 5;
    int y = 10;
    int result = 0;
    
    // Test non-constant case expressions - should all error
    switch(x) {
        case y:  // ERROR: y is not a constant expression
            result = 1;
            break;
            
        case x + 1:  // ERROR: x is not a constant
            result = 2;
            break;
            
        case y * 2:  // ERROR: y is not a constant
            result = 3;
            break;
            
        case (x > y ? 1 : 0):  // ERROR: x and y are not constants
            result = 4;
            break;
            
        // These should work - they're constant expressions
        case 5:
            result = 5;
            break;
            
        case 2 + 3:  // OK: constant expression
            result = 6;
            break;
            
        // Assignment is not a constant expression
        case (result = 7):  // ERROR: assignment is not constant
            result = 8;
            break;
            
        // Increment/decrement are not constant
        case ++result:  // ERROR: increment is not constant
            result = 9;
            break;
            
        case result++:  // ERROR: postfix increment is not constant
            result = 10;
            break;
            
        // Compound assignments are not constant
        case (result += 5):  // ERROR: compound assignment is not constant
            result = 11;
            break;
            
        default:
            result = 100;
            break;
    }
    
    return result;
}