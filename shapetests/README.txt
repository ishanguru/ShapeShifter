There are two groups of tests in the ShapeShifter test suite: (1) tests that are supposed to pass and whose filenames are structured as test_*.shift (2) tests that are supposed to fail and whose filenames are structured as fail_*.shift. 

In each of those groups, various aspects of the ShapeShifter language tested systematically. The nature of the test is represented in the filename itself. For example, text_arith_int.shift tests our whether arithmetic operations function properly using only int data types. 


TO IMPLEMENT -- testing operations, illegal data types
+ fail_arith_mixedtypes.shift
+ fail_eq_mixedtypes.shift
+ tests comparing shapes < > == 
+ tests comparing ints
+ tests comparing bools
+ test UNIONing ints (fail)
+ test DIFFERENCEing ints (fail)
+ test INTERSECTing ints (fail)
+ test UNIONing bools (fail)
+ test DIFFERENCEing bools (fail)
+ test INTERSECTing bools (fail)

TO IMPLEMENT -- testing "shape rules", type promotion, type casting 
