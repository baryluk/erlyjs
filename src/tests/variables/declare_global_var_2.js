// Mandatory. Return here a description of the test case.
function test_description() {
    return "Global variable declaration";
}

// Mandatory. Return here an array of arguments the testsuite will use 
// to invoke the test() function. For no arguments return an empty array.
function test_args() {
    return [];
}

// Mandatory. Return here the expected test result. 
function test_assert() {
    return 5;
}

// Optional. Provide here any global code.
var a = 5;

// Mandatory. The actual test. 
// Testsuite invokes this function with the arguments from test_args()
// and compares the return value with the expected result from test_assert().
function test() {
    return a;
}
