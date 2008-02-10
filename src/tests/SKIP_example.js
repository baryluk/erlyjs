// If the filename of the test starts with SKIP, erlyjs_testsuite:run() 
// will not execute the test.
// This makes sense if we know that a test will fail, e.g. if the compiler
// has not yet implemented the required functionality to make it pass, but we
// want to keep the test around, so we can enable it later.


// Mandatory. Return here a description of the test case.
function test_description() {
    return "Just a simple test";
}

// Mandatory. Return here an array of arguments the testsuite will use 
// to invoke the test() function. For no arguments return an empty array.
function test_args() {
    return [];
}

// Mandatory. Return here the expected test result. 
function test_ok() {
    return true;
}

// Optional. Provide here any global code.


// Mandatory. The actual test. 
// Testsuite invokes this function with the arguments from test_args() 
// and compares the return value with the expected result from test_ok().
function test() {
    return "not implemented yet";
}