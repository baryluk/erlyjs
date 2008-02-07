// If the filename of the test starts with SKIP, erlyjs_testsuite:run() 
// will not execute the test.
// This makes sense if we know that a test will fail, e.g. if the compiler
// has not yet implemented the required functionality to make it pass, but we
// want to keep the test around, so we can enable it later.

function test_description() {
    return "Just a simple test";
}

function test_args() {
    return "";
}

function test_result() {
    return "ok";
}

function test() {
    return "not implemented yet";
}