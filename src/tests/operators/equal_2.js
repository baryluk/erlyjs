function test_description() {
    return "Equal '==' operator test";
}

function test_args() {
    return "";
}

function test_result() {
    return true;
}

function test() {
    var a=4, b=4, c;
    c = a == b;
    return c;
}