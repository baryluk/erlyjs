function test_description() {
    return "Greater '>' operator test";
}

function test_args() {
    return "";
}

function test_result() {
    return false;
}

function test() {
    var a=4, b=6, c;
    c = a > b;
    return c;
}