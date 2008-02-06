function test_description() {
    return "Bitwise shift right '>>' operator test";
}

function test_args() {
    return "";
}

function test_result() {
    return 4;
}

function test() {
    var a=16, b=2, c;
    c = a >> b;
    return c;
}