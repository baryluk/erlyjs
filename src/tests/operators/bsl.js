function test_description() {
    return "Bitwise shift left '<<' operator test";
}

function test_args() {
    return "";
}

function test_result() {
    return 16;
}

function test() {
    var a=4, b=2, c;
    c = a << b;
    return c;
}