function test_description() {
    return "Greater or equal '>=' operator test";
}

function test_args() {
    return "";
}

function test_result() {
    return true;
}

function test() {
    var a=7, b=7, c;
    c = a >= b;
    return c;
}