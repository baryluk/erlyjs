function test_description() {
    return "Less or equal '<=' operator test";
}

function test_args() {
    return "";
}

function test_result() {
    return false;
}

function test() {
    var a=9, b=7, c;
    c = a <= b;
    return c;
}