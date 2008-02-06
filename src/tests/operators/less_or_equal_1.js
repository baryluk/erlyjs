function test_description() {
    return "Less or equal '<=' operator test";
}

function test_args() {
    return "";
}

function test_result() {
    return true;
}

function test() {
    var a=4, b=7, c;
    c = a <= b;
    return c;
}