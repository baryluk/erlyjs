function test_description() {
    return "Not equal '!=' operator test";
}

function test_args() {
    return "";
}

function test_result() {
    return true;
}

function test() {
    var a=3, b=4, c;
    c = a != b;
    return c;
}