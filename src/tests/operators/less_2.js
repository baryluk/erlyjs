function test_description() {
    return "Less '<' operator test";
}

function test_args() {
    return "";
}

function test_result() {
    return false;
}

function test() {
    var a=4, b=4, c;
    c = a < b;
    return c;
}