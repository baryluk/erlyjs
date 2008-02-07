function test_description() {
    return "Strict equal '===' operator test";
}

function test_args() {
    return "";
}

function test_result() {
    return false;
}

function test() {
    var a=4, b=5, c;
    c = a === b;
    return c;
}