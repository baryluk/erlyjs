function test_description() {
    return "Logical OR '||' operator test";
}

function test_args() {
    return "";
}

function test_result() {
    return false;
}

function test() {
    var a=false, b=false, c;
    c = a || b;
    return c;
}