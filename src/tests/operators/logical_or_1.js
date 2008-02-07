function test_description() {
    return "Logical OR '||' operator test";
}

function test_args() {
    return "";
}

function test_result() {
    return true;
}

function test() {
    var a=true, b=false, c;
    c = a || b;
    return c;
}