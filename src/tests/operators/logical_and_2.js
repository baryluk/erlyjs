function test_description() {
    return "Logical AND '&&' operator test";
}

function test_args() {
    return "";
}

function test_result() {
    return false;
}

function test() {
    var a=true, b=false, c;
    c = a && b;
    return c;
}