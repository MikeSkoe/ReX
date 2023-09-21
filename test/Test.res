let run = (
    ~comparator: (('a, 'a) => bool) = ((a, b) => a == b),
    message: string,
    left: 'a,
    right: 'a,
) => {
    if comparator(left, right) {
        Js.log(`🍀: ${message}`);
    } else {
        Js.log(`💥: ${message}`);
        Js.log3(left, "!=", right);
    }
}
