enum square {
    x,
    o,
    empty,
}

struct triple<S> {
    first: S,
    second: S,
    third: S,
}

fn make_triple<S>(s1: S, s2: S, s3: S) -> triple<S> {
    (s1, s2, s3)
}
