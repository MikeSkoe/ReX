type t<'a>;

let make: 'a => t<'a>;

let merge: (
    t<'a>,
    t<'a>,
) => t<'a>;

let thunk: (
    t<'a>,
    (. ('b => unit), 'a) => option<unit => unit>, 
) => t<'b>;

let sub: (
    t<'a>,
    'a => unit,
) => (unit => unit);

let call: (
    t<'a>,
    'a,
) => unit;

module Utils: {
    let reduce: (
        t<'a>,
        'b,
        ('b, 'a) => 'b,
    ) => t<'b>;

    let map: (
        t<'a>,
        'a => 'b,
    ) => t<'b>;
}
