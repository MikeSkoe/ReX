type t<'a>;

let make: 'a => t<'a>;

let map: (
    t<'a>,
    'a => 'b,
) => t<'b>;

let merge: (
    t<'a>,
    t<'a>,
) => t<'a>;

let thunk: (
    t<'a>,
    (. ('b => unit), 'a) => option<unit => unit>, 
) => t<'b>;

let reduce: (
    t<'a>,
    'b,
    ('b, 'a) => 'b,
) => t<'b>;

let sub: (
    t<'a>,
    'a => unit,
) => ((. unit) => unit);

let call: (
    t<'a>,
    'a,
) => unit;

