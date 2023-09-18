type t<'a>;
type call<'call>;
type read<'a>;

let make: 'a => (t<'a>, call<'a>);

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
    (. 'b => unit, 'a) => option<unit => unit>,
) => t<'b>;

let reduce: (
    t<'a>,
    'b,
    ('b, 'a) => 'b,
) => (t<'b>, read<'b>);

let sub: (
    t<'a>,
    'a => unit,
) => ((. unit) => unit);

let call: (
    call<'a>,
    'a,
) => unit;

let read: read<'a> => 'a;
