/*
 `t<'a, 'b>` is an observable that takes `'a` and emmits `'b` to observers
 'a=>('b)
 */
type t<'a, 'b>;

/*
 identity observable. Input type is the same as output type
 */
type id<'a> = t<'a, 'a>;

let make: unit => id<'a>;
let sub: (t<'a, 'b>, 'b => unit) => (unit => unit)
let call: (t<'a, 'b>, 'a) => unit

/*
 impure low-level operator
 */
let thunk: (t<'a, 'b>, ('b, 'b => unit) => unit) => id<'b>

/*
 t<'a, 'b> ╮
           |- id<'b>
 t<'c, 'b> ╯
 */
let either: (t<'a, 'b>, t<'c, 'b>) => id<'b>

/*
 t<'a, 'b> ╮
           |- id<('b, 'd)>
 t<'c, 'd> ╯
 */
let both: (t<'a, 'b>, t<'c, 'd>, ('b, 'd)) => id<('b, 'd)>

let reduce: (t<'a, 'b>, 'c, ('c, 'b) => 'c) => t<'b, 'c>
let map: (t<'a, 'b>, 'b => 'c) => t<'b, 'c>
let flatMap: (t<'a, 'b>, 'b, 'b => t<'b, 'c>) => t<'b, 'c>

/*
 takes interval and returnes self-emmited an observable
 that takes `start` boolean to begin or finish the interval
 */
let interval: int => t<bool, int>

let debounce: (t<'a, 'b>, int) => t<'b, 'b>
let delay: (t<'a, 'b>, int) => t<'b, 'b>
