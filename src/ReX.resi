/**
 `t<'a, 'b>` is an observable that takes `'a` and emmits `'b` to observers
 */
type t<'a, 'b>;

let id: 'a => 'a;
let make: ('a => 'b) => t<'a, 'b>;
let sub: (t<'a, 'b>, 'b => unit) => (unit => unit)

/**
 a weak dependency on an observable
 */
let mapSub: (t<'a, 'b>, 'b => 'c) => (t<'b, 'c>, unit => unit)
let call: (t<'a, 'b>, 'a) => unit

/**
 impure low-level operator
 */
let thunk: (t<'a, 'b>, ('b, 'b => unit) => unit) => t<'b, 'b>

/**
 t<'a, 'b> ╮
           |- t<'b, 'b>
 t<'c, 'b> ╯
 */
let either: (t<'a, 'b>, t<'c, 'b>) => t<'b, 'b>

/**
 t<'a, 'b> ╮
           |- t<('b, 'd), ('b, 'd)>
 t<'c, 'd> ╯
 */
let both: (t<'a, 'b>, t<'c, 'd>, ('b, 'd)) => t<('b, 'd), ('b, 'd)>

let reduce: (t<'a, 'b>, 'c, ('c, 'b) => 'c) => t<'b, 'c>
let map: (t<'a, 'b>, 'b => 'c) => t<'b, 'c>
let filter: (t<'a, 'b>, 'b => bool) => t<'b, 'b>
let flatMap: (t<'a, 'b>, 'b, 'b => t<'b, 'c>) => t<'b, 'c>

/**
 takes interval and returnes self-emmited an observable
 that takes `start` boolean to begin or finish the interval
 */
let interval: int => t<bool, int>

let debounce: (t<'a, 'b>, int) => t<'b, 'b>
let delay: (t<'a, 'b>, int) => t<'b, 'b>
