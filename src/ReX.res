module Subs = Belt.Map.Int

type thunk<'a, 'b> = ('a, 'b => unit) => unit;
type t<'a, 'b> = {
    id: int,
    mutable thunk: thunk<'a, 'b>,
    mutable onNext: Subs.t<'b => unit>,
}

let id = a => a;

let makeId: unit => int
    =
    () => Js.Math.random_int(1, 9999);

let make: ('a => 'b) => t<'a, 'b>
    =
    map => {
        id: makeId(),
        onNext: Subs.empty,
        thunk: (value, dispatch) => value->map->dispatch,
    };

let call: (t<'a, 'b>, 'a) => unit
    =
    (t, value) => t.onNext->Subs.forEach((_, dispatch) => t.thunk(value, dispatch));

let sub: (t<'a, 'b>, 'b => unit) => (unit => unit)
    =
    (t, effect) => {
        let id = makeId();
        let unsub = () => t.onNext = t.onNext->Subs.remove(id);
        t.onNext = t.onNext->Subs.set(id, effect);
        unsub;
    };

let mapSub: (t<'a, 'b>, 'b => 'c) => (t<'b, 'c>, unit => unit)
    =
    (t, map) => {
        let res = make(map);
        let unsub = t->sub(call(res));
        (res, unsub);
    }

let thunk: (t<'a, 'b>, thunk<'b, 'c>) => t<'b, 'c>
    =
    (t, thunk) => {
        let res = {
            id: makeId(),
            thunk: thunk,
            onNext: Subs.empty,
        };
        t.onNext = t.onNext->Subs.set(res.id, value => call(res, value))
        res;
    }

let either: (t<'a, 'b>, t<'c, 'b>) => t<'b, 'b>
    =
    (a, b) => {
        let res = make(id);
        a.onNext = a.onNext->Subs.set(res.id, call(res));
        b.onNext = b.onNext->Subs.set(res.id, call(res));
        res;
    }

let both: (t<'a, 'b>, t<'c, 'd>, ('b, 'd)) => t<('b, 'd), ('b, 'd)>
    =
    (a, b, initial) => {
        let res = make(id);
        let both = ref(initial);
        a.onNext = a.onNext->Subs.set(res.id, value => {
            let (_, right) = both.contents;
            both := (value, right);
            res->call(both.contents);
        });
        b.onNext = b.onNext->Subs.set(res.id, value => {
            let (left, _) = both.contents;
            both := (left, value);
            res->call(both.contents);
        });
        res;
    }

let map: (t<'a, 'b>, 'b => 'c) => t<'b, 'c>
    =
    (t, map) => {
        let res = make(map);
        t.onNext = t.onNext->Subs.set(res.id, call(res));
        res;
    };

let filter: (t<'a, 'b>, 'b => bool) => t<'b, 'b>
    =
    (t, filter) => {
        let res = make(id);
        t.onNext = t.onNext->Subs.set(
            res.id,
            value => {
                if filter(value) {
                    res->call(value);
                }
            }
        );
        res;
    }

let reduce: (t<'a, 'b>, 'c, ('c, 'b) => 'c) => t<'b, 'c>
    =
    (t, initial, reducer) => {
        let stored = ref(initial);
        let res = make(id)->map(value => {
            let newValue = reducer(stored.contents, value);
            stored := newValue;
            newValue;
        });

        t.onNext = t.onNext->Subs.set(res.id, call(res));
        res;
    }

let flatMap: (t<'a, 'b>, 'b, 'b => t<'b, 'c>) => t<'b, 'c>
    =
    (t, empty, flatMap) => {
        let res = flatMap(empty);
        t.onNext = t.onNext->Subs.set(res.id, value => {
            let newRes = flatMap(value);
            res.thunk = newRes.thunk;
            res->call(value);
        })
        res;
    }

let interval: int => t<bool, int>
    =
    interval => {
        let timeoutId = ref(None);

        {
            id: makeId(),
            thunk: ((stop, dispatch) => {
                timeoutId.contents->Option.forEach(clearInterval);

                if !stop {
                    let id = makeId();
                    timeoutId := Some(setInterval(() => id->dispatch, interval))
                }
            }),
            onNext: Subs.empty,
        };
    };

let debounce: (t<'a, 'b>, int) => t<'b, 'b>
    =
    (t, delay) => {
        let timeoutId = ref(None);

        t->thunk((value, dispatch) => {
            timeoutId.contents->Option.forEach(clearTimeout);
            timeoutId := Some(setTimeout(() => value->dispatch, delay))
        })
    }

let delay: (t<'a, 'b>, int) => t<'b, 'b>
    =
    (t, delay) =>
        t->thunk((value, dispatch) => {
            setTimeout(() => value->dispatch, delay)->ignore;
        })
