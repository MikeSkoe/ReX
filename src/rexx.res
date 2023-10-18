module Subs = Belt.Map.Int

let id = a => a;

type thunk<'a, 'b> = ('a, 'b => unit) => unit;

type t<'a, 'b> = {
    id: int,
    mutable thunk: thunk<'a, 'b>,
    mutable onNext: Subs.t<'b => unit>,
}

let makeId: unit => int
    =
    () => Js.Math.random_int(1, 9999);

let make: ('a => 'b) => t<'a, 'b>
    =
    mapper => {
        id: makeId(),
        onNext: Subs.empty,
        thunk: (value, dispatch) => dispatch(mapper(value)),
    };

let sub: (t<'a, 'b>, 'b => unit) => (unit => unit)
    =
    (t, effect) => {
        let id = makeId();
        let unsub = () => t.onNext = t.onNext->Subs.remove(id);
        t.onNext = t.onNext->Subs.set(id, effect);
        unsub;
    };

let call: (t<'a, 'b>, 'a) => unit
    =
    (t, value) => t.onNext->Subs.forEach((_, dispatch) => t.thunk(value, dispatch));

let either: (t<'a, 'b>, t<'c, 'b>, 'b => 'd) => t<'b, 'd>
    =
    (a, b, map) => {
        let res = make(map);
        a.onNext = a.onNext->Subs.set(res.id, call(res));
        b.onNext = b.onNext->Subs.set(res.id, call(res));
        res;
    }

let both: (t<'a, 'b>, t<'c, 'd>, ('b, 'd), (('b, 'd)) => 'e) => t<('b, 'd), 'e>
    =
    (a, b, initial, map) => {
        let res = make(map);
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

let reduce: (t<'a, 'b>, 'c, ('c, 'b) => 'c) => t<'b, 'c>
    =
    (t, initial, reducer) => {
        let stored = ref(initial);
        let res = make(value => {
            let newValue = reducer(stored.contents, value);
            stored := newValue;
            newValue;
        });

        t.onNext = t.onNext->Subs.set(res.id, call(res));
        res;
    }

let map: (t<'a, 'b>, 'b => 'c) => t<'b, 'c>
    =
    (t, map) => {
        let res = make(map);
        t.onNext = t.onNext->Subs.set(res.id, call(res));
        res;
    };

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

let thunk: (t<'a, 'b>, thunk<'b, 'c>) => t<'b, 'c>
    =
    (t, thunk) => {
        let res = make(id);
        res.thunk = thunk;
        t.onNext = t.onNext->Subs.set(res.id, call(res))
        res;
    }

let interval: int => t<bool, int>
    =
    interval => {
        let x = ref(None);

        {
            id: makeId(),
            thunk: ((stop, dispatch) => {
                x.contents->Option.forEach(clearInterval);

                if !stop {
                    let id = makeId();
                    x := Some(setInterval(() => id->dispatch, interval))
                }
            }),
            onNext: Subs.empty,
        };
    };
