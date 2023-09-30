module List = Belt.List;

type t<'a> = {
    id: int,
    mutable subs: list<'a => unit>,
};

let makeId = () => Js.Math.random_int(1, 99999);

let make = (_: 'a): t<'a> => {
    id: makeId(),
    subs: list{},
}

let thunk = (depOn, thunk) => {
    let resOn = {
        id: makeId(),
        subs: list{},
    };
    let lastUnsub = ref(None);

    depOn.subs = depOn.subs->List.add(value =>
        resOn.subs->List.forEach(fn => {
            lastUnsub.contents->Option.forEach(fn => fn());
            lastUnsub := thunk(. fn, value);
        }),
    );
    resOn;
};

let merge = (a, b) => {
    let resOn = {
        id: makeId(),
        subs: list{},
    };
    let callback = value => resOn.subs->List.forEach(fn => value->fn);

    a.subs = a.subs->List.add(callback);
    b.subs = b.subs->List.add(callback);
    resOn;
}

let sub = (depOn, callback) => {
    depOn.subs = depOn.subs->List.add(callback);
    () => depOn.subs = depOn.subs->List.keep(fn => fn !== callback);
}

let call = (t, value) => t.subs->List.forEach(fn => value->fn);

module Utils = {
    let reduce = (depOn, initial, reduce) => {
        let resOn = {
            id: makeId(),
            subs: list{},
        };
        let state = ref(initial);

        depOn.subs = depOn.subs->List.add(value => {
            state := reduce(state.contents, value);
            resOn.subs->List.forEach(fn =>
                state.contents->fn
            );
        });

        resOn;
    }

    let map = (depOn, map) => {
        let resOn = {
            id: makeId(),
            subs: list{},
        }

        depOn.subs = depOn.subs->List.add(value =>
            resOn.subs->List.forEach(fn =>
                value->map->fn,
            ),
        );
        resOn;
    }
}

