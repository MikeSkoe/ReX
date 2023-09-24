module List = Belt.List;

type t<'a> = ref<list<'a => unit>>;

let make = (_: 'a): t<'a> => ref(list{});

let thunk = (depOn, thunk) => {
    let resOn = ref(list{});
    let lastUnsub = ref(None);

    depOn := depOn.contents->List.add(value =>
        resOn.contents->List.forEach(fn => {
            lastUnsub.contents->Option.forEach(fn => fn());
            lastUnsub := thunk(. fn, value);
        }),
    );
    resOn;
};

let map = (depOn, map) => {
    let resOn = ref(list{});

    depOn := depOn.contents->List.add(value =>
        resOn.contents->List.forEach(fn =>
            value->map->fn,
        ),
    );
    resOn;
}

let merge = (a, b) => {
    let resOn = ref(list{});
    let callback = value => resOn.contents->List.forEach(fn => value->fn);

    a := a.contents->List.add(callback);
    b := b.contents->List.add(callback);
    resOn;
}

let reduce = (depOn, initial, reduce) => {
    let resOn = ref(list{});
    let state = ref(initial);

    depOn := depOn.contents->List.add(value => {
        state := reduce(state.contents, value);
        resOn.contents->List.forEach(fn =>
            state.contents->fn
        );
    });

    resOn;
}

let sub = (depOn, callback) => {
    depOn := depOn.contents->List.add(callback);
    (. ()) => depOn := depOn.contents->List.keep(fn => fn != callback);
}

let call = (t, value) => t.contents->List.forEach(fn => value->fn);

