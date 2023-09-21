module List = Belt.List;

let check = (on, ~call, ~msg, ~expected) => {
    let unsub = on->ReX.sub(Test.run(msg, expected));
    call();
    unsub(.);
}

let testMap = () => {
    let (x, setX) = ReX.make(1);

    check(x->ReX.map(num => num + 1),
        ~call=() => setX->ReX.call(1),
        ~msg="map",
        ~expected=2,
    );
}

let testReduce = () => {
    let (x, setX) = ReX.make();
    let appendIfOdd = (acc, curr) => mod(curr, 2) == 0 ? acc : acc->List.add(curr);
    let oddNums =
        x
        ->ReX.reduce(0, (acc, _) => acc + 1)
        ->ReX.reduce(list{}, appendIfOdd);

    for _ in 1 to 9 {
        setX->ReX.call();
    }

    check(oddNums,
        ~msg="reduce",
        ~expected=list{9, 7, 5, 3, 1},
        ~call=() => setX->ReX.call(),
    );
}

let testThunkSimple = () => {
    let (x, setX) = ReX.make(1);
    let thunked = x->ReX.thunk((. dispatch, a) => {
        dispatch(a);
        None;
    });

    check(
        ~call=() => setX->ReX.call(1),
        ~msg="thunk",
        ~expected=1,
        thunked,
    );
}

let testThunkFilter = () => {
    let (x, setX) = ReX.make(1);
    let callIfEven = (. dispatch, a) => {
        if mod(a, 2) == 0 {
            dispatch(a);
        }
        None;
    }
    let thunked =
        x
        ->ReX.thunk(callIfEven)
        ->ReX.reduce(0, (acc, curr) => acc + curr);

    setX->ReX.call(1);
    setX->ReX.call(2);
    setX->ReX.call(31);
    setX->ReX.call(44);
    setX->ReX.call(101);

    check(thunked,
        ~call=() => setX->ReX.call(302),
        ~msg="thunk filter",
        ~expected=2 + 44 + 302,
    );
}

let counter = () => {
    module Counter = {
        type action = Increment(int) | Reset;
        type state = int;
        let empty: state = 0;
        let reduce = (state: state, action: action) => switch action {
            | Increment(shift) => state + shift;
            | Reset => empty;
        }
    }

    let (incr, setIncr) = ReX.make(0);
    let (reset, setReset) = ReX.make();

    let input =
        incr
        ->ReX.map(shift => Counter.Increment(shift))
        ->ReX.merge(reset->ReX.map(_ => Counter.Reset))
        ->ReX.reduce(Counter.empty, Counter.reduce);

    setIncr->ReX.call(1);
    setIncr->ReX.call(20);

    check(input,
        ~call=() => setIncr->ReX.call(300),
        ~msg="counter == 321",
        ~expected=321,
    )
    check(input,
        ~call=() => setReset->ReX.call(),
        ~msg="counter is reset",
        ~expected=0,
    )

    setIncr->ReX.call(2);
    setIncr->ReX.call(30);
    check(input,
        ~call=() => setIncr->ReX.call(400),
        ~msg="counter == 432",
        ~expected=432,
    )
}

let main = () => {
    testMap();
    testReduce();
    testThunkSimple();
    testThunkFilter();
    counter();
}

main();
