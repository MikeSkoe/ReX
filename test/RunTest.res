module List = Belt.List;

module Util = {
    let wait = ms => Js.Promise2.make((~resolve, ~reject as _) =>
        setTimeout(() => resolve(. ()), ms)->ignore,
    );

    let interval = (ms, timeout, dispatch) => {
        let y = ReX.make();
        let unsub = y
            ->ReX.Utils.reduce(0, (acc, _) => acc + 1)
            ->ReX.sub(dispatch);
        let id = Js.Global.setInterval(y->ReX.call, ms);

        wait(timeout)
            ->Js.Promise2.then(async () => Js.Global.clearInterval(id))
            ->ignore;

        Some(() => {
            Js.Global.clearInterval(id);
            unsub();
        });
    }

    let getLastValue = async (on, call) => {
        let lastValue = ref(None);
        let unsub = on->ReX.sub(value => lastValue := Some(value));
        await call();
        unsub();
        lastValue.contents;
    }
}

let testMap = async () => {
    let x = ReX.make(1);
    let mapped = x->ReX.Utils.map(num => num + 1);

    let lastValue = await Util.getLastValue(
        mapped,
        async () => x->ReX.call(1),
    );

    Test.run("map", lastValue, Some(2));
}

let testReduce = async () => {
    let x = ReX.make();
    let appendIfOdd = (acc, curr) => mod(curr, 2) == 0 ? acc : acc->List.add(curr);
    let oddNums =
        x
        ->ReX.Utils.reduce(0, (acc, _) => acc + 1)
        ->ReX.Utils.reduce(list{}, appendIfOdd);

    let lastValue = await Util.getLastValue(oddNums, async () => {
        for _ in 1 to 10 {
            x->ReX.call();
        }
    });

    Test.run("reduce", lastValue, Some(list{9, 7, 5, 3, 1}));
}

let testThunkFilter = async () => {
    let x = ReX.make(1);
    let callIfEven = (. dispatch, a) => {
        if mod(a, 2) == 0 {
            dispatch(a);
        }
        None;
    }
    let thunked =
        x
        ->ReX.thunk(callIfEven)
        ->ReX.Utils.reduce(0, (acc, curr) => acc + curr);

    let lastValue = await Util.getLastValue(thunked, async () => {
        x->ReX.call(1);
        x->ReX.call(2);
        x->ReX.call(31);
        x->ReX.call(44);
        x->ReX.call(101);
        x->ReX.call(302);
    });

    Test.run("thunk filter", lastValue, Some(2 + 44 + 302));
}

let testCounter = async () => {
    module Counter = {
        type action = Increment(int) | Reset;
        type state = int;
        let empty: state = 0;
        let reduce = (state: state, action: action) => switch action {
            | Increment(shift) => state + shift;
            | Reset => empty;
        }
    }

    let incr = ReX.make(0);
    let reset = ReX.make();

    let input =
        incr
        ->ReX.Utils.map(shift => Counter.Increment(shift))
        ->ReX.merge(reset->ReX.Utils.map(_ => Counter.Reset))
        ->ReX.Utils.reduce(Counter.empty, Counter.reduce);

    let lastValue = await Util.getLastValue(input, async () => {
        incr->ReX.call(1);
        incr->ReX.call(20);
        incr->ReX.call(300)
    });

    Test.run("counter === 321", lastValue, Some(321));

    let lastValue = await Util.getLastValue(input, async () => {
        reset->ReX.call();
    });

    Test.run("counter is reset", lastValue, Some(Counter.empty));

    let lastValue = await Util.getLastValue(input, async () => {
        incr->ReX.call(2);
        incr->ReX.call(30);
        incr->ReX.call(400);
    });

    Test.run("counter == 432", lastValue, Some(432));
}

let testInterval = async () => {
    let x = ReX.make();
    let incr =
        x
        ->ReX.thunk((. dispatch, _) => Util.interval(100, 1100, dispatch))
        ->ReX.Utils.reduce(list{}, Belt.List.add)
        ->ReX.Utils.map(Belt.List.reverse);

    let lastValue = await Util.getLastValue(incr, async () => {
        x->ReX.call();
        await Util.wait(400);
        x->ReX.call();
        await Util.wait(2000);
    });

    Test.run("timer",
        lastValue,
        Some(list{1, 2, 3, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}),
    );
}

type tempStrc = { isEven: bool, value: int };

let testSub = async () => {
    let x = ReX.make(0);
    let strc =
        x
        ->ReX.Utils.map(value => {
            isEven: mod(value, 2) == 0,
            value,
        })
        ->ReX.Utils.map(({ isEven }) => isEven);

    let lastValue = await Util.getLastValue(strc, async () => {
        x->ReX.call(3);
    });

    Test.run("sub",
        lastValue,
        Some(false),
    );
    x->ReX.call(2);
    Test.run("unsubed",
        lastValue,
        Some(false),
    );
};

let main = async () => {
    Js.log("test {");
    let _ = await Js.Promise2.all([
        testMap(),
        testReduce(),
        testThunkFilter(),
        testCounter(),
        testInterval(),
        testSub(),
    ]);
    Js.log("}");
}

main()->ignore;
