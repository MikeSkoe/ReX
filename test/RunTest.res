module List = Belt.List;

let { id, make, call, sub, map, reduce, thunk, either, both, interval, flatMap, debounce, delay, filter } = module(ReX);

module Util = {
    let wait = ms => Js.Promise2.make((~resolve, ~reject as _) =>
        setTimeout(() => resolve(. ()), ms)->ignore,
    );

    let getLast = async (on, call) => {
        let lastValue = ref(None);
        let unsub = on->sub(value => lastValue := Some(value));
        await call();
        unsub();
        lastValue.contents;
    }
}

let testMap = async () => {
    let state: ReX.t<int, int> = make(num => num + 1);
    let lastValue = await state
        ->Util.getLast(async () => {
            state->call(1);
        });

    Test.run("map", lastValue, Some(2));
}

let testReduce = async () => {
    let appendIfOdd = (acc, curr) => mod(curr, 2) == 0
        ? acc
        : acc->List.add(curr);

    let input = make(id);
    let lastValue = await input
        ->reduce(0, (acc, _) => acc + 1)
        ->reduce(list{}, appendIfOdd)
        ->Util.getLast(async () => {
            for _ in 1 to 10 {
                input->call();
            }
        });

    Test.run("reduce", lastValue, Some(list{9, 7, 5, 3, 1}));
}

let testFilter = async () => {
    let isEven = value => mod(value, 2) == 0;
    let input = make(id);
    let lastValue = await input
        ->filter(isEven)
        ->reduce(0, (acc, curr) => acc + curr)
        ->Util.getLast(async () => {
            input->call(1);
            input->call(2);
            input->call(31);
            input->call(44);
            input->call(101);
            input->call(302);
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

    let incr = make(shift => Counter.Increment(shift));
    let reset = make(_ => Counter.Reset);

    let state =
        either(incr, reset)
        ->reduce(Counter.empty, Counter.reduce);

    let lastValue = await state
        ->Util.getLast(async () => {
            incr->call(1);
            incr->call(20);
            incr->call(300);
        });

    Test.run("counter === 321", lastValue, Some(321));

    let lastValue = await state->Util.getLast(async () => {
        reset->call();
        await Util.wait(100);
    });

    Test.run("counter is reset", lastValue, Some(Counter.empty));

    let lastValue = await state->Util.getLast(async () => {
        incr->call(2);
        incr->call(30);
        incr->call(400);
    });

    Test.run("counter == 432", lastValue, Some(432));
}

let testDelay = async () => {
    let input = make(id);
    let lastValue = await input
        ->delay(100)
        ->reduce(list{}, Belt.List.add)
        ->Util.getLast(async () => {
            input->call(1);
            await Util.wait(10)
            input->call(2);
            input->call(3);
            await Util.wait(50)
            input->call(4);
        });

    Test.run("delay without waiting", lastValue, None)

    let lastValue = await input
        ->delay(100)
        ->reduce(list{}, Belt.List.add)
        ->Util.getLast(async () => {
            input->call(1);
            await Util.wait(10)
            input->call(2);
            input->call(3);
            await Util.wait(50)
            input->call(4);
            await Util.wait(100);
        });

    Test.run("delay with waiting", lastValue, Some(list{4, 3, 2, 1}))
}

let testDebounce = async () => {
    let input = make(id);
    let lastValue = await input
        ->debounce(100)
        ->reduce(list{}, Belt.List.add)
        ->Util.getLast(async () => {
            input->call(1);
            await Util.wait(200);
            input->call(2);
            input->call(3);
            input->call(4);
            await Util.wait(200);
        });

    Test.run("debounced", lastValue, Some(list{4, 1}))
}

type reincrement = {
    id: int,
    value: int,
}

let testInterval = async () => {
    let interval = interval(100);
    let lastValue = await interval
        ->reduce({ id: 0, value: 1 }, (acc, id) => id !== acc.id
            ? { id, value: 1 }
            : { ...acc, value: acc.value + 1 })
        ->map(({ value }) => value)
        ->Util.getLast(async () => {
            interval->call(false);
            await Util.wait(400);
            interval->call(false);
            await Util.wait(2000);
            interval->call(true);
        });

    Test.run("timer", lastValue, Some(19));
}

type tempStrc = { isEven: bool, value: int };

let testBoth = async () => {
    let a = make(id);
    let b = make(id);
    let lastValue = await both(a, b, (0, ""))
        ->Util.getLast(async () => {
            a->call(1);
            b->call("A");
            a->call(2);
        });

    Test.run("both", lastValue, Some((2, "A")));
}

let testFlatMap = async () => {
    let input = make(id);
    let lastValue = await input
        ->flatMap(true, value => make(id)->map(_ => value == true ? "TRUE" : "FALSE"))
        ->reduce(list{}, Belt.List.add)
        ->Util.getLast(async () => {
            input->call(true);
            input->call(false);
            input->call(false);
            input->call(true);
        });

    Test.run("timer", lastValue, Some(list{"TRUE", "FALSE", "FALSE", "TRUE"}));
}

let testSub = async () => {
    let input = make(id);
    let lastValue = await input
        ->map(value => {
            isEven: mod(value, 2) == 0,
            value,
        })
        ->map(({ isEven }) => isEven)
        ->Util.getLast(async () => {
            input->call(3);
        });

    Test.run("sub", lastValue, Some(false));
};

let main = async () => {
    let _ = await Js.Promise2.all([
        testMap(),
        testReduce(),
        testFilter(),
        testCounter(),
        testInterval(),
        testSub(),
        testFlatMap(),
        testBoth(),
        testDebounce(),
        testDelay(),
    ]);
    Js.log("Finished");
}

main()->ignore;
