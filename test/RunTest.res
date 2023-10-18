module List = Belt.List;

module Util = {
    let wait = ms => Js.Promise2.make((~resolve, ~reject as _) =>
        setTimeout(() => resolve(. ()), ms)->ignore,
    );

    let getLast = async (on, call) => {
        let lastValue = ref(None);
        let unsub = on->Rexx.sub(value => lastValue := Some(value));
        await call();
        unsub();
        lastValue.contents;
    }
}

let testMap = async () => {
    let x = Rexx.make(Rexx.id);
    let mapped = x->Rexx.map(num => num + 1);

    let lastValue = await Util.getLast(
        mapped,
        async () => x->Rexx.call(1),
    );

    Test.run("map", lastValue, Some(2));
}

let testReduce = async () => {
    let x = Rexx.make(Rexx.id);
    let appendIfOdd = (acc, curr) => mod(curr, 2) == 0
        ? acc
        : acc->List.add(curr);
    let oddNums =
        x
        ->Rexx.reduce(0, (acc, _) => acc + 1)
        ->Rexx.reduce(list{}, appendIfOdd);

    let lastValue = await Util.getLast(oddNums, async () => {
        for _ in 1 to 10 {
            x->Rexx.call();
        }
    });

    Test.run("reduce", lastValue, Some(list{9, 7, 5, 3, 1}));
}

let testThunkFilter = async () => {
    let x = Rexx.make(Rexx.id);
    let callIfEven = (a, dispatch) => {
        if mod(a, 2) == 0 {
            dispatch(a);
        }
    }
    let thunked =
        x
        ->Rexx.thunk(callIfEven)
        ->Rexx.reduce(0, (acc, curr) => acc + curr);

    let lastValue = await Util.getLast(thunked, async () => {
        x->Rexx.call(1);
        x->Rexx.call(2);
        x->Rexx.call(31);
        x->Rexx.call(44);
        x->Rexx.call(101);
        x->Rexx.call(302);
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

    let incr = Rexx.make(Rexx.id);
    let reset = Rexx.make(Rexx.id);

    let input =
        Rexx.either(
            incr->Rexx.map(shift => Counter.Increment(shift)),
            reset->Rexx.map(_ => Counter.Reset),
            Rexx.id,
        )
        ->Rexx.reduce(Counter.empty, Counter.reduce);

    let lastValue = await input->Util.getLast(async () => {
        incr->Rexx.call(1);
        incr->Rexx.call(20);
        incr->Rexx.call(300)
    });

    Test.run("counter === 321", lastValue, Some(321));

    let lastValue = await input->Util.getLast(async () => {
        reset->Rexx.call();
    });

    Test.run("counter is reset", lastValue, Some(Counter.empty));

    let lastValue = await input->Util.getLast(async () => {
        incr->Rexx.call(2);
        incr->Rexx.call(30);
        incr->Rexx.call(400);
    });

    Test.run("counter == 432", lastValue, Some(432));
}

type reincrement = {
    id: int,
    value: int,
}

let testInterval = async () => {
    let log = value => {
        Js.log(value);
        value;
    };
    let interval = Rexx.interval(100);
    let incr = interval
        ->Rexx.reduce({ id: 0, value: 1 }, (acc, id) => id !== acc.id
            ? { id, value: 1 }
            : { ...acc, value: acc.value + 1 })
        ->Rexx.map(({ value }) => value);

    let lastValue = await incr->Util.getLast(async () => {
        interval->Rexx.call(false);
        await Util.wait(400);
        interval->Rexx.call(false);
        await Util.wait(2000);
        interval->Rexx.call(true);
    });

    Test.run("timer", lastValue, Some(19));
}

type tempStrc = { isEven: bool, value: int };

let testBoth = async () => {
    let a: Rexx.t<int, int> = Rexx.make(Rexx.id);
    let b: Rexx.t<string, string> = Rexx.make(Rexx.id);
    let res = Rexx.both(a, b, (0, ""), Rexx.id);
    let lastValue = await res->Util.getLast(async () => {
        a->Rexx.call(1);
        b->Rexx.call("A");
        a->Rexx.call(2);
    });

    Test.run("both", lastValue, Some((2, "A")));
}

let testFlatMap = async () => {
    let a: Rexx.t<bool, bool> = Rexx.make(Rexx.id);
    let res = a
        ->Rexx.flatMap(true, value => Rexx.make(_ => value == true ? "TRUE" : "FALSE"))
        ->Rexx.reduce(list{}, Belt.List.add);

    let lastValue = await res->Util.getLast(async () => {
        a->Rexx.call(true);
        a->Rexx.call(false);
        a->Rexx.call(false);
        a->Rexx.call(true);
    });

    Test.run("timer", lastValue, Some(list{"TRUE", "FALSE", "FALSE", "TRUE"}));
}

let testSub = async () => {
    let x = Rexx.make(Rexx.id);
    let strc =
        x
        ->Rexx.map(value => {
            isEven: mod(value, 2) == 0,
            value,
        })
        ->Rexx.map(({ isEven }) => isEven);

    let lastValue = await strc->Util.getLast(async () => {
        x->Rexx.call(3);
    });

    Test.run("sub", lastValue, Some(false));
    x->Rexx.call(2);
    Test.run("unsubed", lastValue, Some(false));
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
        testFlatMap(),
        testBoth(),
    ]);
    Js.log("}");
}

main()->ignore;
