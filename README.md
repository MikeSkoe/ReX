# A ReScript functional reactive library, inspired by [React](https://github.com/dbuenzli/react) (not React.js) and [RxJS](https://rxjs.dev) library

Published in NPM as `@mikeskoe/rex`


> The library is in development, so it is not yet ready for production

## Dictionary
- **Observable:**
is a way to compose events over time.
It has type `t<'a, 'b>`, where `'a` is input event and `'b` is an output event

- **Event:** 
represents a discrete, instantaneous occurrence.
You can think of it as an action in Redux

- **Subscription:**
cancallable effect on an observable's event

## Example

```rescript
module Counter = {
    type action = Increment(int) | Reset;

    type state = int;

    let empty: state = 0;

    let reduce = (state: state, action: action) =>
        switch action {
            | Increment(shift) => state + shift;
            | Reset => empty;
        }
}

// t<int, Count.action>
let incr = make(shift => Counter.Increment(shift));
// t<unit, Count.action>
let reset = make(_ => Counter.Reset);
// t<Count.action, Count.state>
let counter =
    either(incr, reset)
    ->reduce(Counter.empty, Counter.reduce)
    ->debounce(100);

// unit => unit
let unsub = counter->sub(Js.log);

incr->call(1);
incr->call(20);
// ...
// logs: "21" after 100 milliseconds

reset->call(); 
// ...
// logs: "0" after 100 milliseconds

unsub();
```
