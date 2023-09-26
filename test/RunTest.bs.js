// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var ReX = require("../src/ReX.bs.js");
var Test = require("./Test.bs.js");
var Curry = require("rescript/lib/js/curry.js");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Js_promise2 = require("rescript/lib/js/js_promise2.js");

function wait(ms) {
  return new Promise((function (resolve, param) {
                setTimeout((function (param) {
                        resolve(undefined);
                      }), ms);
              }));
}

function interval(ms, timeout, dispatch) {
  var y = ReX.make(undefined);
  var unsub = ReX.sub(ReX.reduce(y, 0, (function (acc, param) {
              return acc + 1 | 0;
            })), dispatch);
  var id = setInterval((function (param) {
          return ReX.call(y, param);
        }), ms);
  Js_promise2.then(wait(timeout), (async function (param) {
          clearInterval(id);
        }));
  return (function (param) {
            clearInterval(id);
            Curry._1(unsub, undefined);
          });
}

async function getLastValue(on, call) {
  var lastValue = {
    contents: undefined
  };
  var unsub = ReX.sub(on, (function (value) {
          lastValue.contents = Caml_option.some(value);
        }));
  await Curry._1(call, undefined);
  Curry._1(unsub, undefined);
  return lastValue.contents;
}

var Util = {
  wait: wait,
  interval: interval,
  getLastValue: getLastValue
};

async function testMap(param) {
  var x = ReX.make(1);
  var mapped = ReX.map(x, (function (num) {
          return num + 1 | 0;
        }));
  var lastValue = await getLastValue(mapped, (async function (param) {
          return ReX.call(x, 1);
        }));
  return Test.run(undefined, "map", lastValue, 2);
}

async function testReduce(param) {
  var x = ReX.make(undefined);
  var appendIfOdd = function (acc, curr) {
    if (curr % 2 === 0) {
      return acc;
    } else {
      return Belt_List.add(acc, curr);
    }
  };
  var oddNums = ReX.reduce(ReX.reduce(x, 0, (function (acc, param) {
              return acc + 1 | 0;
            })), /* [] */0, appendIfOdd);
  var lastValue = await getLastValue(oddNums, (async function (param) {
          for(var _for = 1; _for <= 10; ++_for){
            ReX.call(x, undefined);
          }
        }));
  return Test.run(undefined, "reduce", lastValue, {
              hd: 9,
              tl: {
                hd: 7,
                tl: {
                  hd: 5,
                  tl: {
                    hd: 3,
                    tl: {
                      hd: 1,
                      tl: /* [] */0
                    }
                  }
                }
              }
            });
}

async function testThunkFilter(param) {
  var x = ReX.make(1);
  var callIfEven = function (dispatch, a) {
    if (a % 2 === 0) {
      Curry._1(dispatch, a);
    }
    
  };
  var thunked = ReX.reduce(ReX.thunk(x, callIfEven), 0, (function (acc, curr) {
          return acc + curr | 0;
        }));
  var lastValue = await getLastValue(thunked, (async function (param) {
          ReX.call(x, 1);
          ReX.call(x, 2);
          ReX.call(x, 31);
          ReX.call(x, 44);
          ReX.call(x, 101);
          return ReX.call(x, 302);
        }));
  return Test.run(undefined, "thunk filter", lastValue, 348);
}

async function testCounter(param) {
  var reduce = function (state, action) {
    if (action) {
      return state + action._0 | 0;
    } else {
      return 0;
    }
  };
  var incr = ReX.make(0);
  var reset = ReX.make(undefined);
  var input = ReX.reduce(ReX.merge(ReX.map(incr, (function (shift) {
                  return /* Increment */{
                          _0: shift
                        };
                })), ReX.map(reset, (function (param) {
                  return /* Reset */0;
                }))), 0, reduce);
  var lastValue = await getLastValue(input, (async function (param) {
          ReX.call(incr, 1);
          ReX.call(incr, 20);
          return ReX.call(incr, 300);
        }));
  Test.run(undefined, "counter === 321", lastValue, 321);
  var lastValue$1 = await getLastValue(input, (async function (param) {
          return ReX.call(reset, undefined);
        }));
  Test.run(undefined, "counter is reset", lastValue$1, 0);
  var lastValue$2 = await getLastValue(input, (async function (param) {
          ReX.call(incr, 2);
          ReX.call(incr, 30);
          return ReX.call(incr, 400);
        }));
  return Test.run(undefined, "counter == 432", lastValue$2, 432);
}

async function testInterval(param) {
  var x = ReX.make(undefined);
  var incr = ReX.map(ReX.reduce(ReX.thunk(x, (function (dispatch, param) {
                  return interval(100, 1100, dispatch);
                })), /* [] */0, Belt_List.add), Belt_List.reverse);
  var lastValue = await getLastValue(incr, (async function (param) {
          ReX.call(x, undefined);
          await wait(400);
          ReX.call(x, undefined);
          return await wait(2000);
        }));
  return Test.run(undefined, "timer", lastValue, {
              hd: 1,
              tl: {
                hd: 2,
                tl: {
                  hd: 3,
                  tl: {
                    hd: 1,
                    tl: {
                      hd: 2,
                      tl: {
                        hd: 3,
                        tl: {
                          hd: 4,
                          tl: {
                            hd: 5,
                            tl: {
                              hd: 6,
                              tl: {
                                hd: 7,
                                tl: {
                                  hd: 8,
                                  tl: {
                                    hd: 9,
                                    tl: {
                                      hd: 10,
                                      tl: /* [] */0
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            });
}

async function main(param) {
  console.log("test {");
  await Promise.all([
        testMap(undefined),
        testReduce(undefined),
        testThunkFilter(undefined),
        testCounter(undefined),
        testInterval(undefined)
      ]);
  console.log("}");
}

main(undefined);

var List;

exports.List = List;
exports.Util = Util;
exports.testMap = testMap;
exports.testReduce = testReduce;
exports.testThunkFilter = testThunkFilter;
exports.testCounter = testCounter;
exports.testInterval = testInterval;
exports.main = main;
/*  Not a pure module */
