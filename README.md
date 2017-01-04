# **J**ust **a**nother **c**ompiler **e**xperiment

> Results of some personal holiday hacking session

This project is a compiler written in Haskell for a Datalog-like DSL that generates JavaScript (ES5/CommonJS) code which uses the [LogicJS library](https://github.com/mcsoto/LogicJS) to perform logic deductions.

The language is basically Datalog with: `:-` replaced with `if`, `,` replaced with `and`, and no mixed extensional/intensional predicates (only pure rules or pure facts allowed).

## Example

```
loves(vincent,mia).
loves(marsellus,mia).
loves(pumpkin,honeyBunny).
loves(honeyBunny,pumpkin).

jealous(X,Y) if loves(X,Z) and loves(Y,Z).

evaluate jealous(marsellus,W).
```

When running the compiler, it generates the following code:

```js
var $logic = require('logicjs');
var $or = $logic.or, $and = $logic.and, $eq = $logic.eq, $lvar = $logic.lvar;
function $report(t, vns, vss){console.log('evaluate '+t+':\n'+vss.map(function(vs,j){return'    '+(j+1)+'. '+vs.map(function(v,i){return vns[i]+'='+v;}).join(', ');}).join('\n'));}

function loves(X1, X2) {
  return $or(
    $and($eq(X1, 'vincent'), $eq(X2, 'mia')),
    $and($eq(X1, 'marsellus'), $eq(X2, 'mia')),
    $and($eq(X1, 'pumpkin'), $eq(X2, 'honeyBunny')),
    $and($eq(X1, 'honeyBunny'), $eq(X2, 'pumpkin'))
  );
}

function jealous(X, Y) {
  var Z = $lvar();
  return $and(loves(X, Z), loves(Y, Z));
}

var W = $lvar();
$report('jealous(marsellus,W)', ['W'], $logic.run(jealous('marsellus',W), [W]));
```

Which can be executed in Node.js to output:

```
evaluate jealous(marsellus,W):
    1. W=vincent
    2. W=marsellus
```

## Syntax

Facts:

```
loves(vincent,mia).
```

```
height(andre,177).
```

```
hungry(andre).
```

Rules:
```
jealous(X,Y) if loves(X,Z) and loves(Y,Z).
```

```
conflict(R1, R2, Coloring) if
    adjacent(R1, R2) and
    color(R1, Color, Coloring) and
    color(R2, Color, Coloring).
```

Evaluations:

```
evaluate hungry(X).
```

```
evaluate conflictingAreas(X,Y).
```

## Disclaimer

This compiler and the runtime is probably broken in a bunch of ways, e.g. recursive rules blow up when evaluated. It doesn't matter, this is just a hobby project to give Haskell a test drive. Probably also my Haskell code is naive in a couple of ways.
