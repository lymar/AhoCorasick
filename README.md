# AhoCorasick

[Aho-Corasick](http://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_string_matching_algorithm) string matching algorithm.

## Installation

    cabal update
    cabal install AhoCorasick

## Examples

#### Simplest example

```haskell
example1 = mapM_ print $ findAll simpleSM "ushers" where
    simpleSM = makeSimpleStateMachine ["he","she","his","hers"]
```

```
Position {pIndex = 1, pLength = 3, pVal = "she"}
Position {pIndex = 2, pLength = 2, pVal = "he"}
Position {pIndex = 2, pLength = 4, pVal = "hers"}
```

#### With data

```haskell
example2 = mapM_ print $ findAll sm "ushers" where
    sm = makeStateMachine [("he",0),("she",1),("his",2),("hers",3)]
```

```
Position {pIndex = 1, pLength = 3, pVal = 1}
Position {pIndex = 2, pLength = 2, pVal = 0}
Position {pIndex = 2, pLength = 4, pVal = 3}
```

#### Step-by-step state machine evaluation

```haskell
example3 = mapM_ print $ next sm "ushers" where
    sm = makeSimpleStateMachine ["he","she","his","hers"]
    next _ [] = []
    next sm (s:n) = let (SMStepRes match nextSM) = stateMachineStep sm s in
        (s, match) : next nextSM n
```

```
(u,[])
(s,[])
(h,[])
(e,[(3,"she"),(2,"he")])
(r,[])
(s,[(4,"hers")])
```
