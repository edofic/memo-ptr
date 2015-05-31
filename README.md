# memo-ptr

This is a Haskell library that provides a memoizing combinator that is fully general and fast by relying on pointer equality. 

This means it is useful if you are calling the same function with the *same heap object*. 

One cache supports calling multiple functions so you do not need (but still can) keep track of which function uses which cache. 

## Example

```haskell
module Main where 

import Debug.Trace (trace)

import Memo

f :: Int -> Int
f x = trace ("f: " ++ show x) $ x + 1

two :: Int
two = 2 

main = do 
  c <- newCache
  print $ map (withCache c f) [two | _ <- [1..20]]
```

## Caveats/TODO

  * support curried (== normal) functions
  * smart LRU cache / weak pointers (currently we keep all history forever)

## How it works?
The cache records every function call in the internal `Map` storing the pointer to the function and to the argument plus the thunk with actual result.
Pointer means `GHC.Mem.StableName`. 
