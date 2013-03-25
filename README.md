# ReprTree
A simple library which provides a convenient way to inspect and debug arbitrary data structures.

Following is an example of an output this library produces.

    :
    - A
    | - :
    | | - a
    | | - b
    | | - c
    | - 9
    - C
    | - 3
    - B
    - D
      - :
        - :
        | - asdf
        | - 123
        | - ldskfjkl
        - :
          - f

Which is a result of running the following code:

```haskell
import Data.Generics (Data, Typeable)

data SomeType = 
  A [String] Int | 
  B | 
  C Int | 
  D [[String]]
  deriving (Typeable, Data)

xxx = A ["a", "b", "c"] 9 
    : C 3 
    : B 
    : D [["asdf", "123", "ldskfjkl"], ["f"]]
    : []

main = putStrLn $ reprTreeString xxx
```
