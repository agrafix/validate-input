# validate-input

[![Build Status](https://travis-ci.org/agrafix/validate-input.svg)](https://travis-ci.org/agrafix/validate-input)

A small Haskell combinator library that provides a simple way of validating user provided data structures.

Hackage: [validate-input](http://hackage.haskell.org/package/validate-input) 

## Example

```haskell
data Example
   = Example
   { ex_username :: T.Text
   , ex_dogs :: Int
   } deriving (Show, Eq)
   
checkUsername =
    lengthBetween 3 12 "Should between 3 and 12 chars"
    >=> matchesRegex [re|^[A-za-z0-9]+$|] "only alpha num"
    
checkNumber =
    smallerThan 5 "No more than 5 dogs allowed"

checkExample e =
    Example <$> checkUsername (ex_username e)
            <*> checkNumber (ex_dogs e)

example = runValidator checkExample $ Example "alex" 23
```