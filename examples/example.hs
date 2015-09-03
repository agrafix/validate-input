{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Data.Validator
import qualified Data.Text as T

data Example
   = Example
   { ex_username :: T.Text
   , ex_dogs :: Int
   } deriving (Show)

main :: IO ()
main =
    do putStrLn "Check results:"
       print example

checkUsername :: Monad m => ValidationRuleT String m T.Text
checkUsername =
    lengthBetween 3 12 "Should between 3 and 12 chars"
    >=> matchesRegex [re|^[A-za-z0-9]+$|] "only alpha num"

checkNumber :: Monad m => ValidationRuleT String m Int
checkNumber =
    smallerThan 5 "No more than 5 dogs allowed"

checkExample :: Monad m => ValidationRuleT String m Example
checkExample e =
    Example <$> checkUsername (ex_username e)
            <*> checkNumber (ex_dogs e)

example :: Either String Example
example =
    runValidator checkExample $
    Example
    { ex_username = "alex"
    , ex_dogs = 23
    }
