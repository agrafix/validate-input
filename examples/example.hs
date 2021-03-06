{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Data.Validator
import qualified Data.Text as T

data Example
   = Example
   { ex_username :: T.Text
   , ex_dogs :: Int
   , ex_friendName :: Maybe T.Text
   } deriving (Show)

data ExampleChecked
   = ExampleChecked
   { exc_username :: T.Text
   , exc_dogs :: Int
   , exc_friendName :: T.Text
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

checkExample :: Monad m => TransValidationRuleT String m Example ExampleChecked
checkExample e =
    ExampleChecked
    <$> checkUsername (ex_username e)
    <*> checkNumber (ex_dogs e)
    <*> requiredValue "You must provide a friend name!" (ex_friendName e)

example :: Either String ExampleChecked
example =
    runValidator checkExample $
    Example
    { ex_username = "alex"
    , ex_dogs = 23
    , ex_friendName = Nothing
    }
