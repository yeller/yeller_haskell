{-# LANGUAGE OverloadedStrings #-}
import Network.Yeller
import Control.Exception

foo1 :: [Integer]
foo1 = foo2

foo2 :: [Integer]
foo2 = foo3

foo3 :: [Integer]
foo3 = foo4

foo4 :: [Integer]
foo4 = foo5

foo5 :: [Integer]
foo5 = foo6

foo6 :: [Integer]
foo6 = foo7

foo7 :: [Integer]
foo7 = foo8

foo8 :: [Integer]
foo8 = foo9

foo9 :: [Integer]
foo9 = foo10

foo10 :: [Integer]
foo10 = foo11

foo11 :: [Integer]
foo11 = foo12

foo12 :: [Integer]
foo12 = foo13

foo13 :: [Integer]
foo13 = foo14

foo14 :: [Integer]
foo14 = foo15

foo15 :: [Integer]
foo15 = error "wops"

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

main :: IO ()
main = do
  c <- client (ApplicationEnvironment "production")
  _ <- catchAny (evaluate foo1) (\x -> print x >> sendError c x (ExtraErrorInfo Nothing Nothing Nothing) >> return [])
  putStrLn "done"
