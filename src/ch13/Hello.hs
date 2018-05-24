module Ch13.Hello
  ( sayHello
  ) where

sayHello :: IO ()
sayHello = do
  putStrLn "hello world"
