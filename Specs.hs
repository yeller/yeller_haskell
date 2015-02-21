{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Network.Yeller
import qualified Data.Text as T

main :: IO ()
main = hspec $ describe "Yeller" $
  describe "parsing stacktraces" $ do
    it "parses a simple line" $ do
      parseStackLine "Main.foo15 (Main.hs:48:1-20)" `shouldBe` StackFrame { stackFilename = "Main.hs", stackLineNumber = "48:1-20", stackFunction = "Main.foo15", stackOptions = StackOptions { stackOptionsInApp = True }}

    it "parses a line with more complicated options" $
      parseStackLine "Network.Yeller.sendError (Network/Yeller.hs:(85,1)-(89,64))" `shouldBe` StackFrame { stackFilename = "Network/Yeller.hs", stackLineNumber = "(85,1)-(89,64)", stackFunction = "Network.Yeller.sendError", stackOptions = StackOptions { stackOptionsInApp = True }}

    it "parses a line with no location info" $
      parseStackLine "Main.CAF:main5 (<no location info>)" `shouldBe` StackFrame { stackFilename = "", stackLineNumber = "", stackFunction = "Main.CAF:main5", stackOptions = StackOptions { stackOptionsInApp = True }}

    it "manages another line" $
      parseStackLine "Main.catchAny (Main.hs:51:1-34)" `shouldBe` StackFrame { stackFilename = "Main.hs", stackLineNumber = "51:1-34", stackFunction = "Main.catchAny", stackOptions = StackOptions { stackOptionsInApp = True }}

    it "manages another line" $
      parseStackLine "Main.main.\\ (Main.hs:56:40-117)" `shouldBe` StackFrame { stackFilename = "Main.hs", stackLineNumber = "56:40-117", stackFunction = "Main.main.\\", stackOptions = StackOptions { stackOptionsInApp = True }}
