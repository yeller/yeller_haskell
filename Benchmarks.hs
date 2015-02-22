{-# LANGUAGE OverloadedStrings #-}
import qualified Network.Yeller.Internals as Yeller
import Criterion.Main
import Control.DeepSeq

instance NFData Yeller.StackOptions where
  rnf (Yeller.StackOptions inApp) = rnf inApp

instance NFData Yeller.StackFrame where
  rnf (Yeller.StackFrame file line function options) = rnf file `seq` rnf line `seq` rnf function `seq` rnf options

main :: IO ()
main = defaultMain [
    bgroup "parsing stacktraces" [
        bench "parsing a simple line" $ nf Yeller.parseStackLine "Main.foo15 (Main.hs:48:1-20)"
      , bench "parsing a line with more complicated options" $ nf Yeller.parseStackLine "Network.Yeller.sendError (Network/Yeller.hs:(85,1)-(89,64))"
      , bench "parsing a line with no location info" $ nf Yeller.parseStackLine "Main.CAF:main5 (<no location info>)"
      , bench "parsing another line" $ nf Yeller.parseStackLine "Main.catchAny (Main.hs:51:1-34)"
      , bench "parsing another line" $ nf Yeller.parseStackLine "Main.main.\\ (Main.hs:56:40-117)"
    ]
  ]
