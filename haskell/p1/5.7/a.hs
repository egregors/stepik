import Control.Monad.Writer ( runWriter, Writer )

evalWriter :: Writer w a -> a
evalWriter m = fst (runWriter m)