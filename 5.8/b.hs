import Control.Monad.State
import Control.Monad.Writer (Writer, runWriter)

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = state $ \e -> (x, e `mappend` u)
  where
    (x, u) = runWriter m
