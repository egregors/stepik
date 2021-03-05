import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State (State, state)

readerToState :: Reader r a -> State r a
readerToState m = state $ \e -> (runReader m e, e)
