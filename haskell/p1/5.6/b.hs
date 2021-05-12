import Control.Monad.Reader (Reader, asks)

type User = String

type Password = String

type UsersTable = [(User, Password)]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = asks $ map fst . filter (\e -> "123456" == snd e)
