-- Определите функцию cmp, сравнивающую элементы типа LogLevel так, чтобы было верно, что Error > Warning > Info.

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
Error `cmp` Error = EQ
Warning `cmp` Warning = EQ
Info `cmp` Info = EQ

Error `cmp` Warning = GT
Warning `cmp` Info  = GT
Error `cmp` Info = GT

Info `cmp` Error = LT
Info `cmp` Warning = LT
Warning `cmp` Error = LT