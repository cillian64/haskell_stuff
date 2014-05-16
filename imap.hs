imap :: [(a -> b)] -> a -> [b]
imap [] _ = []
imap (f:fs) a = (f a) : (imap fs a)

