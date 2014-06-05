type Stack = [Double]
type Token = String

main :: IO ()
main = do
    putStrLn "RPN Calculator by David Turner in 2014"
    putStrLn "Enter `help' for operators and commands."
    putStrLn ""
    inputloop []

-- inputloop takes the current state stack, does input, and recurses.
inputloop :: Stack -> IO ()
inputloop stack = do
    putStrLn $ foldl (\x y -> x ++ " " ++ y) ">" (map show stack)
    inputstr <- getLine
    if inputstr == "exit" || inputstr == "quit" then return ()
    else if inputstr == "help" then do
        putStrLn "Operators: +, -, *, /, ^, sqrt, pop, clear"
        putStrLn "Commands: quit, exit, ops"
        inputloop stack
    else -- Update stack and recurse:
        inputloop $ foldl apply stack (words inputstr)

-- apply applies a token to a stack
apply :: Stack -> Token -> Stack
apply stack x
    | isnum(x) = (read x):stack -- infers the type of `read' from `stack'
    | otherwise = applyop x stack

-- applyop applies an operator token to a stack
applyop :: Token -> Stack -> Stack
applyop "+" (b:a:xs) = (a+b):xs
applyop "-" (b:a:xs) = (a-b):xs
applyop "*" (b:a:xs) = (a*b):xs
applyop "/" (b:a:xs) = (a/b):xs
applyop "^" (b:a:xs) = (a**b):xs
applyop "sqrt" (a:xs) = (sqrt a):xs
applyop "pop" (a:xs) = xs
applyop "clear" _ = []
applyop _ xs = xs -- Fallthrough - do nothing. Includes empty stack with valid op

-- Isnum tells us whether the token is a valid number
isnum :: Token -> Bool
isnum token = case reads token :: [(Double, String)] of
    [(_, "")] -> True
    _         -> False

