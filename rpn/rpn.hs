-- The operating stack is a list of strings
-- Use `words' to convert an input string into a list of strings
-- and then reverse this to get the appropriate stack

main :: IO ()
main = do
    inputloop []

inputloop :: [String] -> IO ()
inputloop stack = do
    putStrLn $ foldl (\x y -> x ++ " " ++ y) ">" stack
    inputstr <- getLine
    let input = words inputstr
    let new_stack = apply input stack
    inputloop new_stack

-- Apply takes a list of input tokens, a stack, and returns the resulting stack
apply :: [String] -> [String] -> [String]
apply (x:xs) stack
    | isnum(x) = apply xs (x:stack)
    | isop(x) = apply xs (applyop x stack)
    | otherwise = ["invalid operator or input"]
apply [] stack = stack

-- Applyop takes an operator string, a stack, and returns the resulting stack
applyop :: String -> [String] -> [String]
applyop op (x1:x2:xs)
    | op == "+" = (show (a+b)):xs
    | op == "-" = (show (a-b)):xs
    | op == "*" = (show (a*b)):xs
    | op == "/" = (show (a/b)):xs
    | op == "clear" = []
    | op == "sqrt" = (show $ sqrt b):x2:xs
    where
        a = read x2 :: Float
        b = read x1 :: Float
applyop op [x]
    | op == "clear" = []
    | op == "sqrt" = [(show $ sqrt a)]
    | otherwise = ["error, stack underflow"]
    where a = read x :: Float
applyop op [] = ["error, stack underflow"]

isnum :: String -> Bool
isnum token = (token !! 0 >= '0') && (token !! 0 <= '9')

isop :: String -> Bool
isop token = (token == "+") || (token == "-") || (token == "*") || (token == "/") || (token == "clear") || (token == "sqrt")

