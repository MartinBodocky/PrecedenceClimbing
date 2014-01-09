module PrecedenceClimbing where

import Data.Char
import Debug.Trace

{--  General Helpers  --}
isWhiteSpace :: Char -> Bool
isWhiteSpace x = (x == ' ') || (x == '\t') || (x == '\r')

{-- Start: TOKENIZER --}

-- Lexical analysis representation
data ParentThe = LeftOne | RightOne deriving(Show,Eq)
data OperatorSymbol = Mult | Divide | Plus | Minus | Pow  deriving(Show,Eq)
data Token = Constant Double | Operator OperatorSymbol | Parenthesis ParentThe | FinishToken deriving(Show,Eq)
type Equation = [Token]

-- syntax errors:
-- Operator + Operator (second - allowed)    ++
-- Operator + ParentThe RightOne             -)
-- ParentThe LeftOne + Operator (ex Minus)   (*
-- ParentThe RightOne + ParentThe LeftOne    ()

tokenizer :: String -> Maybe Token -> Equation
tokenizer [] Nothing = []
tokenizer [] (Just x) = [x]
tokenizer stream@(x:xs) Nothing
    | isDigit x =
        let (number, rest) = readNumber stream
            in tokenizer rest (Just(Constant number))
    | isOperator x = tokenizer xs (Just(nameOperator x))
    | isParentThe x = tokenizer xs (Just(nameParentThe x))
    | otherwise = error $ "tokenizer -> Syntax error: (Nothing) Unexpected symbol " ++ [x]

tokenizer stream@(x:xs) (Just all@(Constant c))
    -- Constant + Constant
    | isDigit x =
         let (number, rest) = readNumber stream
            in tokenizer rest (Just(Constant number))
    -- Constant + Operator
    | isOperator x = all : tokenizer xs (Just(nameOperator x))
    -- Constant + ParentThe
    | isParentThe x = all : tokenizer xs (Just(nameParentThe x))
    | otherwise = error $ "tokenizer -> Syntax error: (Constant) Unexpected symbol " ++ [x]

tokenizer stream@(x:xs) (Just all@(Operator c))
    -- Operator + Constant
    | isDigit x =
         let (number, rest) = readNumber stream
            in all : tokenizer rest (Just(Constant number))
    -- Operator + Operator, only if second one is minus
    | isOperator x =
        if x == operatorToChar Minus
            then all : tokenizer xs (Just (nameOperator x))
            else error  $ "tokenizer -> Syntax error: Two operator cannot follow " ++ show c ++ [x]
    -- Operator + ParentThe, if RightOne syntax error
    | isParentThe x =
        if x == parentThetoChar RightOne
            then error $ "tokenizer -> Syntax error: Operator cannot be followed by " ++ [x]
            else all : tokenizer xs (Just(nameParentThe x))
    | otherwise = error $ "tokenizer -> Syntax error: (Operator) Unexpected symbol " ++ [x]

tokenizer stream@(x:xs) (Just all@(Parenthesis c))
    -- ParentThe + Constant
    | isDigit x =
        let (number, rest) = readNumber stream
            in all : tokenizer rest (Just(Constant number))
    -- ParentThe + Operator, if LeftOne we can accept just negative operator
    | isOperator x =
        if c == LeftOne
            then
            if x == operatorToChar Minus
                then all : tokenizer xs (Just(nameOperator x)) -- LeftOne + Operator Minus
                else error $ "tokenizer -> Syntax error: After Left parentthesis cannot follow " ++ [x]
            else
                all : tokenizer xs (Just(nameOperator x))
    -- ParentThe + ParentThe, if not LeftOne and RightOne
    | isParentThe x =
        case (c, x) of
            (LeftOne, ')') ->
                 error $ "tokenizer -> Syntax error: After Left parentthesis cannot follow Right parentthesis " ++ [x]
            _-> all : tokenizer xs (Just (nameParentThe x))
    | otherwise = error $ "tokenizer -> Syntax error: (Parenthesis) Unexpected symbol " ++ [x]


-- Tokenizer's helper
readNumber :: String -> (Double, String)
readNumber str =
    let (number, rest) = span (\x -> isDigit x || isDot x) str
        in (read number :: Double, rest)

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/^="

nameOperator :: Char -> Token
nameOperator x
    | x == '*' = Operator Mult
    | x == '/' = Operator Divide
    | x == '+' = Operator Plus
    | x == '-' = Operator Minus
    | x == '^' = Operator Pow
    | otherwise = error $ "nameOperator -> Syntax error: Unexpected operator " ++ [x]

operatorToChar :: OperatorSymbol -> Char
operatorToChar x
    | x == Mult = '*'
    | x == Divide = '/'
    | x == Plus = '+'
    | x == Minus = '-'
    | x == Pow = '^'

isParentThe :: Char -> Bool
isParentThe x = x `elem` "()"

nameParentThe :: Char -> Token
nameParentThe x
    | x == '(' = Parenthesis LeftOne
    | x == ')' = Parenthesis RightOne
    | otherwise = error $ "nameParentThe -> Syntax error: Unexpected operator " ++ [x]

parentThetoChar :: ParentThe -> Char
parentThetoChar x
    | x == LeftOne = '('
    | x == RightOne = ')'

isDot :: Char -> Bool
isDot x = (x =='.')

lookAhead :: [Token] -> Token
lookAhead [] = FinishToken
lookAhead (t:ts) = t

correcting :: Equation -> Equation
correcting [] = []
correcting (x:xs)
    -- when is double minus it coverts it for plus
    | x == (Operator Minus) && lookAhead xs == (Operator Minus) = (Operator Plus) : correcting (tail xs)
    | otherwise = x : correcting xs

{-- End TOKENIZER --}

{-- Start:  Precedence Climbing --}

{--
Parser Grammar
Exercise <- Expr '=' Expr
Expr     <- Atom { Actions Expr}
Actions  <- "+" | "-" | "*" | "/" | "^"
Atom     <- Number | [+-sqrt] Atom | '(' Expr ')'
--}

data Expression =
              SumNode OperatorSymbol Expression Expression
            | ProdNode OperatorSymbol Expression Expression
            | AssignNode Expression Expression
            | PowNode Expression Expression
            | UnaryNode OperatorSymbol Expression
            | NumNode Double
            | VarNode String
            deriving (Show)

data Assoc = RightAssc | LeftAssc deriving(Show, Eq)

-- precedence tables
operatorPrecedence :: OperatorSymbol -> Int
operatorPrecedence x
    | x == Plus = 2
    | x == Minus = 2
    | x == Mult = 3
    | x == Divide = 3
    | x == Pow = 4
    | otherwise = error $ "operatorPrecedence -> I cannot find precendence for: "++ show x

operatorAssociativity :: OperatorSymbol -> Assoc
operatorAssociativity  x
    | x == Plus = LeftAssc
    | x == Minus = LeftAssc
    | x == Mult = LeftAssc
    | x == Divide = LeftAssc
    | x == Pow = RightAssc
    | otherwise = error $ "operatorAssociativity -> I cannot find associativity for: "++ show x

precedenceClimbing :: [Token] -> Expression
precedenceClimbing tokens =
    let (expr, rest) = makeExpression tokens 0
        in expr
-- Expr     <- Atom { Actions Expr}
makeExpression :: [Token] -> Int -> (Expression, [Token])
makeExpression all@(t:ts) prec =
    let (current, rest) = parserExpression all
        in
        if length rest == 0
            then (current, rest)
            else
                parseOperator rest prec current
-- Actions  <- "+" | "-" | "*" | "/" | "^"
makeBranch :: OperatorSymbol -> Expression -> Expression -> Expression
makeBranch op current rhs =
    case op of
        (Plus) -> SumNode op current rhs
        (Minus) -> SumNode op current rhs
        (Mult) -> ProdNode op current rhs
        (Divide) -> ProdNode op current rhs
        (Pow) -> PowNode current rhs
        --_-> error $ "makeBranch -> We cannot parse: "++ show op ++ " current: "++ show current

parseOperator :: [Token] -> Int -> Expression -> (Expression, [Token])
parseOperator [] _ x = (x,[])
parseOperator all@(t:ts) currentPrec current  =
    case t of
        (Operator op) ->
            let opPrec = operatorPrecedence op
                assoc = operatorAssociativity op
                in
                if opPrec >= currentPrec
                    then
                    let newPrec = if assoc == RightAssc then opPrec else opPrec+1
                        (rhs,rest) = makeExpression ts newPrec
                        branch = makeBranch op current rhs
                        in
                            parseOperator rest currentPrec branch
                    else
                        (current, all)
        (Parenthesis RightOne) -> (current, all)
        _-> if length ts == 0 then (current, []) else error $ "parseOperator -> We didn't expect anything else: "++show all

-- Atom     <- Number | [+-sqrt] Atom | '(' Expr ')'
parserExpression :: [Token] -> (Expression, [Token])
parserExpression all@(t:ts) =
    case t of
	(Parenthesis LeftOne) ->
	   let (expr, rest) = makeExpression ts 0
		in
                case lookAheadToken rest of
	            (Parenthesis RightOne) -> (expr, restTokens rest)
                    _-> error $ "parserExpression -> We are missing right parenthesis... "++ show rest
        (Constant x) ->
                ((NumNode x), ts)
        (Operator x) ->
            case x  of
                (Minus) ->
                    case lookAheadToken ts of
                        (Constant x) -> ((NumNode (-x)), restTokens ts)
                        _-> error $ "parserExpression -> We cannot parse: "++ show all
                (Plus) ->
                    case lookAheadToken ts of
                        (Constant x) -> ((NumNode (x)), restTokens ts)
                        _-> error $ "parserExpression -> We cannot parse: "++ show all
                _-> error $ "parserExpression -> We canot parse: "++ show all
        _-> error $ "parserExpression -> We canoot parse: "++ show all

-- precedence climbing's helpers
lookAheadToken :: [Token] -> Token
lookAheadToken [] = error "lookAheadToken -> No more tokens..."
lookAheadToken (t:ts) = t

restTokens :: [Token] -> [Token]
restTokens [] = error "restTokens -> We expect rest tokens"
restTokens (x:xs) = xs

{-- Evaluator --}

evaluate :: Expression -> Double
evaluate (AssignNode left right) =
    let l = evaluate left
        r = evaluate right
        in l + r
evaluate (VarNode c) = 0
evaluate (NumNode x) = x
evaluate (SumNode op left right) =
    let l = evaluate left
        r = evaluate right
        in
        case op of
            Plus -> l + r
            Minus -> l - r
evaluate (ProdNode op left right) =
    let l = evaluate left
        r = evaluate right
        in
        case op of
            Mult -> l * r
            Divide -> l / r
            Pow -> l ^ (round r)
evaluate (PowNode left right) =
    let l = evaluate left
        r = evaluate right
        in l ^ (round r)
evaluate (UnaryNode op expr) =
    let val = evaluate expr
        in
        case op of
            Plus -> val
            Minus -> -val

-- compute, glue all together
compute :: String -> Double
compute str = (evaluate . precedenceClimbing . correcting ) $ tokenizer (filter (\x-> not $ isWhiteSpace x ) str) Nothing



























