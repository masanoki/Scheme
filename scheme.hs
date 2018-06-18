import System.IO
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Error
import Control.Monad.Except
import Data.Char

data ExpAll = Num Int
            | Num1 Int
            | Bool Bool
            | String String
            | Id String
            | Idvalue String
            | Emp
            | List [ExpAll]
            | ShowList [ExpAll]
            | Dot [ExpAll] ExpAll
            | IdList [ExpAll]
            | ExpList [ExpAll]
            | Def (ExpAll, ExpAll)
            | DefMacro (ExpAll, ExpAll, ExpAll)
            | Set (ExpAll, ExpAll, ExpAll)
--          | Setlist (ExpAll, ExpAll, ExpAll)
            | ApplyList [ExpAll]
            | Ld String
            | Funcoriginal [ExpAll]
            | Function [ExpAll] deriving Show -- needn't Show

original :: [(String, ExpAll)]
original = 
  [("lambda", Id "<syntax lambda>" ),
   ("quote", Id "<syntax quote>"),
   ("set!", Id "<syntax set!>"),
   ("let", Id "<syntax let>"),
   ("let*", Id "<syntax let*>"),
   ("letrec", Id "<syntax letrec>"),
   ("if", Id "<syntax if>"),
   ("cond", Id "<syntax cond>"),
   ("and", Id "<syntax and>"),
   ("or", Id "<syntax or>"),
   ("begin", Id "<syntax begin>"),
   ("do", Id "<syntax do>"),
   ("else", Id "<syntax else>"),
   ("exit", String "exit")]

func :: [(String, ExpAll)]
func = 
  [("number?", Funcoriginal [Num1 1, String "number?"]),
   ("+", Funcoriginal [String "rest", String "+"]),
   ("sum", Funcoriginal [String "rest", String "+"]),
   ("-", Funcoriginal [String "rest", Num1 1, String "-"]),
   ("*", Funcoriginal [String "rest", String "*"]),
   ("product", Funcoriginal [String "rest", String "*"]),
   ("/", Funcoriginal [String "rest", Num1 1, String "/"]),
   ("=", Funcoriginal [String "rest", Num1 2, String "="]),
   ("<=", Funcoriginal [String "rest", Num1 2, String "<="]),
   ("<", Funcoriginal [String "rest", Num1 2, String "<"]),
   (">=", Funcoriginal [String "rest", Num1 2, String ">="]),
   (">", Funcoriginal [String "rest", Num1 2, String ">"]),
   ("null?", Funcoriginal [Num1 1, String "null?"]),
   ("pair?", Funcoriginal [Num1 1, String "pair?"]),
   ("list?", Funcoriginal [Num1 1, String "list?"]),
   ("symbol?", Funcoriginal [Num1 1, String "symbol?"]),
   ("car", Funcoriginal [Num1 1, String "car"]),
   ("head", Funcoriginal [Num1 1, String "car"]),
   ("cdr", Funcoriginal [Num1 1, String "cdr"]),
   ("tail", Funcoriginal [Num1 1, String "cdr"]),
   ("cons", Funcoriginal [Num1 2, String "cons"]),
   ("list", Funcoriginal [String "rest", String "list"]),
   ("length", Funcoriginal [Num1 1, String "length"]),
   ("memq", Funcoriginal [Num1 2, String "memq"]),
   ("last", Funcoriginal [Num1 1, String "last"]),
   ("append", Funcoriginal [String "rest", String "append"]),
   ("++", Funcoriginal [String "rest", String "append"]),
--   ("set-car!", Funcoriginal [Num1 2, String "set-car!"]),
--   ("set-cdr!", Funcoriginal [Num1 2, String "set-cdr!"]),
   ("boolean?", Funcoriginal [Num1 1, String "boolean?"]),
   ("not", Funcoriginal [Num1 1, String "not"]),
   ("string?", Funcoriginal [Num1 1, String "string?"]),
   ("string-append", Funcoriginal [String "rest", String "string-append"]),
   ("symbol->string", Funcoriginal [Num1 1, String "symbol->string"]),
   ("string->symbol", Funcoriginal [Num1 1, String "string->symbol"]),
   ("string->number", Funcoriginal [Num1 1, Num1 2, String "string->number"]),
   ("number->string", Funcoriginal [Num1 1, Num1 2, String "number->string"]),
   ("procedure?", Funcoriginal [Num1 1, String "procedure?"]),
   ("eq?", Funcoriginal [Num1 2, String "eq?"]),
   ("neq?", Funcoriginal [Num1 2, String "neq?"]),
   ("equal?", Funcoriginal [Num1 2, String "equal?"])
  ]

---------Parser---------

idparts = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "!$%&*+-./<=>?@^_"
idparts1 = ['A'..'Z'] ++ ['a'..'z'] ++ "!$%&*+-/<=>?@^_"

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseConst :: Parser ExpAll
parseConst = parseNum
             <|> parseBool
             <|> parseString
             <|> parseEmp

parseNum :: Parser ExpAll
parseNum = many1 digit >>= return . Num . read

parseBool :: Parser ExpAll
parseBool = do
  char '#'
  bool <- oneOf "tf"
  if bool == 't'
    then return $ Bool True
    else return $ Bool False

parseString :: Parser ExpAll
parseString = do
  char '"'
  str <- many $ try (string "\\\"") <|> many1 (char '\\') <|> many1 (noneOf "\\\"")
  char '"'
  return $ String $ concat str

parseEmp :: Parser ExpAll
parseEmp = do
  char '('
  spaces
  char ')'
  return Emp

parseId :: Parser ExpAll
parseId = try (do
  num <- many1 $ digit
  chr <- oneOf idparts1
  id <- many $ oneOf idparts
  return $ Id $ num ++ [chr] ++ id )
  <|> do
    num <- many $ digit
    dot <- char '.'
    num1 <- many $ digit
    id <- many1 $ oneOf idparts
    return $ Id $ num ++ "." ++ num1 ++ id
  <|> do
    x <- oneOf idparts1
    xs <- many $ oneOf idparts
    return $ Id $ x:xs 

parseApply :: [[(String, ExpAll)]] -> Parser ExpAll
parseApply env = do
  char '('
  spaces
  cstr <- parseExpA env
  spaces
  exp <- many $ try $ spaces >> parseExp env
  spaces
  char ')'
  return $ ApplyList $ cstr:exp

parselookIdsub :: [[(String, ExpAll)]] -> Parser String
parselookIdsub ([]:[]) = do
  Id id <- parseId
  return id
parselookIdsub (((str, exp):xs):[]) = do {try (string str >> spaces1); parserZero} <|> parselookIdsub (xs:[])
parselookIdsub ([]:(xs:xss)) = parselookIdsub (xs:xss)
parselookIdsub (((str, exp):xs):xss) =  try (string str) <|> parselookIdsub (xs:xss)

parselookId :: [[(String, ExpAll)]] -> Parser ExpAll
parselookId env = do
  str <- parselookIdsub env
  return $ Idvalue str

parseIdvalue :: [[(String, ExpAll)]] -> Parser ExpAll
parseIdvalue env = do
  Id id <- parseId
  return $ Idvalue id

parseSet :: [[(String, ExpAll)]] -> Parser ExpAll
parseSet env = do
  char '('
  spaces
  string "set!"
  spaces1
  id <- parseId
  spaces
  exp <- parseExp env
  spaces
  char ')'
  return $ Set (id, exp, Num1 (length env))

{-
parseSetlist :: [[(String, ExpAll)]] -> Parser ExpAll
parseSetlist env = do
  char '('
  spaces
  str <- try (string "set-car!") <|> string "set-cdr!"
  spaces1
  id <- parseId
  spaces
  exp <- parseExp env
  spaces
  char ')'
  return $ Setlist (str, id, exp)
  -}

parseLet :: [[(String, ExpAll)]] -> Parser ExpAll
parseLet env = do
  char '('
  spaces
  hd <- string "let"
  spaces1
  id <- parseId
  spaces
  bind <- try (parseBindList env) <|> parseEmp
  spaces
  def <- many $ try $ spaces >> parseDefine (bindEnv ([]:env) bind)
  spaces
  exp <- many1 $ try $ spaces >> parseExp (defEnv (bindEnv ([]:env) bind) def)
  spaces
  char ')'
  return $ IdList (Id hd:[id]++[bind]++def++exp)

parseLet1 :: [[(String, ExpAll)]] -> Parser ExpAll
parseLet1 env = do
  char '('
  spaces
  hd <- try (string "letrec") <|> try (string "let*") <|> try (string "letrec") <|> string "let"
  spaces
  bind <- try (parseBindList env) <|> parseEmp
  spaces
  def <- many $ try $ spaces >> parseDefine (bindEnv ([]:env) bind)
  spaces
  exp <- many1 $ try $ spaces >> parseExp (defEnv (bindEnv ([]:env) bind) def)
  spaces
  char ')'
  return $ IdList (Id hd:[bind]++def++exp)

{-
parseLet2 :: [[(String, ExpAll)]] -> Parser ExpAll
parseLet2 env = do
  char '('
  spaces
  hd <- try (string "let*") <|> string "letrec"
  spaces
  bind <- try (parseBindList ([]:env)) <|> parseEmp
  spaces
  def <- many $ try $ spaces >> parseDefine (bindEnv ([]:env) bind)
  spaces
  exp <- many1 $ try $ spaces >> parseExp (defEnv (bindEnv ([]:env) bind) def)
  spaces
  char ')'
  return $ IdList (Id hd:[bind]++def++exp)
 -}

parseCond :: [[(String, ExpAll)]] -> Parser ExpAll
parseCond env = do
  char '('
  spaces
  hd <- string "cond"
  spaces
  exp <- many $ try $ spaces >> parseCondsub env
  spaces
  char ')'
  return $ IdList (Id hd:exp)

parseCondsub :: [[(String, ExpAll)]] -> Parser ExpAll
parseCondsub env = do
  char '('
  spaces
  hexp <- parseExp env
  spaces
  exp <- many1 $ try $ spaces >> parseExp env
  spaces
  char ')'
  return $ IdList (hexp:exp)

parseIdList :: [[(String, ExpAll)]] -> Parser ExpAll
parseIdList env = do
  char '('
  spaces
  hd <- string "if" <|> string "and" <|> string "or" <|> string "begin"
  spaces
  ret <- many $ try $ spaces >> parseExp env
  spaces
  char ')'
  return $ IdList $ Id hd:ret

parseLambda :: [[(String, ExpAll)]] -> Parser ExpAll
parseLambda env = do
  char '('
  spaces
  id <- string "lambda"
  arg <- parseArg
  spaces
  def <- many $ try $ spaces >> parseDefine (argEnv ([]:env) arg)
  spaces
  exp <- many1 $ try $ spaces >> parseExp (defEnv (argEnv ([]:[]:env) arg) def)
  spaces
  char ')'
  return $ IdList $ Id id:arg:def++exp

parseDo :: [[(String, ExpAll)]] -> Parser ExpAll
parseDo env = do
  char '('
  spaces
  id <- string "do"
  spaces
  char '('
  bind <- many $ try $ spaces >> parseBindDo env
  char ')'
  spaces
  char '('
  judge <- many1 $ try $ spaces >> parseExp (bindDoEnv env bind)
  spaces
  char ')'
  spaces
  def <- many $ try $ spaces >> parseDefine (bindDoEnv env bind)
  spaces
  exp <- many $ try $ spaces >> parseExp (defEnv (bindDoEnv env bind) def)
  spaces
  char ')'
  return $ IdList $ Id id:IdList bind:IdList judge:def++exp

parseBindDo :: [[(String, ExpAll)]] -> Parser ExpAll
parseBindDo env = do
  char '('
  spaces
  id <- parseId
  spaces
  init <- parseExp env
  spaces
  update <- parseExp env
  spaces
  char ')'
  return $ IdList [id, init, update]

parseArg :: Parser ExpAll
parseArg = try (do
  spaces
  char '('
  spaces
  do {char ')'; return Emp} <|> do
    fix <- many1 $ try $ spaces >> parseId
    try (spaces >> char ')' >> return (List fix)) <|> do
      spaces1
      char '.'
      spaces1
      lst <- parseId
      spaces
      char ')'
      return (Dot fix lst))
    <|> do
      spaces1
      id <- parseId
      return id

argEnv :: [[(String, ExpAll)]] -> ExpAll -> [[(String, ExpAll)]]
argEnv env (Emp) = env
argEnv env (Id id) = localset (id, Emp) env
argEnv env (List []) = env
argEnv env (List (Id id:xs)) = localset (id, Emp) (argEnv env (List xs))
argEnv env (Dot [] (Id id)) = localset (id, Emp) env
argEnv env (Dot (Id id:xs) y) = localset (id, Emp) (argEnv env (Dot xs y))

parseS_Exp :: Parser ExpAll
parseS_Exp = try parseConst
             <|> do
               char '('
               left <- many1 $ try $ spaces >> parseS_Exp
               try (spaces >> char ')' >> return (List left)) <|> do
                 spaces1
                 char '.'
                 spaces1
                 right <- parseS_Exp
                 spaces
                 char ')'
                 return $ Dot left right
             <|> parseId
             <|> do
               char '\''
               right <- parseS_Exp
               return $ Dot [Id "'"] right

parseS :: Parser ExpAll
parseS = do { char '\''; spaces; ret <- parseS_Exp; return ret } <|> do
  char '('
  spaces
  string "quote"
  spaces1
  ret <- parseS_Exp
  spaces
  char ')'
  return ret

parseExpm :: [[(String, ExpAll)]] -> Parser ExpAll
parseExpm env = try (parseApply env)
                <|> try (parseLambda env)
                <|> try parseS
                <|> try (parseSet env)
                <|> try (parseLet env)
--                <|> try (parseLet2 env)
                <|> try (parseLet1 env)
                <|> try (parseCond env)
                <|> try (parseDo env)
                <|> try (parseIdList env)

parseExp :: [[(String, ExpAll)]] -> Parser ExpAll
parseExp env = try parseConst
             <|> parseIdvalue env
             <|> parseExpm env

parseExpA ::[[(String, ExpAll)]] -> Parser ExpAll
parseExpA env = try parseConst
                <|> parselookId env
                <|> parseExpm env

parseDefine :: [[(String, ExpAll)]] -> Parser ExpAll
parseDefine env = do
  char '('
  spaces
  string "define"
  try (do
    spaces
    char '('
    spaces
    cstr <- parseId
    idlist <- many $ try $ spaces >> parseId
    try (do
      spaces
      char ')'
      spaces
      def <- many $ try $ spaces >> parseDefine (cstrEnv ([]:env) idlist)
      spaces
      exp <- many1 $ try $ spaces >> parseExp (defEnv (cstrEnv ([]:env) idlist) def)
      char ')'
      return $ Def (cstr, (IdList (Id "lambda":List idlist:def++exp)))) <|> do
        spaces1
        char '.'
        spaces1
        Id lst <- parseId
        spaces
        char ')'
        spaces
        defl <- many $ try $ spaces >> parseDefine (localset (lst, Emp) (cstrEnv ([]:env) idlist))
        spaces
        expl <- many1 $ try $ spaces >> parseExp (defEnv (localset (lst, Emp) (cstrEnv ([]:env) idlist)) defl)
        return $ Def (cstr, (IdList (Id "lambda":Dot idlist (Id lst):defl++expl)))) <|> do
  spaces1
  id <- parseId
  spaces
  exp <- parseExp ([]:env)
  spaces
  char ')'
  return $ Def (id, exp)

parseBindList :: [[(String, ExpAll)]] -> Parser ExpAll
parseBindList env = do
  char '('
  bind <- many $ spaces >> parseBind env
  spaces
  char ')'
  return $ IdList bind

parseBind :: [[(String, ExpAll)]] -> Parser ExpAll
parseBind env = do
  char '('
  spaces
  id <- parseId
  spaces
  exp <- parseExp env
  spaces
  char ')'
  return $ IdList $ id:[exp]

cstrEnv :: [[(String, ExpAll)]] -> [ExpAll] -> [[(String, ExpAll)]]
cstrEnv env [] = env
cstrEnv env ((Id id):xs) = localset (id, Emp) (cstrEnv env xs)

bindEnv :: [[(String, ExpAll)]] -> ExpAll -> [[(String, ExpAll)]]
bindEnv env (IdList ((IdList (Id id:_)):ys)) = localset (id, Emp) (bindEnv env (IdList ys))
bindEnv env (IdList ys) = env

bindDoEnv :: [[(String, ExpAll)]] -> [ExpAll] -> [[(String, ExpAll)]]
bindDoEnv env (IdList (Id id:xs):ys) = localset (id, Emp) (bindDoEnv env ys)
bindDoEnv env [] = env

defEnv :: [[(String, ExpAll)]] -> [ExpAll] -> [[(String, ExpAll)]]
defEnv env (Def (Id id, _):xs) = localset (id, Emp) (defEnv env xs)
defEnv env [] = env

parseLoad :: [[(String, ExpAll)]] -> Parser ExpAll
parseLoad env = do
  char '('
  spaces
  hd <- string "load"
  spaces
  String str <- parseString
  spaces
  char ')'
  return $ Ld str

parseDefMacro :: [[(String, ExpAll)]] -> Parser ExpAll
parseDefMacro env = do
  char '('
  spaces
  string "define-macro"
  spaces
  char '('
  id <- parseId
  spaces
  idlst <- many $ try $ spaces >> parseId
  try (do
    spaces
    char ')'
    spaces
    exp <- parseExp env
    spaces
    char ')'
    return $ DefMacro (id, List idlst, exp))
    <|> do
      spaces1
      char '.'
      spaces1
      right <- parseId
      spaces
      char ')'
      spaces
      exp <- parseExp env
      spaces
      char ')'
      return $ DefMacro (id, Dot idlst right, exp)

parseTop :: [[(String, ExpAll)]] -> Parser ExpAll
parseTop env = try (parseDefMacro env)
               <|> try (parseDefine env)
               <|> try (parseLoad env)
               <|> parseExp env

---------calculate----------

unpackBool :: ExpAll -> Bool
unpackBool (Bool False) = False
unpackBool _ = True

unpackId :: ExpAll -> String
unpackId (Id id) = id

lookId :: [[(String, ExpAll)]] -> String -> ExpAll
lookId [[]] id = String $ "not found " ++ id
lookId ([]:ys) id = lookId ys id
lookId (((str, exp):xs):ys) id =
  if str == id
    then exp
    else lookId (xs:ys) id

lookLocal :: [[(String, ExpAll)]] -> (String, ExpAll) -> [[(String, ExpAll)]]
lookLocal ([]:ys) (id, exp) = [(id, exp)]:ys
lookLocal (((str, befexp):xs):ys) (id, exp) =
  if str == id
    then (((id, exp):xs):ys)
    else localset (str, befexp) (lookLocal (xs:ys) (id, exp))

rewriteenv :: [[(String, ExpAll)]] -> (String, ExpAll) -> ([[(String, ExpAll)]], Bool)
rewriteenv ([]:[]) _ = (([]:[]), False) --output error!
rewriteenv ([]:ys@(_:_)) (id, exp) = ([]:env, bool)
  where (env, bool) = rewriteenv ys (id, exp)
rewriteenv (((str, befexp):xs):ys) (id, exp) =
  if str == id
    then ((((id, exp):xs):ys), True)
    else rewriteset (str, befexp) (rewriteenv (xs:ys) (id, exp))

rewriteset :: a -> ([[a]], b) -> ([[a]], b)
rewriteset x (xss, bool) = ((localset x xss), bool)

localset :: a -> [[a]] -> [[a]]
localset x xss@(xs:ys) = (x:xs):ys

calculate :: [[(String, ExpAll)]] -> ExpAll -> (ExpAll, [[(String, ExpAll)]])
calculate env (IdList (Id x:xs)) = case x of
  "lambda" -> (Function xs, env)
  "let" -> calcLet ([]:env) $ IdList xs
  "let*" -> calcLetstar ([]:env) $ IdList xs
  "letrec" -> calcLetrec ([]:env) $ IdList xs
  "if" -> calcIf env $ IdList xs
  "cond" -> calcCond env $ IdList xs
  "and" -> calcAnd env $ IdList xs
  "or" -> calcOr env $ IdList xs
  "begin" -> calcBegin env $ ExpList xs
  "do" -> calcDo ([]:env) $ IdList xs
calculate env (Idvalue id) = (lookId env id, env)
calculate env (ExpList (x:[])) = calculate env x
calculate env (ExpList (x:xs@(_:_))) =
  calculate afenv $ ExpList xs
  where (_, afenv) = calculate env x
calculate env (ApplyList (Function xs:ys)) = calcApply env (ApplyList (Function xs:ys))
calculate env (ApplyList xs@(DefMacro (_, _, _):ys)) = calcMacro env xs
calculate env (ApplyList ((Num _):_)) = (String "error", env)
calculate env (ApplyList ((Bool _):_)) = (String "error", env)
calculate env (ApplyList ((String _):_)) = (String "error", env)
calculate env (ApplyList (Funcoriginal (Num1 n:Num1 n1:xs@(_:[])):ys)) =
  if length ys == n || length ys == n1
    then calcfunc env (Funcoriginal xs:ys)
    else (String "error", env)
calculate env (ApplyList (Funcoriginal (String "rest":Num1 n:xs):ys)) =
  if length ys >= n
    then calcfunc env (Funcoriginal xs:ys)
    else (String "error", env)
calculate env (ApplyList (Funcoriginal (String "rest":xs@(_:[])):ys)) = calcfunc env (Funcoriginal xs:ys)
calculate env (ApplyList (Funcoriginal (Num1 n:xs@(_:[])):ys)) =
  if length ys == n
    then calcfunc env (Funcoriginal xs:ys)
    else (String "error", env)
calculate env (ApplyList (x:xs)) =
  calculate afenv (ApplyList (afexp:xs))
  where (afexp, afenv) = calculate env x
calculate env (Def (Id id, exp)) = 
  calcDef afenv (id, afexp)
  where (afexp, afenv) = calculate env exp
calculate env (Set (Id id, exp, n)) =
  calcSet afenv (id, afexp, n)
  where (afexp, afenv) = calculate env exp
  {-
calculate env (Set (String str,Id id,exp)) =
  if str == "set-car!"
    then calcSet afenv (Dot afexp cdrret 
    else calcSet afenv (
  where (afexp, afenv) = calculate env exp
  -}
calculate env (Dot xs y) = (calcDotList (Dot xs y) , env)
calculate env (List xs) = (calcDotList (List xs), env)
calculate env x@(DefMacro (Id id, _, _)) = calcDefMacro env x
calculate env other = (other, env)

calcDefMacro env (DefMacro (Id id,List xs, y)) =
  calcDef afenv (id, DefMacro (Id id,List xs, afexp))
  where (afexp, afenv) = calculate env (valtoid xs y)
calcDefMacro env (DefMacro (Id id, Dot xs z, y)) =
  calcDefMacro env (DefMacro (Id id, List (xs++[z]), y))

calcMacro :: [[(String, ExpAll)]] -> [ExpAll] -> (ExpAll, [[(String, ExpAll)]])
calcMacro env (DefMacro (_,List idlst, xs):ys) =
  if length idlst == length ys
    then calculate env $ convertid  (mcr idlst (convertList xs) ys)
    else (String "error", env)
calcMacro env (DefMacro (_, Dot idlst z, xs):ys) =
  if length idlst <= length ys
    then calculate env $ convertid (mcr (idlst++[z]) (convertList xs) ys)
    else (String "error", env)

convertList :: ExpAll -> ExpAll
convertList (List []) = List []
convertList (List (Id id:xs))
  | id == "lambda" || id == "let" || id == "let*" || id == "letrec"
    || id == "if" || id == "cond" || id == "and" || id == "or"
    || id == "begin" || id == "do"
    = IdList (Id id:convertListsub xs)
  | id == "set!" =
    if length xs == 2
      then Set (convertList (head xs) ,convertList (head (tail xs)) , Num 0)
      else List (Id id:xs)
  | otherwise = ApplyList (Id id:convertListsub xs)
convertList x = x

convertListsub [] = []
convertListsub (x:xs) = convertList x:convertListsub xs

convertid :: ExpAll -> ExpAll
convertid (Id id) = Idvalue id
convertid (ApplyList xs) = ApplyList (convertidsub xs)
convertid (IdList xs) = IdList (convertidsub xs)
convertid (Function xs) = Function (convertidsub xs)
convertid x = x

convertidsub :: [ExpAll] -> [ExpAll]
convertidsub [] = []
convertidsub (x:xs) = convertid x : convertidsub xs

valuetoid :: String -> [ExpAll] -> ExpAll -> [ExpAll]
valuetoid _ [] _ = []
valuetoid id (z@(Idvalue y):ys) exp =
  if id == y
    then exp:valuetoid id ys exp
    else z:valuetoid id ys exp
valuetoid id (IdList xs:ys) exp = IdList (valuetoid id xs exp): valuetoid id ys exp
valuetoid id (ApplyList xs:ys) exp = ApplyList (valuetoid id xs exp):valuetoid id ys exp
valuetoid id (Function xs:ys) exp = Function (valuetoid id xs exp):valuetoid id ys exp
valuetoid id (x:xs) exp = x:valuetoid id xs exp

valuetoidsub :: [ExpAll] -> ExpAll -> ExpAll -> ExpAll
valuetoidsub [] y _ = y
valuetoidsub (Id id:xs) (ApplyList ys) exp = valuetoidsub xs (ApplyList (valuetoid id ys exp)) exp
valuetoidsub (Id id:xs) (IdList ys) exp = valuetoidsub xs (IdList (valuetoid id ys exp)) exp
valuetoidsub (Id id:xs) (Function ys) exp = valuetoidsub xs (Function (valuetoid id ys exp)) exp
valuetoidsub (Id id1:xs) z@(Idvalue id) exp =
  if id1 == id
    then exp
    else valuetoidsub xs z exp
valuetoidsub (x:xs) y exp = valuetoidsub xs y exp

valtoid [] y = y
valtoid (x:xs) y = valtoid xs (valuetoidsub [x] y x)

idtomcrsub :: String -> [ExpAll] -> ExpAll -> [ExpAll]
idtomcrsub _ [] _ = []
idtomcrsub id (z@(Id id1):ys) exp =
  if id == id1
    then exp:idtomcrsub id ys exp
    else z:idtomcrsub id ys exp
idtomcrsub id (List xs:ys) exp = List (idtomcrsub id xs exp):idtomcrsub id ys exp
idtomcrsub id (IdList xs:ys) exp = IdList (idtomcrsub id xs exp):idtomcrsub id ys exp
idtomcrsub id (ApplyList xs:ys) exp = ApplyList (idtomcrsub id xs exp):idtomcrsub id ys exp
idtomcrsub id (Function xs:ys) exp = Function (idtomcrsub id xs exp):idtomcrsub id ys exp
idtomcrsub id (x:xs) exp = x:idtomcrsub id xs exp

idtomcr :: [ExpAll] -> ExpAll -> ExpAll -> ExpAll
idtomcr [] y _ = y
idtomcr (Id id:xs) (List ys) exp = idtomcr xs (List (idtomcrsub id ys exp)) exp
idtomcr (Id id:xs) (ApplyList ys) exp = idtomcr xs (ApplyList (idtomcrsub id ys exp)) exp
idtomcr (Id id:xs) (IdList ys) exp = idtomcr xs (IdList (idtomcrsub id ys exp)) exp
idtomcr (Id id:xs) (Function ys) exp = idtomcr xs (Function (idtomcrsub id ys exp)) exp
idtomcr (Id id1:xs) z@(Id id) exp =
  if id1 == id
    then exp
    else idtomcr xs z exp
idtomcr (x:xs) y exp = idtomcr xs y exp

mcr :: [ExpAll] -> ExpAll -> [ExpAll] -> ExpAll
mcr [] y [] = y
mcr [x] y zs@(_:_:_) = idtomcr [x] y (List zs)
mcr (x:xs) y (z:zs) = mcr xs (idtomcr [x] y z) zs

calcLambda :: [[(String, ExpAll)]] -> ExpAll -> (ExpAll, [[(String, ExpAll)]])
calcLambda env (Function (x:xs)) = (Function $ x:calcLambdasub env x xs, env)

calcLambdasub :: [[(String, ExpAll)]] -> ExpAll -> [ExpAll] -> [ExpAll]
calcLambdasub env x [] = []
calcLambdasub env x (IdList ys:zs) = IdList (calcLambdasub env x ys): calcLambdasub env x zs
calcLambdasub env x (ApplyList ys:zs) = ApplyList (calcLambdasub env x ys): calcLambdasub env x zs
calcLambdasub env x (Set (sym, id,n):zs) = Set (sym, head (calcLambdasub env x [id]), n):calcLambdasub env x zs
{-
calcLambdasub env (x:xs) (Function (y:ys)) =
  if idBool y x
    then calcLambda
    else 
    -}
calcLambdasub env x (y:ys) =
  if idBool x y
    then y:calcLambdasub env x ys
    else fst (calculate env y):calcLambdasub env x ys

idBool :: ExpAll -> ExpAll -> Bool
idBool (Id id) (Idvalue id1) = id == id1
idBool (List (x:xs)) id = idBool x id || idBool (List xs) id
idBool (Dot xs y) id = idBool (List xs) id || idBool y id
idBool (List []) _ = False

calcLetId :: [[(String, ExpAll)]] -> [[(String, ExpAll)]] ->  ExpAll -> [[(String, ExpAll)]]
calcLetId env retenv (IdList (IdList []:body)) = retenv
calcLetId env retenv (IdList ((IdList ((IdList (Id id:[exp])):xs)):body)) =
  localset (id, afexp) (calcLetId env afenv (IdList (IdList xs:body)))
  where (afexp, _) = calculate env exp
        (_, afenv) = calculate retenv exp

calcApply :: [[(String, ExpAll)]] -> ExpAll -> (ExpAll, [[(String, ExpAll)]])
calcApply env (ApplyList (Function (Emp:xs):ys)) =
  if length ys == 0
    then calculate env (ExpList xs)
    else (String "error", env)
calcApply env (ApplyList ((Function (List []:ys)):[])) = 
  calcLet ([]:env) (IdList (IdList []:ys))
calcApply env (ApplyList ((Function (List []:ys)):zs@(_:_))) = (String "error", env)
calcApply env (ApplyList ((Function (List (x:xs):ys)):[])) = (String "error", env)
calcApply env (ApplyList f@((Function (List (x:xs):ys)):(z:zs))) =
  if length xs == length zs
    then calcLet ([]:env) (functiontoLet f)
    else (String "error", env)
calcApply env (ApplyList ((Function (Id id:ys)):zs)) =
  calcLet ([]:env) (IdList (IdList (IdList (Id id:[List zs]):[]):ys))
calcApply env (ApplyList (Function ((Dot [] id):ys):zs)) =
  calcApply env (ApplyList (Function (id:ys):zs))
calcApply env (ApplyList f@(Function ((Dot xs id):ys):zs)) =
  if length xs <= length zs
    then calcLet ([]:env) (functiontoLet f)
    else (String "error", env)

functiontoLet :: [ExpAll] -> ExpAll
functiontoLet ((Function (List []:ys)):[]) = IdList (IdList []:ys)
functiontoLet ((Function (List (x:xs):ys)):(z:zs)) =
  IdList ((IdList zipped):ys)
  where zipped = ziplist (x:xs) (z:zs)
functiontoLet (Function ((Dot (x:xs) id):ys):zs)=
  IdList (IdList (zipped++[IdList (id:[List (drop (length zipped) zs)])]):ys)
  where zipped = ziplist (x:xs) zs

ziplist :: [ExpAll] -> [ExpAll] -> [ExpAll]
ziplist (x:xs) (y:ys) = (IdList (x:[y])):(ziplist xs ys)
ziplist [] _ = []
ziplist _ _ = error "error ziplist"

calcLet :: [[(String, ExpAll)]] -> ExpAll -> (ExpAll, [[(String, ExpAll)]])
calcLet env (IdList (Emp:body)) = (afexp, afenv)
  where (afexp, henv:afenv) = calculate env (ExpList body)
calcLet env (IdList (IdList []:body)) = (afexp, afenv)
  where (afexp, henv:afenv) = calculate env (ExpList body)
calcLet env x@(IdList ((IdList ((IdList (Id id:[exp])):xs)):body)) =
  calcLet (calcLetId env env x) (IdList (IdList []:body))
calcLet env (IdList (Id id:IdList lst:body)) = 
  calcLet (localset (id, Function (List idlst:body)) env) (IdList (IdList lst:body))
  where idlst = idlisttoid (IdList lst)

idlisttoid :: ExpAll -> [ExpAll]
idlisttoid (IdList []) = []
idlisttoid (IdList (IdList (Id id:_):xs)) = Id id:(idlisttoid (IdList xs))
idlisttoid other = error "error"

calcLetstar :: [[(String, ExpAll)]] -> ExpAll -> (ExpAll, [[(String, ExpAll)]])
calcLetstar env (IdList (Emp:body)) = (afexp, afenv)
  where (afexp, henv:afenv) = calculate env (ExpList body)
calcLetstar env (IdList (IdList []:body)) = (afexp, afenv)
  where (afexp, henv:afenv) = calculate env (ExpList body)
calcLetstar env (IdList ((IdList ((IdList (Id id:[exp])):xs)):body)) =
  calcLetstar (((id, afexp):hafenv):afenv) (IdList (IdList xs:body))
  where (afexp, (hafenv:afenv)) = calculate env exp

calcLetrec :: [[(String, ExpAll)]] -> ExpAll -> (ExpAll, [[(String, ExpAll)]])
calcLetrec env (IdList (Emp:body)) = (afexp, afenv)
  where (afexp, henv:afenv) = calculate env (ExpList body)
calcLetrec env (IdList (IdList []:body)) = (afexp, afenv)
  where (afexp, henv:afenv) = calculate env (ExpList body)
calcLetrec env (IdList ((IdList ((IdList (Id id:[exp])):xs)):body)) =
  calcLetrec (((id, afexp):hafenv):afenv) (IdList (IdList xs:body))
  where (afexp, (hafenv:afenv)) = calculate (localset (id, IdList [exp]) env) exp

calcIf :: [[(String, ExpAll)]] -> ExpAll -> (ExpAll, [[(String, ExpAll)]])
calcIf env (IdList (bool:true:falses)) =
  if (length falses < 2)
    then if (unpackBool exp) 
      then calculate afenv true
      else if length falses == 0
        then (Id "<undef>", afenv)
        else calculate afenv $ head falses
    else (String "error if", env)
  where (exp, afenv) = calculate env bool

calcCond :: [[(String, ExpAll)]]  -> ExpAll -> (ExpAll, [[(String, ExpAll)]])
calcCond env (IdList ((IdList ((Id "else"):xs@(_:_))):[])) = calculate env $ ExpList xs
calcCond env (IdList ((IdList ((Id "else"):xs@(_:_))):ys@(_:_))) = (String "error cond", env)
calcCond env (IdList ((IdList (x:xs@(_:_))):ys)) =
  if (unpackBool exp)
    then calculate afenv $ ExpList xs
    else calcCond afenv $ IdList ys
  where (exp, afenv) = calculate env x

calcAnd :: [[(String, ExpAll)]] -> ExpAll -> (ExpAll, [[(String, ExpAll)]])
calcAnd env (IdList []) = (Bool True, env)
calcAnd env (IdList (x:[])) = calculate env x
calcAnd env (IdList (x:xs@(_:_))) =
  if (unpackBool exp)
    then calcAnd afenv $ IdList xs
    else (Bool False, afenv)
  where (exp, afenv) = calculate env x

calcOr :: [[(String, ExpAll)]] -> ExpAll -> (ExpAll, [[(String, ExpAll)]])
calcOr env (IdList []) = (Bool False, env)
calcOr env (IdList (x:[])) = calculate env x
calcOr env (IdList (x:xs@(_:_))) =
  if (unpackBool exp)
    then (exp, afenv)
    else calcOr afenv $ IdList xs
    where (exp, afenv) = calculate env x

calcBegin :: [[(String, ExpAll)]] -> ExpAll -> (ExpAll, [[(String, ExpAll)]])
calcBegin env (ExpList []) = (Num 0, env)
calcBegin env (ExpList xs@(_:_)) = calculate env (ExpList xs)
calcBegin env _ = error "error calcBegin"

calcDo :: [[(String, ExpAll)]] -> ExpAll -> (ExpAll, [[(String, ExpAll)]])
calcDo env (IdList ((IdList ((IdList [Id id, init, update]):xs)):ys)) =
  calcDo (localset (id, init) env) (IdList (IdList xs:ys++[Set (Id id, update,Num1 (length env))]))
calcDo env (IdList (IdList []:IdList (judge:ret):body)) =
  if (unpackBool judgeexp)
   then (retexp, retenv)
    else calculate judgeenv (ExpList (body++[IdList (Id "do":IdList []:IdList (judge:ret):body)]))
    where (judgeexp, judgeenv) = calculate env judge
          (retexp, (_:retenv)) = calculate judgeenv (ExpList (judgeexp:ret))

{-
calcDosub :: [[(String, ExpAll)]] -> ExpAll -> ([(String, ExpAll)], [[(String, ExpAll)]])
calcDosub env (IdList []) = ([], env)
calcDosub env (IdList (IdList [Id id, init, update]:xs)) =
  ((id, update):afexp, localset (id, init) afenv)
  where (afexp, afenv) = calcDosub env (IdList xs)

calcup :: [[(Stirng, ExpAll)]] -> [(String, ExpAll)] -> [(String, ExpAll)]
calcup env [] = []
calcup env (id, update):xs = (id, calculate env update):calcup xs
-}

calcDef :: [[(String, ExpAll)]] -> (String, ExpAll) -> (ExpAll, [[(String, ExpAll)]])
calcDef xss (id, exp) = (Id id, (lookLocal xss (id, exp)))

calcSet :: [[(String, ExpAll)]] -> (String, ExpAll, ExpAll) -> (ExpAll, [[(String, ExpAll)]])
calcSet xss (id, exp, Num1 n) = 
  if bool
    then (exp, (take ((length xss) - n) xss)++ret)
    else (String "error", (take ((length xss) - n) xss)++ret)
  where (ret, bool) = rewriteenv (drop ((length xss) - n) xss) (id, exp)

calcDotList :: ExpAll -> ExpAll
calcDotList (List (Id "'":xs)) = calcDotList $ List (Id "quote":[List xs])
calcDotList (List (x:xs)) = List $ (calcDotList x):(calcDotList1 xs)
calcDotList (List []) = List []
calcDotList (Dot (Id "'":xs) y) = calcDotList $ List $ Id "quote":[Dot xs y]
calcDotList (Dot xs Emp) = List xs
calcDotList (Dot xs (List [])) = calcDotList (List xs)
calcDotList (Dot xs (List ys)) = calcDotList (List (xs ++ ys))
calcDotList (Dot xs (Dot ys z)) = calcDotList (Dot (xs++ys) z)
calcDotList (Dot (x:xs) y) = Dot (calcDotList x:calcDotList1 xs) (calcDotList y)
calcDotList other = other

calcDotList1 :: [ExpAll] -> [ExpAll]
calcDotList1 [] = []
calcDotList1 (x:xs) = calcDotList x : calcDotList1 xs

calcfunc :: [[(String, ExpAll)]] -> [ExpAll]  -> (ExpAll, [[(String, ExpAll)]])
calcfunc env (Funcoriginal [String "number?"]:x:[]) = (isNum afexp, afenv)
  where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "+"]:xs) = (Num (sum (unpackNum aflst)), afenv)
  where (aflst, afenv) = calclist env xs
calcfunc env (Funcoriginal [String "-"]:x:[]) = (Num ((-) 0 (unpacknum afexp)), afenv)
  where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "-"]:xs@(_:_)) = (Num (foldl (-) (unpacknum afexp) (unpackNum aflst)), afenv)
  where (afexp:aflst, afenv) = calclist env xs
calcfunc env (Funcoriginal [String "/"]:x:[]) = (Num (div 1 (unpacknum afexp)), afenv)
  where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "/"]:xs@(_:_)) = (Num (foldl div (unpacknum afexp) (unpackNum aflst)), afenv)
  where (afexp:aflst, afenv) = calclist env xs
calcfunc env (Funcoriginal [String "*"]:xs) = (Num (product (unpackNum aflst)), afenv)
  where (aflst, afenv) = calclist env xs
calcfunc env (Funcoriginal [String "="]:xs) = (calccmpr (==) (unpackNum aflst), afenv)
  where (aflst, afenv) = calclist env xs
calcfunc env (Funcoriginal [String "<"]:xs) = (calccmpr (<) (unpackNum aflst), afenv)
  where (aflst, afenv) = calclist env xs
calcfunc env (Funcoriginal [String "<="]:xs) = (calccmpr (<=) (unpackNum aflst), afenv)
  where (aflst, afenv) = calclist env xs
calcfunc env (Funcoriginal [String ">"]:xs) = (calccmpr (>) (unpackNum aflst), afenv)
  where (aflst, afenv) = calclist env xs
calcfunc env (Funcoriginal [String ">="]:xs) = (calccmpr (>=) (unpackNum aflst), afenv)
  where (aflst, afenv) = calclist env xs
calcfunc env (Funcoriginal [String "null?"]:x:[]) = (isNull afexp, afenv)
  where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "pair?"]:x:[]) = (isPair afexp, afenv)
  where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "list?"]:x:[]) = (isList afexp, afenv)
  where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "symbol?"]:x:[]) = (issymbol afexp, afenv)
  where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "car"]:x:[]) = (carret afexp, afenv)
  where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "cdr"]:x:[]) = (cdrret afexp, afenv)
  where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "cons"]:x:y:[]) = (calcDotList (Dot [afexp1] afexp2), afenv2)
  where (afexp1, afenv1) = calculate env x
        (afexp2, afenv2) = calculate afenv1 y
calcfunc env (Funcoriginal [String "list"]:xs) = (List aflst, afenv)
  where (aflst, afenv) = calclist env xs
calcfunc env (Funcoriginal [String "length"]:List x:[]) = (Num1 (length x), env)
calcfunc env (Funcoriginal [String "length"]:Emp:[]) = (Num1 0, env)
calcfunc env (Funcoriginal [String "length"]:_:[]) = (String "error", env)
calcfunc env (Funcoriginal [String "memq"]:x:y:[]) = (memqf afexp1 afexp2, afenv2)
  where (afexp1, afenv1) = calculate env x
        (afexp2, afenv2) = calculate afenv1 y
calcfunc env (Funcoriginal [String "last"]:x:[]) = (lastf afexp, afenv)
  where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "append"]:xs) = (appendf xs, afenv)
  where (aflst, afenv) = calclist env xs
calcfunc env (Funcoriginal [String "boolean?"]:x:[]) = (isBool afexp, afenv)
 where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "not"]:x:[]) = (notf afexp, afenv)
  where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "string?"]:x:[]) = (isString afexp, afenv)
  where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "string-append"]:xs) = (String (concat (unpackstr aflst)), env)
  where (aflst, afenv) = calclist env xs
calcfunc env (Funcoriginal [String "symbol->string"]:x:[]) = (idtostring afexp, afenv)
  where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "string->symbol"]:x:[]) = (stringtoid afexp, afenv)
  where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "string->number"]:str:[]) =
  calcfunc env (Funcoriginal [String "string->number"]:str:Num 10:[])
calcfunc env (Funcoriginal [String "string->number"]:strn@(_:_:[])) =
  if (unpackBool (isString afstr))
    then if (strtointf (unpackStr afstr) (unpacknum afnum))
      then (Num (strtointpm (unpackStr afstr) (unpacknum afnum)), afenv)
      else (Bool False, afenv)
    else (String "error", env)
  where (afstr:afnum:afexp, afenv) = calclist env strn
calcfunc env (Funcoriginal [String "number->string"]:str:[]) =
  calcfunc env (Funcoriginal [String "number->string"]:str:Num 10:[])
calcfunc env (Funcoriginal [String "number->string"]:strn@(_:_:[])) =
  if (unpackBool (isNum afstr)) && (unpacknum afnum < 37)
    then (String (inttostr (unpacknum afstr) (unpacknum afnum)), afenv)
    else (String "error", env)
  where (afstr:afnum:afexp, afenv) = calclist env strn
calcfunc env (Funcoriginal [String "procedure?"]:x:[]) = (isProcedure afexp, afenv)
  where (afexp, afenv) = calculate env x
calcfunc env (Funcoriginal [String "eq?"]:x:y:[]) = (cmpad x y, afenv2)
  where (afexp1, afenv1) = calculate env x
        (afexp2, afenv2) = calculate afenv1 y
calcfunc env (Funcoriginal [String "neq?"]:x:y:[]) = (Bool . not $ isequal afexp1 afexp2, afenv2)
  where (afexp1, afenv1) = calculate env x
        (afexp2, afenv2) = calculate afenv1 y
calcfunc env (Funcoriginal [String "equal?"]:x:y:[]) = (Bool $ isequal afexp1 afexp2 , afenv2)
  where (afexp1, afenv1) = calculate env x
        (afexp2, afenv2) = calculate afenv1 y

calclist :: [[(String, ExpAll)]]  -> [ExpAll] -> ([ExpAll], [[(String, ExpAll)]])
calclist env [] = ([], env)
calclist env (x:xs) = (afexp:lstexp, lstenv)
  where (afexp, afenv) = calculate env x
        (lstexp, lstenv) = calclist afenv xs

calccmpr :: (Int -> Int -> Bool) -> [Int] -> ExpAll
calccmpr _ (_:[]) = Bool True
calccmpr f (x:y:xs) = if f x y then calccmpr f (y:xs) else Bool False

unpackNum :: [ExpAll] -> [Int]
unpackNum [] = []
unpackNum (Num n:xs) = n : unpackNum xs

unpackStr :: ExpAll -> String
unpackStr (String str) = str
unpackStr _ = "error unpackStr"

unpacknum :: ExpAll -> Int
unpacknum (Num n) = n

isNum (Num _) = Bool True
isNum _ = Bool False

isNull Emp = Bool True
isNull (List []) = Bool True
isNull _ = Bool False

isPair (List (_:_)) = Bool True
isPair (Dot _ _) = Bool True
isPair _ = Bool False

isList (List _) = Bool True
isList Emp = Bool True
isList _ = Bool False

issymbol (Id _) = Bool True
issymbol _ = Bool False

carret (List (x:xs)) = x
carret (Dot (x:xs) _) = x
carret other = String "error"

cdrret (List (x:xs)) = List xs
cdrret (Dot (x:xs) ys) = Dot xs ys
cdrret other = String "error"

memqf :: ExpAll -> ExpAll -> ExpAll
memqf x (List (y:ys)) =
  if isequal x y
    then List (y:ys)
    else memqf x (List ys)
memqf x (Dot (y:ys) z) =
  if isequal x y
    then Dot (y:ys) z
    else memqf x (Dot ys z)
memqf _ _ = Bool False

lastf :: ExpAll -> ExpAll
lastf (List xs@(_:_)) = last xs
lastf (Dot xs _) = last xs
lastf _ = String "error"

appendf :: [ExpAll] -> ExpAll
appendf [] = Emp
appendf (x:[]) = x
appendf (Emp:xs) = appendf xs
appendf (List xs:List ys:zs) = appendf (List (xs ++ ys):zs) 
appendf (List xs:y:[]) = Dot xs y
appendf other = String "error"

isBool (Bool _) = Bool True
isBool _ = Bool False

notf (Bool False) = Bool True
notf _ = Bool False

isString (String "error") = Bool False
isString (String _) = Bool True
isString _ = Bool False

stringtoid (String xs) = Id xs
stringtoid _ = String "error"

idtostring (Id xs) = String xs
idtostring _ = String "error"

unpackstr :: [ExpAll] -> [String]
unpackstr [] = []
unpackstr (String str:xs) = str : unpackstr xs

strtointf :: [Char] -> Int -> Bool
strtointf ('-':xs) n = strtointsub xs n
strtointf ('+':xs) n = strtointsub xs n
strtointf other n = strtointsub other n

strtointsub :: [Char] -> Int -> Bool
strtointsub [] _ = False
strtointsub [x] n = if n < 37 then True else False
strtointsub (x:xs@(_:_)) n
  | '0' <= x && x <= '9' =
    if (ord x - ord '0') < n
      then strtointsub xs n
      else False
  | 'a' <= x && x <= 'z' =
    if ord x - ord 'a' + 10 < n
      then strtointsub xs n
      else False

strtointpm :: [Char] -> Int -> Int
strtointpm ('-':xs) n = (-1) * strtoint xs n
strtointpm ('+':xs) n = strtoint xs n
strtointpm other n = strtoint other n

strtoint :: [Char] -> Int -> Int
strtoint [x] n
  | '0' <= x && x <= '9' = ord x - ord '0'
  | 'a' <= x && x <= 'z' = ord x - ord 'a' + 10
strtoint (x:xs@(_:_)) n = strtoint [x] n * n + strtoint xs n

inttostr :: Int -> Int -> [Char]
inttostr n n1 =
  if n < 0
    then "-" ++ inttostr (-1*n) n1
    else if quo == 0
      then ch
      else inttostr quo n1 ++ ch
    where quo = div n n1
          rem = mod n n1
          ch = if rem > 9 then [chr (ord 'a' + rem - 10)] else show rem

isProcedure (Function _) = Bool True
isProcedure (Funcoriginal _) = Bool True
isProcedure _ = Bool False

cmpad (Id x) (Id y) = if x == y then Bool True else Bool False
--cmpad (Idvalue (str1, _)) (Idvalue (str2, _)) = if str1 == str2 then Bool True else Bool False
cmpad (Idvalue str1) (Idvalue str2) = if str1 == str2 then Bool True else Bool False
cmpad _ _ = Bool False

isequal :: ExpAll -> ExpAll -> Bool
isequal (Num n1) (Num n2) = n1 == n2
isequal (Bool b1) (Bool b2) = b1 == b2
isequal (String str1) (String str2) = str1 == str2
isequal (Id id1) (Id id2) = id1 == id2
isequal Emp Emp = True
isequal (List (x:xs)) (List (y:ys)) =
  if isequal x y
    then isequal (List xs) (List ys)
    else False
isequal (List []) (List []) = True
isequal (Dot xs x) (Dot ys y) =
  if isequal x y
    then isequal (List xs) (List ys)
    else False
isequal (Funcoriginal f1) (Funcoriginal f2) = isequal (List f1) (List f2)
isequal _ _ = False

--------------show-------------------

showval :: ExpAll -> String
showval (Bool True) = "#t"
showval (Bool False) = "#f"
showval (Num n) = show n
showval (Num1 n) = show n
showval (String str) = "\"" ++ str ++ "\""
showval Emp = "()"
showval (Function _) = "#<closure>"
showval (DefMacro _) = "<macro>"
showval (List (Id "quote":xs)) = "'"++ showval (ShowList xs)
showval (List (x:xs)) = "(" ++ (showval x) ++ (showval (ShowList xs)) ++ ")"
showval (List []) = "()"
showval (ShowList (x:xs)) = " " ++ (showval x) ++ (showval (ShowList xs))
showval (ShowList []) = ""
showval (Dot (x:xs) y) = "(" ++ (showval x) ++ (showval (ShowList xs)) ++ " . " ++ (showval y) ++ ")"
showval (ExpList xs) = showval $ last xs
showval (Id id) = id
showval (Ld str) = "load "++str
showval other = show other --debug

isLd (Ld xs) = True
isLd _ = False

unpackLd (Ld xs) = xs
unpackLd _ = error "error unpackLd"

run_parser :: Parser ExpAll -> String -> ExpAll
run_parser p str = case parse p "" str of
  Left err -> Id "parse error"
  Right val -> val

act :: [[(String, ExpAll)]] -> IO (ExpAll, [[(String, ExpAll)]])
act env = getLine >>= return . run_parser (parseTop env) >>= return . (calculate env)

scheme :: IO()
scheme = do
  let loop env = do
      putStr "scheme > " >> hFlush stdout
      (exp, afterenv) <- act env
      str <- return $ showval exp
      putStrLn str
      if (isLd exp)
        then do 
          file <- parseFromFile (parseTop env) (unpackLd exp)
          case file of
            Left err -> putStrLn "read error"
            Right ld -> putStrLn $ showval $ fst $ calculate afterenv ld
        else return ()
      when (str /= "\"exit\"") (loop afterenv)
  loop [[], func, original]

main = do
  scheme
