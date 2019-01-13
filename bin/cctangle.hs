{-# LINE 64 "ccweb.org" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 49 "ccweb.org" #-}
import Control.Applicative ((<|>), (<**>), liftA2)
import Control.Monad (void, when)
import Control.Monad.State (State, get, modify, runState)
import Data.Foldable (foldlM, foldrM)
import Data.Char (isPrint, isSpace, ord)
import Data.List (intercalate, isPrefixOf, isSuffixOf, nub, sort)
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Debug.Trace (trace)
import qualified Data.Map as Map
import GHC.Exts (IsList(..), groupWith, sortWith)
import Network.HostName (getHostName)
{-# LINE 214 "org/doc.org" #-}
import qualified System.Info as Sys
{-# LINE 72 "org/parser.org" #-}
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
{-# LINE 395 "org/tangle.org" #-}
import qualified System.Directory as D
import qualified System.Environment as Env
import qualified System.FilePath as F
import qualified System.Posix.Files as F
{-# LINE 33 "org/scaffold.org" #-}
import qualified Options.Applicative as O
{-# LINE 180 "org/scaffold.org" #-}
import qualified Text.PrettyPrint as PP
{-# LINE 12 "org/doc.org" #-}
data Document = Document
  { keywords :: [Keyword]
  , sections :: [Section]
  }
{-# LINE 40 "org/doc.org" #-}
data Keyword = AuthorKeyword Text
             | PropertyKeyword Properties
             | TitleKeyword Text
             | OtherKeyword String
{-# LINE 68 "org/doc.org" #-}
data Section = Section
  { sectionNumber :: Int
  , sectionHeadline :: Maybe Headline
  , documentation :: [Text]
  , sectionSourceBlock :: Maybe SourceBlock
  , sectionProps :: Properties
  , sectionDerivedProperties :: Properties
  }
{-# LINE 100 "org/doc.org" #-}
data Headline = Headline Int Text deriving Show
{-# LINE 121 "org/doc.org" #-}
type Property   = Map.Map String SExpr
type Properties = Map.Map String Property
{-# LINE 132 "org/doc.org" #-}
data SExpr =
  Atom String
  | BoolAtom Bool
  | IntAtom Int
  | SExpr [SExpr]
  deriving (Eq, Ord)
{-# LINE 169 "org/doc.org" #-}
class Eval a where
  eval :: ParserState -> a -> a
{-# LINE 177 "org/doc.org" #-}
instance Eval Property where; eval s = Map.map (eval s)
instance Eval Properties where; eval s = Map.map (eval s)
{-# LINE 186 "org/doc.org" #-}
instance Eval SExpr where
{-# LINE 195 "org/doc.org" #-}
  eval s (SExpr [Atom "identity", expr]) = eval s expr
{-# LINE 203 "org/doc.org" #-}
  eval s (SExpr [Atom "system-name"]) = Atom $ hostName s
  eval _ (Atom "system-type") = Atom $ case Sys.os of
              "linux" -> "gnu/linux"
              "mingw32" -> "windows-nt"
              o -> o
{-# LINE 222 "org/doc.org" #-}
  eval _ (Atom ('#':'o':x:y:z:[])) =
    IntAtom $ 8 * ((8 * oct x) + oct y) + oct z
    where oct c = ord c - ord '0'
{-# LINE 231 "org/doc.org" #-}
  eval s (SExpr [Atom "eq", e1, e2]) = BoolAtom $ (eval s e1) == (eval s e2)
  
{-# LINE 233 "org/doc.org" #-}
  eval s (SExpr [Atom "string-suffix-p", suffix, expr]) =
    case (eval s suffix, eval s expr) of
      (Atom s', Atom str) -> BoolAtom $ isSuffixOf s' str
      (e1, e2) -> SExpr [Atom "string-suffix-p", e1, e2]
{-# LINE 243 "org/doc.org" #-}
  eval s (SExpr [Atom "when", expr, result]) =
    case eval s expr of
      BoolAtom True -> eval s result
      expr' -> expr'
  
{-# LINE 248 "org/doc.org" #-}
  eval s (SExpr [Atom "unless", expr, result]) =
    case eval s expr of
      BoolAtom False -> eval s result
      BoolAtom True  -> BoolAtom False
      expr' -> expr'
{-# LINE 188 "org/doc.org" #-}
  eval _ x = x
{-# LINE 265 "org/doc.org" #-}
class HeaderArgs a where
  headerArgs :: a -> Property
  headerArg :: String -> a -> Maybe SExpr
  headerArg k a = Map.lookup k $ headerArgs a

{-# LINE 270 "org/doc.org" #-}
instance HeaderArgs Properties where
  headerArgs = Map.findWithDefault Map.empty "header-args"

{-# LINE 273 "org/doc.org" #-}
instance HeaderArgs Section where
  headerArgs = headerArgs . sectionDerivedProperties
{-# LINE 289 "org/doc.org" #-}
data SourceBlock = SourceBlock
  { blockName :: Maybe Text
  , blockLanguage :: String
  , blockLines :: [CodeLine]
  , blockLocation :: P.SourcePos
  , blockProperties :: Property
  , blockDerivedProperties :: Property
  }
{-# LINE 318 "org/doc.org" #-}
instance HeaderArgs SourceBlock where
  headerArgs = blockDerivedProperties
{-# LINE 329 "org/doc.org" #-}
data SourceBlockId = FileBlock FilePath | NamedBlock Text deriving Eq

{-# LINE 331 "org/doc.org" #-}
instance Ord SourceBlockId where
  (<=) (FileBlock p1) (FileBlock p2) = p1 <= p2
  (<=) (NamedBlock p1) (NamedBlock p2) = p1 <= p2
  (<=) (FileBlock _) (NamedBlock _) = False
  (<=) _ _ = True
{-# LINE 345 "org/doc.org" #-}
sourceBlockId :: SourceBlock -> Maybe SourceBlockId
sourceBlockId SourceBlock{ blockName = (Just name) } = Just $ NamedBlock name
sourceBlockId block = case headerArg ":tangle" block of
    Nothing -> Nothing
    Just (Atom f) -> Just $ FileBlock f
    Just (BoolAtom _) -> Nothing
    Just e -> error $ "unsupported tangle destination: " ++ show e
{-# LINE 357 "org/doc.org" #-}
data CodeElement = Literal P.SourcePos String
                 | SectionReference P.SourcePos Text

{-# LINE 360 "org/doc.org" #-}
data CodeLine = CodeLine P.SourcePos [CodeElement]
{-# LINE 367 "org/doc.org" #-}
data Text = Text [TextElement] deriving (Eq, Ord, Show)

{-# LINE 369 "org/doc.org" #-}
data TextElement =
  Bold String
  | InlineCode String
  | Italics String
  | Plain String
  | StrikeThrough String
  | TeXMath String
  | Verbatim String
  deriving (Eq, Ord, Show)
{-# LINE 384 "org/doc.org" #-}
type DocumentPartition = Map.Map SourceBlockId [Section]
{-# LINE 27 "org/doc.org" #-}
instance Pretty Document where
  pretty = prettyStruct "Document"
             [ ("keywords", pretty . keywords)
             , ("sections", pretty . sections)
             ]
{-# LINE 50 "org/doc.org" #-}
instance Pretty Keyword where
  pretty (AuthorKeyword a) = PP.hsep [PP.text "Author:", pretty a]
  pretty (PropertyKeyword a) = PP.hsep [PP.text "Properties:", pretty a]
  pretty (TitleKeyword a) = PP.hsep [PP.text "Title:", pretty a]
  pretty (OtherKeyword a) = PP.hsep [PP.text "Other:", pretty a]
{-# LINE 82 "org/doc.org" #-}
instance Pretty Section where
  pretty = prettyStruct "Section"
             [ ("number", pretty . sectionNumber)
             , ("headline", pretty . sectionHeadline)
             , ("text", pretty . documentation)
             , ("code", pretty . sectionSourceBlock)
             , ("properties", pretty . sectionProps)
             , ("derived properties", pretty . sectionDerivedProperties)
             ]
{-# LINE 107 "org/doc.org" #-}
instance Pretty Headline where
  pretty (Headline l t) = PP.hcat [PP.char '*', PP.braces (pretty l), pretty t]
{-# LINE 145 "org/doc.org" #-}
instance Show SExpr where
  show (Atom x) = x
  show (BoolAtom x) = show x
  show (IntAtom x) = show x
  show (SExpr xs) = "(" ++ intercalate " " (map show xs) ++ ")"

{-# LINE 151 "org/doc.org" #-}
instance Pretty SExpr where
  pretty = PP.text . show
{-# LINE 303 "org/doc.org" #-}
instance Pretty SourceBlock where
  pretty = prettyStruct "SourceBlock"
             [ ("name", pretty . blockName)
             , ("language", pretty . blockLanguage)
             , ("properties", pretty . blockProperties)
             , ("derived properties", pretty . blockDerivedProperties)
             , ("location", pretty . blockLocation)
             , ("lines", pretty . blockLines)
             ]
{-# LINE 23 "org/parser.org" #-}
instance Pretty ParserState where
  pretty = prettyStruct "ParserState"
    [ ("counter", pretty . sectionCounter)
    , ("host", pretty . hostName)
    , ("log level", pretty . parserLogLevel)
    , ("property stack", pretty . propertyStack)
    ]
{-# LINE 62 "org/parser.org" #-}
instance Pretty OrgLine where
  pretty (OrgLine p l) =
    PP.hcat [pretty p, PP.colon, PP.text (takeWhile (/= '\NUL') l)]
{-# LINE 190 "org/scaffold.org" #-}
class Pretty a where
  pretty :: a -> PP.Doc
  prettyList :: [a] -> PP.Doc
  prettyList = PP.brackets . PP.fcat . PP.punctuate PP.comma . map pretty

{-# LINE 195 "org/scaffold.org" #-}
instance Pretty PP.Doc where
  pretty = id

{-# LINE 198 "org/scaffold.org" #-}
instance Pretty Bool where
  pretty True = PP.text "True"
  pretty False = PP.text "False"

{-# LINE 202 "org/scaffold.org" #-}
instance Pretty Char where
  pretty = PP.char
  prettyList = PP.text

{-# LINE 206 "org/scaffold.org" #-}
instance Pretty Int where
  pretty = PP.int

{-# LINE 209 "org/scaffold.org" #-}
instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = PP.text "Nothing"
  pretty (Just a) = pretty a

{-# LINE 213 "org/scaffold.org" #-}
instance (Pretty a, Pretty b) => Pretty (a,b) where
  pretty (a,b) = PP.parens . PP.fcat . PP.punctuate PP.comma $ [pretty a, pretty b]

{-# LINE 216 "org/scaffold.org" #-}
instance Pretty a => Pretty [a] where
  pretty = prettyList

{-# LINE 219 "org/scaffold.org" #-}
instance Pretty P.ParseError where
  pretty = PP.text . show
{-# LINE 225 "org/scaffold.org" #-}
instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
  pretty = PP.brackets
           . PP.fcat
           . PP.punctuate PP.comma
           . map (\(k,v) -> pretty k PP.<> PP.equals PP.<> pretty v)
           . Map.toList
{-# LINE 235 "org/scaffold.org" #-}
data PrettyStruct = PrettyStruct String [(String, PP.Doc)]

{-# LINE 237 "org/scaffold.org" #-}
instance Pretty PrettyStruct where
  pretty (PrettyStruct t ps) =
    let fields = map (\(n,p) -> PP.text n PP.<> PP.colon PP.<+> p) ps
    in PP.text t PP.<> PP.braces (PP.fcat $ PP.punctuate PP.comma fields)

{-# LINE 242 "org/scaffold.org" #-}
prettyStruct :: String -> [(String, (a -> PP.Doc))] -> a -> PP.Doc
prettyStruct name kfs a = pretty (PrettyStruct name (map (\(k,f) -> (k,f a)) kfs))
{-# LINE 248 "org/scaffold.org" #-}
instance Pretty P.SourcePos where
  pretty p = pretty (P.sourceName p)
             PP.<> PP.colon PP.<> pretty (P.sourceLine p)
             PP.<> PP.colon PP.<> pretty (P.sourceColumn p)
{-# LINE 137 "ccweb.org" #-}
instance Pretty OrgLines where
  pretty (OrgLines ls) = pretty ls
{-# LINE 153 "ccweb.org" #-}
instance Pretty TextElement where
  pretty (Bold a) = PP.text "Bold:" PP.<+> pretty a
  pretty (InlineCode a) = PP.text "InlineCode:" PP.<+> pretty a
  pretty (Italics a) = PP.text "Italics:" PP.<+> pretty a
  pretty (Plain a) = pretty a
  pretty (StrikeThrough a) = PP.text "StrikeThrough:" PP.<+> pretty a
  pretty (TeXMath a) = PP.text "TeXMath:" PP.<+> pretty a
  pretty (Verbatim a) = PP.text "Verbatim:" PP.<+> pretty a
{-# LINE 183 "ccweb.org" #-}
instance Pretty Text where
  pretty (Text xs) = PP.text "Text:" PP.<+> PP.hcat (map pretty xs)
{-# LINE 204 "ccweb.org" #-}
instance Pretty CodeElement where
  pretty (Literal p s) = PP.parens $ pretty p PP.<> PP.colon PP.<> pretty s
  pretty (SectionReference p t) = pretty p PP.<> PP.colon PP.<> PP.char '«' PP.<> pretty t PP.<> PP.char '»'
{-# LINE 236 "ccweb.org" #-}
instance Pretty CodeLine where
  pretty (CodeLine p xs) = pretty p PP.<> PP.colon PP.<> pretty xs
{-# LINE 7 "org/scaffold.org" #-}
data LogLevel = Quiet | Error | Warning | Info | Debug | Trace deriving (Eq,Show)

{-# LINE 9 "org/scaffold.org" #-}
instance Ord LogLevel where
  (<=) _ Trace = True
  (<=) Trace _ = False
  (<=) _ Debug = True
  (<=) Debug _ = False
  (<=) _ Info = True
  (<=) Info _ = False
  (<=) _ Warning = True
  (<=) Warning _ = False
  (<=) _ Error = True
  (<=) Error _ = False
  (<=) Quiet Quiet = True

{-# LINE 22 "org/scaffold.org" #-}
instance Pretty LogLevel where
  pretty = PP.text . show

{-# LINE 25 "org/scaffold.org" #-}
logM :: Pretty a => LogLevel -> LogLevel -> a -> IO ()
logM lvl minLvl = when (lvl >= minLvl) . putStrLn . PP.render . pretty
{-# LINE 11 "org/parser.org" #-}
data ParserState = ParserState
  { sectionCounter :: Int
  , propertyStack  :: Stack Properties
  , hostName       :: String
  , parserLogLevel :: LogLevel
  }
{-# LINE 39 "org/parser.org" #-}
type Parser = P.Parsec OrgLines ParserState

{-# LINE 41 "org/parser.org" #-}
class Parse a where
  parse :: Parser a
{-# LINE 54 "org/parser.org" #-}
data OrgLine = OrgLine P.SourcePos String
{-# LINE 90 "org/parser.org" #-}
ingest :: FilePath -> IO [OrgLine]
ingest file = do
    zipWith OrgLine [P.newPos file l 0 | l <- [1..]] . lines
      <$> readFile file
      >>= foldrM scan []
  where
    scan l@(OrgLine pos s) ls =
      maybe (return (l : ls)) (fmap (++ ls) . ingest)
      $ (
{-# LINE 108 "org/parser.org" #-}
         fromEither . P.parse
         (P.setPosition pos *>
             P.optionMaybe (
               P.try (P.spaces *> P.string "#+INCLUDE:" *> P.spaces)
               *> P.char '"'
               *> P.manyTill anyChar (P.char '"')
             ))
         (P.sourceName pos)
{-# LINE 98 "org/parser.org" #-}
                            :: String -> Maybe FilePath) s
{-# LINE 132 "org/parser.org" #-}
anyChar :: (P.Stream s m Char) => P.ParsecT s u m Char
anyChar = P.noneOf "\n\r"
{-# LINE 146 "org/parser.org" #-}
instance Monad m => P.Stream OrgLine m Char where
  uncons (
{-# LINE 159 "org/parser.org" #-}
          OrgLine _ ('\NUL':_)
{-# LINE 147 "org/parser.org" #-}
                              ) =
    return Nothing
  uncons (OrgLine p []) =
    return $ Just ('\n', OrgLine (P.updatePosChar p '\n') "\NUL")
  uncons (OrgLine p (x:xs)) =
    return $ Just (x, OrgLine (P.updatePosChar p x) xs)
{-# LINE 169 "org/parser.org" #-}
newtype OrgLines = OrgLines [OrgLine]
{-# LINE 178 "org/parser.org" #-}
instance Monad m => P.Stream OrgLines m Char where
  uncons (OrgLines (
{-# LINE 159 "org/parser.org" #-}
                    OrgLine _ ('\NUL':_)
{-# LINE 179 "org/parser.org" #-}
                                        :[])) = return Nothing
  uncons (OrgLines (x:xs)) = P.uncons x >>= \case
    Nothing -> P.uncons (fromList xs :: OrgLines)
    Just (x',xs') -> return $ Just (x', OrgLines (xs':xs))
  uncons (OrgLines []) =
    error $ "(internal): uncons of empty OrgLines instance"
{-# LINE 195 "org/parser.org" #-}
space :: Parser Char
space = P.satisfy (\c -> isSpace c && notElem c "\n\r")

{-# LINE 198 "org/parser.org" #-}
spaces :: Parser String
spaces = P.many space

{-# LINE 201 "org/parser.org" #-}
spaces1 :: Parser String
spaces1 = P.many1 space
{-# LINE 212 "org/parser.org" #-}
symbolChar :: Parser Char
symbolChar = P.satisfy (\c -> isPrint c && not (isSpace c) && notElem c "()=")

{-# LINE 215 "org/parser.org" #-}
symbol :: Parser String
symbol = P.many1 symbolChar
{-# LINE 229 "org/parser.org" #-}
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P.tokenPrim
            (\c -> show [c])
            (\pos _ (OrgLines rest) ->
               case rest of
                 (OrgLine pos' _):_ -> pos'
                 [] -> error $ show pos ++ ": (internal) empty OrgLines")
            (\c -> if f c then Just c else Nothing)
{-# LINE 247 "org/parser.org" #-}
char :: Char -> Parser Char
char c = satisfy (==c) P.<?> show [c]

{-# LINE 250 "org/parser.org" #-}
newline :: Parser Char
newline = char '\n' P.<?> "lf new-line"

{-# LINE 253 "org/parser.org" #-}
crlf :: Parser Char
crlf = char '\r' *> char '\n' P.<?> "crlf new-line"

{-# LINE 256 "org/parser.org" #-}
endOfLine :: Parser Char
endOfLine = newline <|> crlf P.<?> "new-line"
{-# LINE 151 "org/scaffold.org" #-}
newtype Stack a = Stack [a]

{-# LINE 153 "org/scaffold.org" #-}
instance Pretty a => Pretty (Stack a) where
  pretty (Stack xs) = pretty xs

{-# LINE 156 "org/scaffold.org" #-}
top :: Stack a -> a
top (Stack xs) = head xs

{-# LINE 159 "org/scaffold.org" #-}
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack $ x:xs

{-# LINE 162 "org/scaffold.org" #-}
pop :: Stack a -> Stack a
pop (Stack []) = error $ "popping empty stack"
pop (Stack (_:xs)) = Stack xs

{-# LINE 166 "org/scaffold.org" #-}
resize :: Int -> Stack a -> Stack a
resize i (Stack xs)
  | i == l = Stack xs
  | i < l = Stack $ reverse . take i . reverse $ xs
  | i > l && l > 0 = Stack $ replicate (i - l) (head xs) ++ xs
  | otherwise = error $ "resizing empty stack"
  where
    l = length xs
{-# LINE 96 "ccweb.org" #-}
initialParserState :: ParserState
initialParserState = ParserState
  { sectionCounter = 0
  , propertyStack  = Stack []
  , parserLogLevel = Debug
  , hostName       = []
  }
{-# LINE 107 "ccweb.org" #-}
parserDebug :: (Pretty a, Pretty b) => a -> b -> Parser ()
parserDebug l s = do
  lvl <- parserLogLevel <$> P.getState
  pos <- P.getPosition
  let label = pretty pos PP.<> PP.colon PP.<> pretty l PP.<> PP.colon
  if lvl >= Trace
  then trace (PP.render (PP.hang label 4 (pretty s))) $ return ()
  else return ()

{-# LINE 116 "ccweb.org" #-}
parserTrace :: Pretty a => a -> Parser ()
parserTrace l = do
  (OrgLines ls) <- P.stateInput <$> P.getParserState
  u <- P.getState
  let user = pretty "user state" PP.<> PP.colon PP.<> pretty u
      input = pretty "looking at" PP.<> PP.colon PP.<> pretty (take 3 ls)
  parserDebug l (PP.fcat [user, input])
{-# LINE 129 "ccweb.org" #-}
instance IsList OrgLines where
  type Item OrgLines = OrgLine
  fromList xs = OrgLines xs
  toList (OrgLines xs) = xs
{-# LINE 145 "ccweb.org" #-}
line :: Parser a -> Parser a
line p = p <* endOfLine
{-# LINE 165 "ccweb.org" #-}
instance Parse TextElement where
  parse = literalOr (return . Plain) (
{-# LINE 171 "ccweb.org" #-}
                                      P.try (Bold <$> enclosed '*')
                                      <|> P.try (InlineCode <$> enclosed '~')
                                      <|> P.try (Italics <$> enclosed '/')
                                      <|> P.try (StrikeThrough <$> enclosed '+')
                                      <|> P.try (TeXMath <$> enclosed '$')
                                      <|> P.try (Verbatim <$> enclosed '=')
{-# LINE 166 "ccweb.org" #-}
                                                                           )
{-# LINE 191 "ccweb.org" #-}
instance Parse Text where
  parse = do
    --parserTrace "Text: parse"
    P.notFollowedBy (parse :: Parser Headline)
    P.notFollowedBy (spaces *> P.string "#+NAME")
    P.notFollowedBy (spaces *> P.string "#+BEGIN_SRC")
    Text <$> line (P.many1 parse :: Parser [TextElement])
{-# LINE 211 "ccweb.org" #-}
instance Parse CodeElement where
  parse = literalOr
              (liftA2 Literal P.getPosition . return)
              (P.try $ 
{-# LINE 219 "ccweb.org" #-}
                       do
                         p <- P.try (P.string "<<") *> P.getPosition
                         --parserTrace "Trying to parse a code reference after '<<'"
                         l <- OrgLine p <$> P.manyTill anyChar (P.try $ P.string ">>")
                         let t = fromEither
                               $ P.runParser
                               (parse :: Parser Text)
                               initialParserState
                               (P.sourceName p)
                               (fromList [l] :: OrgLines)
                             r = SectionReference p t
                         --parserTrace $ PP.text "Reference parsed" PP.<> PP.colon PP.<> pretty r
                         return r
{-# LINE 214 "ccweb.org" #-}
                                 )
{-# LINE 242 "ccweb.org" #-}
instance Parse CodeLine where
  parse = line (liftA2 CodeLine
                     P.getPosition
                     (P.many parse :: Parser [CodeElement]))
{-# LINE 252 "ccweb.org" #-}
instance Parse SExpr where
  parse =
    (
{-# LINE 263 "ccweb.org" #-}
     SExpr <$> ( P.try (P.char '(')
                  *> spaces
                  *> (P.sepEndBy parse spaces1 <* P.char ')')
                )
{-# LINE 254 "ccweb.org" #-}
                 )
    <|>
    P.try (
{-# LINE 271 "ccweb.org" #-}
           BoolAtom <$> (
             (P.string "no" *> P.lookAhead P.space *> return False)
             <|>
             (P.string "yes" *> P.lookAhead P.space *> return True)
             )
{-# LINE 256 "ccweb.org" #-}
              )
    <|>
    (
{-# LINE 280 "ccweb.org" #-}
     Atom <$> (enclosed '"' <|> symbol)
{-# LINE 258 "ccweb.org" #-}
                                       )
{-# LINE 287 "ccweb.org" #-}
instance Parse Properties where
  parse = (
{-# LINE 294 "ccweb.org" #-}
           do
             _ <- P.try (P.string "#+PROPERTY:") *> spaces
             p <- symbol <* spaces1
             kvs <- 
{-# LINE 303 "ccweb.org" #-}
                    line (Map.fromList
                          <$> P.sepEndBy
                           (
{-# LINE 311 "ccweb.org" #-}
                            do
                              k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                              v <- parse :: Parser SExpr
                              return (k, v)
{-# LINE 305 "ccweb.org" #-}
                                           )
                           spaces1)
{-# LINE 298 "ccweb.org" #-}
             return $ Map.singleton p kvs
{-# LINE 288 "ccweb.org" #-}
                                         )
              <|> (
{-# LINE 319 "ccweb.org" #-}
                   do
                     _ <- P.try (spaces *> P.string ":PROPERTIES:" *> spaces *> endOfLine)
                     Map.fromList <$> P.manyTill
                       (
{-# LINE 328 "ccweb.org" #-}
                        do
                          p <- spaces *> (enclosed ':' <* spaces)
                          kvs <- 
{-# LINE 303 "ccweb.org" #-}
                                 line (Map.fromList
                                       <$> P.sepEndBy
                                        (
{-# LINE 311 "ccweb.org" #-}
                                         do
                                           k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                                           v <- parse :: Parser SExpr
                                           return (k, v)
{-# LINE 305 "ccweb.org" #-}
                                                        )
                                        spaces1)
{-# LINE 331 "ccweb.org" #-}
                          return (p, kvs)
{-# LINE 322 "ccweb.org" #-}
                                         )
                       (P.try (spaces *> P.string ":END:" *> spaces *> endOfLine))
{-# LINE 289 "ccweb.org" #-}
                                                                                  )
{-# LINE 338 "ccweb.org" #-}
instance Parse Headline where
  parse = do
    s <- P.try (char '*') *> P.many (char '*')
    _ <- spaces1
    t <- parse :: Parser Text
    return $ Headline (length s) t
{-# LINE 348 "ccweb.org" #-}
headlineLevel :: Headline -> Int
headlineLevel (Headline x _) = x
{-# LINE 356 "ccweb.org" #-}
instance Parse SourceBlock where
  parse = do
    --parserTrace "SourceBlock: trying to find a code block here"
    n  <- P.optionMaybe (
{-# LINE 384 "ccweb.org" #-}
                         P.try (spaces *> P.string "#+NAME:")
                         *> spaces
                         *> parse :: Parser Text
{-# LINE 359 "ccweb.org" #-}
                                                )
    i  <- P.try (spaces <* (P.string "#+BEGIN_SRC" *> spaces1))
    --parserTrace "SourceBlock: found a code block, parsing language"
    l  <- symbol <* spaces
    --parserTrace "SourceBlock: found a code block, parsing properties"
    ps <- 
{-# LINE 303 "ccweb.org" #-}
          line (Map.fromList
                <$> P.sepEndBy
                 (
{-# LINE 311 "ccweb.org" #-}
                  do
                    k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                    v <- parse :: Parser SExpr
                    return (k, v)
{-# LINE 305 "ccweb.org" #-}
                                 )
                 spaces1)
{-# LINE 365 "ccweb.org" #-}
    ps' <- Map.union ps . headerArgs . top . propertyStack <$> P.getState
    p  <- P.getPosition
    --parserTrace "SourceBlock: found a code block, parsing lines"
    ls <- P.manyTill (P.string i *> parse :: Parser CodeLine)
         (P.try (line (P.string i *> P.string "#+END_SRC" *> spaces)))
    --parserTrace "SourceBlock: code block parsed"
    state <- P.getState
    return $ SourceBlock
      { blockName = n
      , blockLanguage = l
      , blockProperties = ps
      , blockDerivedProperties = eval state ps'
      , blockLocation = p
      , blockLines = ls
      }
{-# LINE 396 "ccweb.org" #-}
emptyLine :: Parser ()
emptyLine = void $ spaces *> endOfLine

{-# LINE 399 "ccweb.org" #-}
skipEmptyLines :: Parser ()
skipEmptyLines = P.skipMany (P.try emptyLine)
{-# LINE 405 "ccweb.org" #-}
literalOr :: (String -> Parser a) -> Parser a -> Parser a
literalOr f p = scan "" where
  scan [] = p <|> (anyChar >>= scan . (:""))
  scan acc =
    ((P.lookAhead p) *> (f $ reverse acc))
    <|> (anyChar >>= scan . (:acc))
    <|> (f $ reverse acc)
{-# LINE 420 "ccweb.org" #-}
enclosed :: Char -> Parser String
enclosed d = P.try (P.char d) *> P.manyTill anyChar (P.char d)
{-# LINE 428 "ccweb.org" #-}
instance Parse Section where
  parse = do
    parserTrace "start of section parsing"

{-# LINE 432 "ccweb.org" #-}
    skipEmptyLines
    h <- P.optionMaybe parse :: Parser (Maybe Headline)

{-# LINE 435 "ccweb.org" #-}
    skipEmptyLines
    ps <- case h of
      Nothing -> return Map.empty
      _ -> parse <|> return Map.empty :: Parser Properties
    parserDebug "section properties" ps

{-# LINE 441 "ccweb.org" #-}
    when (isJust h) $ do
      stk' <- resize (1 + (headlineLevel $ fromJust h)) . propertyStack <$> P.getState
      P.updateState (\s -> s{ propertyStack = stk' })
      ps' <- Map.unionWith Map.union ps . top . propertyStack <$> P.getState
      P.updateState (\s -> s{ propertyStack = push ps' stk' })
    P.getState >>= parserDebug "updated parser properties" . propertyStack

{-# LINE 448 "ccweb.org" #-}
    skipEmptyLines
    ts  <- P.optionMaybe $ P.many1 $ parse :: Parser (Maybe [Text])

{-# LINE 451 "ccweb.org" #-}
    skipEmptyLines
    c <- case (ts, h) of
          (Nothing, Nothing) -> Just <$> (parse :: Parser SourceBlock)
          (_, _) -> P.optionMaybe $ parse :: Parser (Maybe SourceBlock)

{-# LINE 456 "ccweb.org" #-}
    skipEmptyLines

{-# LINE 458 "ccweb.org" #-}
    parserTrace "end of section parsing"

{-# LINE 460 "ccweb.org" #-}
    state <- P.getState
    let n = 1 + sectionCounter state
        ps' = top . propertyStack $ state
    P.updateState (\s -> s{ sectionCounter = n })
    return Section
      { sectionNumber = n
      , sectionHeadline = h
      , sectionProps = ps
      , sectionDerivedProperties = eval state ps'
      , documentation = fromMaybe [] ts
      , sectionSourceBlock = c
      }
{-# LINE 478 "ccweb.org" #-}
instance Parse Keyword where
  parse = propertyKeyword <|> titleKeyword <|> authorKeyword <|> otherKeyword
    where
      authorKeyword = do
        keywordStart "#+AUTHOR:"
        AuthorKeyword <$> parse
      propertyKeyword = do
        PropertyKeyword <$> 
{-# LINE 294 "ccweb.org" #-}
                            do
                              _ <- P.try (P.string "#+PROPERTY:") *> spaces
                              p <- symbol <* spaces1
                              kvs <- 
{-# LINE 303 "ccweb.org" #-}
                                     line (Map.fromList
                                           <$> P.sepEndBy
                                            (
{-# LINE 311 "ccweb.org" #-}
                                             do
                                               k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                                               v <- parse :: Parser SExpr
                                               return (k, v)
{-# LINE 305 "ccweb.org" #-}
                                                            )
                                            spaces1)
{-# LINE 298 "ccweb.org" #-}
                              return $ Map.singleton p kvs
{-# LINE 486 "ccweb.org" #-}
      titleKeyword = do
        keywordStart "#+TITLE:"
        TitleKeyword <$> parse
      otherKeyword = do
        _ <- P.try $ P.string "#+"
        t <- P.many anyChar
        _ <- endOfLine
        return $ OtherKeyword t
      keywordStart h = void $ P.try (P.string h *> spaces)
{-# LINE 499 "ccweb.org" #-}
headerProperties :: [Keyword] -> Properties
headerProperties = foldl acc mempty where
    acc a (PropertyKeyword p) = Map.union p a
    acc a _ = a
{-# LINE 509 "ccweb.org" #-}
instance Parse Document where
  parse = do
    --parserTrace "Document: Parsing keywords"
    hs <- P.many parse :: Parser [Keyword]
    P.updateState $ \s -> s{ propertyStack = Stack [headerProperties hs] }
    skipEmptyLines
    --parserTrace "Document: Parsing sections"
    ss <- P.many parse :: Parser [Section]
    skipEmptyLines *> P.eof
    return Document
      { keywords = hs
      , sections = ss
      }
{-# LINE 526 "ccweb.org" #-}
readOrgFile :: LogLevel -> FilePath -> IO (OrgLines, Document)
readOrgFile lvl fp = do
  ingested <- fromList <$> ingest fp :: IO OrgLines
  h <- getHostName
  let doc = fromEither $ P.runParser
        (parse :: Parser Document)
        initialParserState{ hostName = h, parserLogLevel = lvl }
        fp
        ingested
  return (ingested, doc)
{-# LINE 43 "org/scaffold.org" #-}
data Options = Options
  { optionQuiet :: Bool
  , optionVerbosity :: Int
  , dryRun :: Bool
  , listInputFiles :: Bool
  , listOutputFiles :: Bool
  , inputFile :: String
  }

{-# LINE 52 "org/scaffold.org" #-}
instance Pretty Options where
  pretty o = pretty $ PrettyStruct "Options"
    [ ("quiet", pretty $ optionQuiet o)
    , ("verbosity", pretty $ optionVerbosity o)
    , ("dry-run", pretty $ dryRun o)
    , ("list-input-files", pretty $ listInputFiles o)
    , ("list-output-files", pretty $ listOutputFiles o)
    , ("input file", pretty $ inputFile o)
    ]

{-# LINE 62 "org/scaffold.org" #-}
userOptionParser :: O.Parser Options
userOptionParser = Options
  <$> (
{-# LINE 87 "org/scaffold.org" #-}
       O.switch
         ( O.short 'q'
           <> O.long "quiet"
           <> O.help "Only print things that were asked for."
         )
{-# LINE 64 "org/scaffold.org" #-}
          )
  <*> (
{-# LINE 96 "org/scaffold.org" #-}
       length
       <$>
       O.many
         ( O.flag' ()
           ( O.short 'v'
             <> O.long "verbose"
             <> O.help "Be verbose. Can be given multiple times for more verbosity."
           ))
{-# LINE 65 "org/scaffold.org" #-}
             )
  <*> dryRunParser
  <*> 
{-# LINE 125 "org/scaffold.org" #-}
      O.switch
        ( O.short 'I'
        <> O.long "list-input-files"
        <> O.help "If given, list the files that have to be read in, one per line."
        )
{-# LINE 68 "org/scaffold.org" #-}
  <*> 
{-# LINE 116 "org/scaffold.org" #-}
      O.switch
        ( O.short 'O'
        <> O.long "list-output-files"
        <> O.help "If given, list the files that would be written out, one per line."
        )
{-# LINE 69 "org/scaffold.org" #-}
  <*> (
{-# LINE 108 "org/scaffold.org" #-}
       O.argument O.str
         ( O.metavar "FILE"
         <> O.help "The name of the input file."
         )
{-# LINE 69 "org/scaffold.org" #-}
          )
{-# LINE 74 "org/scaffold.org" #-}
logLevel :: Options -> LogLevel
logLevel Options{ optionQuiet = q, optionVerbosity = v } =
  case (if q then 0 else 3) + v of
    0 -> Quiet
    1 -> Error
    2 -> Warning
    3 -> Info
    4 -> Debug
    _ -> Trace
{-# LINE 134 "org/scaffold.org" #-}
dryRunParser :: O.Parser Bool
dryRunParser = O.switch
  ( O.short 'n'
  <> O.long "dry-run"
  <> O.help "If given, do not write any files. Useful when debugging."
  )
{-# LINE 123 "org/parser.org" #-}
fromEither :: Pretty a => Either a b -> b
fromEither = either (error . PP.render . pretty) id
{-# LINE 10 "org/tangle.org" #-}
main :: IO ()
main = do
  opts <- O.execParser optParser
  logM (logLevel opts) Info $ "This is CCTANGLE"

{-# LINE 15 "org/tangle.org" #-}
  (ingested, doc) <- readOrgFile (logLevel opts) (inputFile opts)
  logM (logLevel opts) Debug $ PP.render (pretty ingested)
  logM (logLevel opts) Debug $ PP.render (pretty doc)

{-# LINE 19 "org/tangle.org" #-}
  home <- Env.getEnv "HOME"
  let outs = 
{-# LINE 39 "org/tangle.org" #-}
             (map (\(fp,b) -> (case fp of { ('~':xs) -> home ++ xs; _ -> fp }, b))
                  :: [(FilePath, [SourceBlock])] -> [(FilePath, [SourceBlock])])
               . (map (\(f,ss) -> (f, map (fromJust . sectionSourceBlock) ss))
                  :: [(FilePath, [Section])] -> [(FilePath, [SourceBlock])])
               . (mapMaybe (\(i,ss) -> case i of; (FileBlock f) -> Just (f,ss); _ -> Nothing) . toList
                  :: DocumentPartition -> [(FilePath, [Section])])
               . (
{-# LINE 389 "org/doc.org" #-}
                  ((Map.fromList . map (\bs -> (fst (head bs), map snd bs)))
                       :: [[(SourceBlockId, Section)]] -> DocumentPartition)
                    . (groupWith fst
                       :: [(SourceBlockId, Section)] -> [[(SourceBlockId, Section)]])
                    . (sortWith fst
                       :: [(SourceBlockId, Section)] -> [(SourceBlockId, Section)])
                    . (mapMaybe (\s -> sectionSourceBlock s >>= sourceBlockId >>= Just . (,s))
                       :: [Section] -> [(SourceBlockId, Section)])
                    . sections
{-# LINE 46 "org/tangle.org" #-}
                  :: Document -> DocumentPartition)
               $ doc
{-# LINE 20 "org/tangle.org" #-}
                     :: [(FilePath, [SourceBlock])]
{-# LINE 56 "org/tangle.org" #-}
  when (listOutputFiles opts) $ mapM_ putStrLn (map fst outs)
  when (listInputFiles opts)  $ mapM_ putStrLn (
    nub . sort .
    map (\(OrgLine p _) -> P.sourceName p) $ (toList ingested)
    )
{-# LINE 22 "org/tangle.org" #-}
  logM (logLevel opts) Debug $ PP.render (pretty outs)

{-# LINE 24 "org/tangle.org" #-}
  mapM_ (tangleFile opts doc) outs
  logM (logLevel opts) Info $ "Done."
  where
    optParser = O.info (userOptionParser <**> O.helper)
      ( O.fullDesc
      <> O.progDesc "Tangle the input FILE"
      <> O.header "cctangle - A literate programming tangler" )
{-# LINE 70 "org/tangle.org" #-}
data Line = CodeText String | LinePragma String P.SourcePos
{-# LINE 78 "org/tangle.org" #-}
instance Show Line where
  show (CodeText str) = str ++ "\n"
{-# LINE 88 "org/tangle.org" #-}
  show (LinePragma "haskell" pos) = unwords
    [ "{-# LINE", show (P.sourceLine pos)
    , "\"" ++ P.sourceName pos ++ "\""
    , "#-}\n" ]
{-# LINE 98 "org/tangle.org" #-}
  show (LinePragma "c" pos) = unwords
    [ "#line", show (P.sourceLine pos)
    , "\"" ++ P.sourceName pos ++ "\"\n"
    ]
  show (LinePragma "c++" pos) = show (LinePragma "c" pos)
{-# LINE 81 "org/tangle.org" #-}
  show (LinePragma _ _) = []
{-# LINE 130 "org/tangle.org" #-}
type Indent = String
{-# LINE 141 "org/tangle.org" #-}
data TangleState = TangleState
  { document :: Document
  , indent :: Indent
  , blocks :: Stack SourceBlock
  , pragmaLanguage :: String
  }
{-# LINE 154 "org/tangle.org" #-}
type Tangler a = State TangleState a

{-# LINE 156 "org/tangle.org" #-}
getTopBlock :: Tangler SourceBlock
getTopBlock = top . blocks <$> get

{-# LINE 159 "org/tangle.org" #-}
getIndent :: Tangler Indent
getIndent = indent <$> get

{-# LINE 162 "org/tangle.org" #-}
setIndentFrom :: String -> Tangler ()
setIndentFrom str = modify
  (\s -> s{ indent = replicate (length str) ' ' })

{-# LINE 166 "org/tangle.org" #-}
addIndentFrom :: String -> Tangler ()
addIndentFrom str = modify
  (\s -> s{ indent = indent s ++ replicate (length str) ' ' })
{-# LINE 177 "org/tangle.org" #-}
tangleSourceBlock :: SourceBlock -> Tangler [Line]
tangleSourceBlock block = do
  modify (\s -> s{ blocks = push block (blocks s) })
  ls <- concat <$> mapM tangleCodeLine (blockLines block)
  modify (\s -> s{ blocks = pop (blocks s) })
  maybeAddLinePragma ls (blockLocation block) block
{-# LINE 187 "org/tangle.org" #-}
maybeAddLinePragma :: [Line] -> P.SourcePos -> SourceBlock -> Tangler [Line]
maybeAddLinePragma ls pos block = do
  lang <- pragmaLanguage <$> get
  let pragma = LinePragma lang pos
  return $ case (headerArg ":comments" block, show pragma) of
    (Just (BoolAtom True), (_:_)) -> pragma : ls
    _ -> ls
{-# LINE 202 "org/tangle.org" #-}
tangleCodeLine :: CodeLine -> Tangler [Line]
tangleCodeLine (CodeLine _ []) = (:[]) . CodeText <$> getIndent
{-# LINE 231 "org/tangle.org" #-}
tangleCodeLine (CodeLine _ elements) =
  do
    initialIndent <- getIndent
    ls <- removeLeadingEmptyLine . 
{-# LINE 224 "org/tangle.org" #-}
                                   reverse <$> foldlM
                                     (
{-# LINE 267 "org/tangle.org" #-}
                                      \acc element -> do
                                        let (accPragmas, accRest) = breakCodeText acc
                                        case (accPragmas, accRest, element) of
{-# LINE 280 "org/tangle.org" #-}
                                          ([], (CodeText l:ls), Literal _ s) -> do
                                            addIndentFrom s
                                            return $ CodeText (l ++ s) : ls
{-# LINE 292 "org/tangle.org" #-}
                                          (_, _, Literal pos s) -> do
                                            i <- getIndent
                                            addIndentFrom s
                                            getTopBlock >>= maybeAddLinePragma (CodeText (i ++ s) : acc) pos
{-# LINE 311 "org/tangle.org" #-}
                                          (_, (CodeText l:ls), SectionReference pos name) -> do
                                            refLines <- 
{-# LINE 329 "org/tangle.org" #-}
                                                        do
                                                          bs <- 
{-# LINE 337 "org/tangle.org" #-}
                                                                filter (\b -> case (blockName b) of
                                                                               Nothing -> False
                                                                               Just t -> t == name)
                                                                  . mapMaybe sectionSourceBlock
                                                                  . sections
                                                                  . document
                                                                  <$> get
{-# LINE 330 "org/tangle.org" #-}
                                                                          :: Tangler [SourceBlock]
                                                          when (null bs) (error $ "source block not defined anywhere: " ++ show name)
                                                          concat <$> mapM tangleSourceBlock bs :: Tangler [Line]
{-# LINE 313 "org/tangle.org" #-}
                                            let (refPragmas, (CodeText first : rest)) = breakCodeText refLines
                                            acc' <- case (refPragmas, accPragmas) of
                                              ([], []) -> do
                                                unindented <- 
{-# LINE 348 "org/tangle.org" #-}
                                                              (\str -> do
                                                                 i <- getIndent
                                                                 return $ if isPrefixOf i str; then drop (length i) str; else str
                                                              )
{-# LINE 316 "org/tangle.org" #-}
                                                                first
                                                addIndentFrom unindented
                                                return $ reverse rest ++ (CodeText(l ++ unindented) : ls)
                                              _ -> do
                                                let reverseRefLines = reverse refLines
                                                    CodeText lst = head . snd $ breakCodeText reverseRefLines
                                                setIndentFrom lst
                                                return $ reverseRefLines ++ acc
                                            getTopBlock >>= maybeAddLinePragma acc' pos
{-# LINE 271 "org/tangle.org" #-}
                                          _ -> error $ "unreachable element tangling case"
{-# LINE 225 "org/tangle.org" #-}
                                                                                          )
                                     [CodeText initialIndent] elements
{-# LINE 235 "org/tangle.org" #-}
    modify (\s -> s{ indent = initialIndent })
    getTopBlock >>= maybeAddLinePragma ls ((
{-# LINE 247 "org/tangle.org" #-}
                                            \case
                                              (Literal p _) -> p
                                              (SectionReference p _) -> p
{-# LINE 236 "org/tangle.org" #-}
                                                                         ) $ head elements)
      where
        removeLeadingEmptyLine :: [Line] -> [Line]
        removeLeadingEmptyLine a@(CodeText l:p@(LinePragma _ _):ls)
          | all isSpace l = p:ls
          | otherwise = a
        removeLeadingEmptyLine a = a
{-# LINE 257 "org/tangle.org" #-}
breakCodeText :: [Line] -> ([Line],[Line])
breakCodeText = break (\case { (CodeText _) -> True; _ -> False })
{-# LINE 359 "org/tangle.org" #-}
tangleFile :: Options -> Document -> (FilePath, [SourceBlock]) -> IO ()
tangleFile opts _ (fp, []) =
  logM (logLevel opts) Warning $ "Not writing empty output file (" ++ fp ++ ")"
tangleFile opts doc (fp, bs) = do
  let block = head bs
      ls = concatMap
           (\b -> fst $ runState (tangleSourceBlock b)
                 TangleState{ document = doc
                            , indent = []
                            , blocks = Stack []
                            , pragmaLanguage = blockLanguage block }
           )
           bs
      contents = concatMap show $ 
{-# LINE 112 "org/tangle.org" #-}
                                  foldr (\x xs ->
                                           let redundant (p1,n,p2) =
                                                 P.sourceName p1 == P.sourceName p2
                                                 && P.sourceLine p1 + n == P.sourceLine p2
                                           in case (x:xs) of
                                                (LinePragma _ _ : LinePragma _ _ : _) -> xs
                                                (LinePragma _ p1 : l1 : LinePragma _ p2 : xs') ->
                                                  if redundant (p1,1,p2); then x : l1 : xs'; else (x:xs)
                                                _ -> (x:xs))
                                        []
{-# LINE 372 "org/tangle.org" #-}
                                           ls
  logM (logLevel opts) Info $ "Writing the output file (" ++ fp ++ ")..."
  if dryRun opts
    then logM (logLevel opts) Info contents
    else do
      let dir = F.takeDirectory fp
      when (mkDir && dir /= ".") $ D.createDirectoryIfMissing True dir

{-# LINE 380 "org/tangle.org" #-}
      F.fileExist fp >>= (`when` F.removeLink fp)
      writeFile fp contents

{-# LINE 383 "org/tangle.org" #-}
      case headerArg ":tangle-mode" block of
        Nothing -> return ()
        Just (IntAtom mode) -> F.setFileMode fp $ fromIntegral mode
        Just e -> error $ "unsupported tangle-mode: " ++ show e
  where
    mkDir = case headerArg ":mkdirp" (head bs) of
              Just (BoolAtom x) -> x
              _ -> False
{-# LINE 5 "org/tangle.org" #-}
