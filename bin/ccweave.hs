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
{-# LINE 388 "org/tangle.org" #-}
import qualified System.Directory as D
import qualified System.Environment as Env
import qualified System.FilePath as F
import qualified System.Posix.Files as F
{-# LINE 123 "ccweb.org" #-}
import qualified Text.PrettyPrint as PP
{-# LINE 227 "ccweb.org" #-}
import qualified Options.Applicative as O
{-# LINE 342 "ccweb.org" #-}
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
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
{-# LINE 133 "ccweb.org" #-}
class Pretty a where
  pretty :: a -> PP.Doc
  prettyList :: [a] -> PP.Doc
  prettyList = PP.brackets . PP.fcat . PP.punctuate PP.comma . map pretty

{-# LINE 138 "ccweb.org" #-}
instance Pretty PP.Doc where
  pretty = id

{-# LINE 141 "ccweb.org" #-}
instance Pretty Bool where
  pretty True = PP.text "True"
  pretty False = PP.text "False"

{-# LINE 145 "ccweb.org" #-}
instance Pretty Char where
  pretty = PP.char
  prettyList = PP.text

{-# LINE 149 "ccweb.org" #-}
instance Pretty Int where
  pretty = PP.int

{-# LINE 152 "ccweb.org" #-}
instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = PP.text "Nothing"
  pretty (Just a) = pretty a

{-# LINE 156 "ccweb.org" #-}
instance (Pretty a, Pretty b) => Pretty (a,b) where
  pretty (a,b) = PP.parens . PP.fcat . PP.punctuate PP.comma $ [pretty a, pretty b]

{-# LINE 159 "ccweb.org" #-}
instance Pretty a => Pretty [a] where
  pretty = prettyList

{-# LINE 162 "ccweb.org" #-}
instance Pretty P.ParseError where
  pretty = PP.text . show
{-# LINE 168 "ccweb.org" #-}
instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
  pretty = PP.brackets
           . PP.fcat
           . PP.punctuate PP.comma
           . map (\(k,v) -> pretty k PP.<> PP.equals PP.<> pretty v)
           . Map.toList
{-# LINE 178 "ccweb.org" #-}
data PrettyStruct = PrettyStruct String [(String, PP.Doc)]

{-# LINE 180 "ccweb.org" #-}
instance Pretty PrettyStruct where
  pretty (PrettyStruct t ps) =
    let fields = map (\(n,p) -> PP.text n PP.<> PP.colon PP.<+> p) ps
    in PP.text t PP.<> PP.braces (PP.fcat $ PP.punctuate PP.comma fields)

{-# LINE 185 "ccweb.org" #-}
prettyStruct :: String -> [(String, (a -> PP.Doc))] -> a -> PP.Doc
prettyStruct name kfs a = pretty (PrettyStruct name (map (\(k,f) -> (k,f a)) kfs))
{-# LINE 191 "ccweb.org" #-}
instance Pretty P.SourcePos where
  pretty p = pretty (P.sourceName p)
             PP.<> PP.colon PP.<> pretty (P.sourceLine p)
             PP.<> PP.colon PP.<> pretty (P.sourceColumn p)
{-# LINE 462 "ccweb.org" #-}
instance Pretty OrgLine where
  pretty (OrgLine p l) =
    pretty p
    PP.<> PP.colon
    PP.<> PP.text (takeWhile (/= '\NUL') l)
{-# LINE 512 "ccweb.org" #-}
instance Pretty OrgFile where
  pretty (OrgFile ls) = pretty ls
{-# LINE 626 "ccweb.org" #-}
instance Pretty TextElement where
  pretty (Bold a) = PP.text "Bold:" PP.<+> pretty a
  pretty (InlineCode a) = PP.text "InlineCode:" PP.<+> pretty a
  pretty (Italics a) = PP.text "Italics:" PP.<+> pretty a
  pretty (Plain a) = pretty a
  pretty (StrikeThrough a) = PP.text "StrikeThrough:" PP.<+> pretty a
  pretty (TeXMath a) = PP.text "TeXMath:" PP.<+> pretty a
  pretty (Verbatim a) = PP.text "Verbatim:" PP.<+> pretty a
{-# LINE 656 "ccweb.org" #-}
instance Pretty Text where
  pretty (Text xs) = PP.text "Text:" PP.<+> PP.hcat (map pretty xs)
{-# LINE 677 "ccweb.org" #-}
instance Pretty CodeElement where
  pretty (Literal p s) = PP.parens $ pretty p PP.<> PP.colon PP.<> pretty s
  pretty (SectionReference p t) = pretty p PP.<> PP.colon PP.<> PP.char '«' PP.<> pretty t PP.<> PP.char '»'
{-# LINE 718 "ccweb.org" #-}
instance Pretty CodeLine where
  pretty (CodeLine p xs) = pretty p PP.<> PP.colon PP.<> pretty xs
{-# LINE 201 "ccweb.org" #-}
data LogLevel = Quiet | Error | Warning | Info | Debug | Trace deriving (Eq,Show)

{-# LINE 203 "ccweb.org" #-}
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

{-# LINE 216 "ccweb.org" #-}
instance Pretty LogLevel where
  pretty = PP.text . show

{-# LINE 219 "ccweb.org" #-}
logM :: Pretty a => LogLevel -> LogLevel -> a -> IO ()
logM lvl minLvl = when (lvl >= minLvl) . putStrLn . PP.render . pretty
{-# LINE 88 "ccweb.org" #-}
newtype Stack a = Stack [a]

{-# LINE 90 "ccweb.org" #-}
instance Pretty a => Pretty (Stack a) where
  pretty (Stack xs) = pretty xs

{-# LINE 93 "ccweb.org" #-}
top :: Stack a -> a
top (Stack xs) = head xs

{-# LINE 96 "ccweb.org" #-}
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack $ x:xs

{-# LINE 99 "ccweb.org" #-}
pop :: Stack a -> Stack a
pop (Stack []) = error $ "popping empty stack"
pop (Stack (_:xs)) = Stack xs

{-# LINE 103 "ccweb.org" #-}
resize :: Int -> Stack a -> Stack a
resize i (Stack xs)
  | i == l = Stack xs
  | i < l = Stack $ reverse . take i . reverse $ xs
  | i > l && l > 0 = Stack $ replicate (i - l) (head xs) ++ xs
  | otherwise = error $ "resizing empty stack"
  where
    l = length xs
{-# LINE 353 "ccweb.org" #-}
type Parser = P.Parsec OrgFile ParserState

{-# LINE 355 "ccweb.org" #-}
class Parse a where
  parse :: Parser a
{-# LINE 361 "ccweb.org" #-}
data ParserState = ParserState
  { sectionCounter :: Int
  , propertyStack  :: Stack Properties
  , hostName       :: String
  , parserLogLevel :: LogLevel
  }

{-# LINE 368 "ccweb.org" #-}
instance Pretty ParserState where
  pretty s = pretty $ PrettyStruct "ParserState"
      [ ("counter", pretty (sectionCounter s))
      , ("host", pretty (hostName s))
      , ("log level", pretty (parserLogLevel s))
      , ("property stack", pretty (propertyStack s))
      ]

{-# LINE 376 "ccweb.org" #-}
initialParserState :: ParserState
initialParserState = ParserState
  { sectionCounter = 0
  , propertyStack  = Stack []
  , parserLogLevel = Debug
  , hostName       = []
  }
{-# LINE 387 "ccweb.org" #-}
parserDebug :: (Pretty a, Pretty b) => a -> b -> Parser ()
parserDebug l s = do
  lvl <- parserLogLevel <$> P.getState
  pos <- P.getPosition
  let label = pretty pos PP.<> PP.colon PP.<> pretty l PP.<> PP.colon
  if lvl >= Trace
  then trace (PP.render (PP.hang label 4 (pretty s))) $ return ()
  else return ()

{-# LINE 396 "ccweb.org" #-}
parserTrace :: Pretty a => a -> Parser ()
parserTrace l = do
  (OrgFile ls) <- P.stateInput <$> P.getParserState
  u <- P.getState
  let user = pretty "user state" PP.<> PP.colon PP.<> pretty u
      input = pretty "looking at" PP.<> PP.colon PP.<> pretty (take 3 ls)
  parserDebug l (PP.fcat [user, input])
{-# LINE 409 "ccweb.org" #-}
class Location a where
  newPosition :: P.SourcePos -> Char -> a -> P.SourcePos
  getPosition :: a -> P.SourcePos
  getPosition = error $ "getPosition not implemented"
{-# LINE 417 "ccweb.org" #-}
instance Location String where
  newPosition pos c _cs = P.updatePosChar pos c
{-# LINE 425 "ccweb.org" #-}
data OrgLine = OrgLine P.SourcePos String

{-# LINE 427 "ccweb.org" #-}
instance Location OrgLine where
  getPosition (OrgLine p _) = p
  newPosition _ _ (OrgLine p _) = p
{-# LINE 446 "ccweb.org" #-}
instance Monad m => P.Stream OrgLine m Char where
  -- The actual end of the stream
  uncons (
{-# LINE 441 "ccweb.org" #-}
          OrgLine _ ('\NUL':_)
{-# LINE 448 "ccweb.org" #-}
                              ) =
    return Nothing
  -- An empty string --- insert last newline
  uncons (OrgLine p []) =
    return $ Just ('\n', OrgLine (P.updatePosChar p '\n') "\NUL")
  -- Uncons a character
  uncons (OrgLine p (x:xs)) =
    return $ Just (x, OrgLine (P.updatePosChar p x) xs)
{-# LINE 474 "ccweb.org" #-}
newtype OrgFile = OrgFile [OrgLine]
{-# LINE 483 "ccweb.org" #-}
instance Monad m => P.Stream OrgFile m Char where
  uncons (OrgFile (
{-# LINE 441 "ccweb.org" #-}
                   OrgLine _ ('\NUL':_)
{-# LINE 484 "ccweb.org" #-}
                                       :[])) = return Nothing
  uncons (OrgFile (x:xs)) = P.uncons x >>= \case
    Nothing -> P.uncons (fromList xs :: OrgFile)
    Just (x',xs') -> return $ Just (x', OrgFile (xs':xs))
  uncons (OrgFile []) =
    error $ "(internal): uncons of empty OrgFile instance"
{-# LINE 494 "ccweb.org" #-}
instance Location OrgFile where
  getPosition (OrgFile (x:_)) = getPosition x
  getPosition _ = error $ "(internal) getPosition of empty OrgFile"

{-# LINE 498 "ccweb.org" #-}
  newPosition pos c (OrgFile (x:_)) = newPosition pos c x
  newPosition _ _ (OrgFile []) = error $ "(internal): location of empty OrgFile"
{-# LINE 504 "ccweb.org" #-}
instance IsList OrgFile where
  type Item OrgFile = OrgLine
  fromList xs = OrgFile xs
  toList (OrgFile xs) = xs
{-# LINE 524 "ccweb.org" #-}
satisfy :: (Location s, P.Stream s m Char) => (Char -> Bool) -> P.ParsecT s u m Char
satisfy f = P.tokenPrim
            (\c -> show [c])
            (\pos c cs -> newPosition pos c cs)
            (\c -> if f c then Just c else Nothing)
{-# LINE 539 "ccweb.org" #-}
char :: (Location s, P.Stream s m Char) => Char -> P.ParsecT s u m Char
char c = satisfy (==c) P.<?> show [c]

{-# LINE 542 "ccweb.org" #-}
newline :: (Location s, P.Stream s m Char) => P.ParsecT s u m Char
newline = char '\n' P.<?> "lf new-line"

{-# LINE 545 "ccweb.org" #-}
crlf :: (Location s, P.Stream s m Char) => P.ParsecT s u m Char
crlf = char '\r' *> char '\n' P.<?> "crlf new-line"

{-# LINE 548 "ccweb.org" #-}
endOfLine :: (Location s, P.Stream s m Char) => P.ParsecT s u m Char
endOfLine = newline <|> crlf P.<?> "new-line"
{-# LINE 558 "ccweb.org" #-}
anyChar :: (Location s, P.Stream s m Char) => P.ParsecT s u m Char
anyChar = P.noneOf "\n\r"

{-# LINE 561 "ccweb.org" #-}
space :: P.Stream s m Char => P.ParsecT s u m Char
space = P.satisfy (\c -> isSpace c && notElem c "\n\r")

{-# LINE 564 "ccweb.org" #-}
spaces :: P.Stream s m Char => P.ParsecT s u m String
spaces = P.many space

{-# LINE 567 "ccweb.org" #-}
spaces1 :: P.Stream s m Char => P.ParsecT s u m String
spaces1 = P.many1 space
{-# LINE 573 "ccweb.org" #-}
symbolChar :: P.Stream s m Char => P.ParsecT s u m Char
symbolChar = P.satisfy (\c -> isPrint c && not (isSpace c) && notElem c "()=")

{-# LINE 576 "ccweb.org" #-}
symbol :: P.Stream s m Char => P.ParsecT s u m String
symbol = P.many1 symbolChar
{-# LINE 582 "ccweb.org" #-}
line :: Parser a -> Parser a
line p = p <* endOfLine
{-# LINE 588 "ccweb.org" #-}
fromEither :: Pretty a => Either a b -> b
fromEither = either (error . PP.render . pretty) id

{-# LINE 591 "ccweb.org" #-}
ingest :: FilePath -> IO [OrgLine]
ingest fp = do
  ls <- fromEither
       <$> P.parse (P.many1 $ 
{-# LINE 610 "ccweb.org" #-}
                              liftA2 OrgLine P.getPosition (P.manyTill anyChar endOfLine)
{-# LINE 594 "ccweb.org" #-}
                                                                                         ) fp
       <$> readFile fp
  foldrM scan [] ls
  where
    scan l@(OrgLine p s) acc =
      case fromEither
           $ P.parse
           (P.setPosition p *> 
{-# LINE 615 "ccweb.org" #-}
                               P.optionMaybe (
                                 P.try (P.spaces *> P.string "#+INCLUDE:" *> P.spaces)
                                 *> P.char '"'
                                 *> P.manyTill anyChar (P.char '"')
                               )
{-# LINE 601 "ccweb.org" #-}
                                )
           (P.sourceName p) s
      of
        Nothing -> return (l : acc)
        Just fp' -> (++ acc) <$> ingest fp'
{-# LINE 638 "ccweb.org" #-}
instance Parse TextElement where
  parse = literalOr (return . Plain) (
{-# LINE 644 "ccweb.org" #-}
                                      P.try (Bold <$> enclosed '*')
                                      <|> P.try (InlineCode <$> enclosed '~')
                                      <|> P.try (Italics <$> enclosed '/')
                                      <|> P.try (StrikeThrough <$> enclosed '+')
                                      <|> P.try (TeXMath <$> enclosed '$')
                                      <|> P.try (Verbatim <$> enclosed '=')
{-# LINE 639 "ccweb.org" #-}
                                                                           )
{-# LINE 664 "ccweb.org" #-}
instance Parse Text where
  parse = do
    --parserTrace "Text: parse"
    P.notFollowedBy (parse :: Parser Headline)
    P.notFollowedBy (spaces *> P.string "#+NAME")
    P.notFollowedBy (spaces *> P.string "#+BEGIN_SRC")
    Text <$> line (P.many1 parse :: Parser [TextElement])
{-# LINE 684 "ccweb.org" #-}
instance Location CodeElement where
  getPosition (Literal p _) = p
  getPosition (SectionReference p _) = p

{-# LINE 688 "ccweb.org" #-}
  newPosition _ _ _ = error $ "(internal) newPosition of CodeElement"
{-# LINE 693 "ccweb.org" #-}
instance Parse CodeElement where
  parse = literalOr
              (liftA2 Literal P.getPosition . return)
              (P.try $ 
{-# LINE 701 "ccweb.org" #-}
                       do
                         p <- P.try (P.string "<<") *> P.getPosition
                         --parserTrace "Trying to parse a code reference after '<<'"
                         l <- OrgLine p <$> P.manyTill anyChar (P.try $ P.string ">>")
                         let t = fromEither
                               $ P.runParser
                               (parse :: Parser Text)
                               initialParserState
                               (P.sourceName p)
                               (fromList [l] :: OrgFile)
                             r = SectionReference p t
                         --parserTrace $ PP.text "Reference parsed" PP.<> PP.colon PP.<> pretty r
                         return r
{-# LINE 696 "ccweb.org" #-}
                                 )
{-# LINE 724 "ccweb.org" #-}
instance Parse CodeLine where
  parse = line (liftA2 CodeLine
                     P.getPosition
                     (P.many parse :: Parser [CodeElement]))
{-# LINE 734 "ccweb.org" #-}
instance Parse SExpr where
  parse =
    (
{-# LINE 745 "ccweb.org" #-}
     SExpr <$> ( P.try (P.char '(')
                  *> spaces
                  *> (P.sepEndBy parse spaces1 <* P.char ')')
                )
{-# LINE 736 "ccweb.org" #-}
                 )
    <|>
    P.try (
{-# LINE 753 "ccweb.org" #-}
           BoolAtom <$> (
             (P.string "no" *> P.lookAhead P.space *> return False)
             <|>
             (P.string "yes" *> P.lookAhead P.space *> return True)
             )
{-# LINE 738 "ccweb.org" #-}
              )
    <|>
    (
{-# LINE 762 "ccweb.org" #-}
     Atom <$> (enclosed '"' <|> symbol)
{-# LINE 740 "ccweb.org" #-}
                                       )
{-# LINE 769 "ccweb.org" #-}
instance Parse Properties where
  parse = (
{-# LINE 776 "ccweb.org" #-}
           do
             _ <- P.try (P.string "#+PROPERTY:") *> spaces
             p <- symbol <* spaces1
             kvs <- 
{-# LINE 785 "ccweb.org" #-}
                    line (Map.fromList
                          <$> P.sepEndBy
                           (
{-# LINE 793 "ccweb.org" #-}
                            do
                              k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                              v <- parse :: Parser SExpr
                              return (k, v)
{-# LINE 787 "ccweb.org" #-}
                                           )
                           spaces1)
{-# LINE 780 "ccweb.org" #-}
             return $ Map.singleton p kvs
{-# LINE 770 "ccweb.org" #-}
                                         )
              <|> (
{-# LINE 801 "ccweb.org" #-}
                   do
                     _ <- P.try (spaces *> P.string ":PROPERTIES:" *> spaces *> endOfLine)
                     Map.fromList <$> P.manyTill
                       (
{-# LINE 810 "ccweb.org" #-}
                        do
                          p <- spaces *> (enclosed ':' <* spaces)
                          kvs <- 
{-# LINE 785 "ccweb.org" #-}
                                 line (Map.fromList
                                       <$> P.sepEndBy
                                        (
{-# LINE 793 "ccweb.org" #-}
                                         do
                                           k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                                           v <- parse :: Parser SExpr
                                           return (k, v)
{-# LINE 787 "ccweb.org" #-}
                                                        )
                                        spaces1)
{-# LINE 813 "ccweb.org" #-}
                          return (p, kvs)
{-# LINE 804 "ccweb.org" #-}
                                         )
                       (P.try (spaces *> P.string ":END:" *> spaces *> endOfLine))
{-# LINE 771 "ccweb.org" #-}
                                                                                  )
{-# LINE 820 "ccweb.org" #-}
instance Parse Headline where
  parse = do
    s <- P.try (char '*') *> P.many (char '*')
    _ <- spaces1
    t <- parse :: Parser Text
    return $ Headline (length s) t
{-# LINE 830 "ccweb.org" #-}
headlineLevel :: Headline -> Int
headlineLevel (Headline x _) = x
{-# LINE 838 "ccweb.org" #-}
instance Parse SourceBlock where
  parse = do
    --parserTrace "SourceBlock: trying to find a code block here"
    n  <- P.optionMaybe (
{-# LINE 866 "ccweb.org" #-}
                         P.try (spaces *> P.string "#+NAME:")
                         *> spaces
                         *> parse :: Parser Text
{-# LINE 841 "ccweb.org" #-}
                                                )
    i  <- P.try (spaces <* (P.string "#+BEGIN_SRC" *> spaces1))
    --parserTrace "SourceBlock: found a code block, parsing language"
    l  <- symbol <* spaces
    --parserTrace "SourceBlock: found a code block, parsing properties"
    ps <- 
{-# LINE 785 "ccweb.org" #-}
          line (Map.fromList
                <$> P.sepEndBy
                 (
{-# LINE 793 "ccweb.org" #-}
                  do
                    k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                    v <- parse :: Parser SExpr
                    return (k, v)
{-# LINE 787 "ccweb.org" #-}
                                 )
                 spaces1)
{-# LINE 847 "ccweb.org" #-}
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
{-# LINE 878 "ccweb.org" #-}
emptyLine :: (Location s, P.Stream s m Char) => P.ParsecT s u m ()
emptyLine = void $ spaces *> endOfLine

{-# LINE 881 "ccweb.org" #-}
skipEmptyLines :: (Location s, P.Stream s m Char) => P.ParsecT s u m ()
skipEmptyLines = P.skipMany (P.try emptyLine)
{-# LINE 887 "ccweb.org" #-}
literalOr :: (Location s, P.Stream s m Char) => (String -> P.ParsecT s u m a) -> P.ParsecT s u m a -> P.ParsecT s u m a
literalOr f p = scan "" where
  scan [] = p <|> (anyChar >>= scan . (:""))
  scan acc =
    ((P.lookAhead p) *> (f $ reverse acc))
    <|> (anyChar >>= scan . (:acc))
    <|> (f $ reverse acc)
{-# LINE 902 "ccweb.org" #-}
enclosed :: (Location s, P.Stream s m Char) => Char -> P.ParsecT s u m String
enclosed d = P.try (P.char d) *> P.manyTill anyChar (P.char d)
{-# LINE 910 "ccweb.org" #-}
instance Parse Section where
  parse = do
    parserTrace "start of section parsing"

{-# LINE 914 "ccweb.org" #-}
    skipEmptyLines
    h <- P.optionMaybe parse :: Parser (Maybe Headline)

{-# LINE 917 "ccweb.org" #-}
    skipEmptyLines
    ps <- case h of
      Nothing -> return Map.empty
      _ -> parse <|> return Map.empty :: Parser Properties
    parserDebug "section properties" ps

{-# LINE 923 "ccweb.org" #-}
    when (isJust h) $ do
      stk' <- resize (1 + (headlineLevel $ fromJust h)) . propertyStack <$> P.getState
      P.updateState (\s -> s{ propertyStack = stk' })
      ps' <- Map.unionWith Map.union ps . top . propertyStack <$> P.getState
      P.updateState (\s -> s{ propertyStack = push ps' stk' })
    P.getState >>= parserDebug "updated parser properties" . propertyStack

{-# LINE 930 "ccweb.org" #-}
    skipEmptyLines
    ts  <- P.optionMaybe $ P.many1 $ parse :: Parser (Maybe [Text])

{-# LINE 933 "ccweb.org" #-}
    skipEmptyLines
    c <- case (ts, h) of
          (Nothing, Nothing) -> Just <$> (parse :: Parser SourceBlock)
          (_, _) -> P.optionMaybe $ parse :: Parser (Maybe SourceBlock)

{-# LINE 938 "ccweb.org" #-}
    skipEmptyLines

{-# LINE 940 "ccweb.org" #-}
    parserTrace "end of section parsing"

{-# LINE 942 "ccweb.org" #-}
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
{-# LINE 960 "ccweb.org" #-}
instance Parse Keyword where
  parse = propertyKeyword <|> titleKeyword <|> authorKeyword <|> otherKeyword
    where
      authorKeyword = do
        keywordStart "#+AUTHOR:"
        AuthorKeyword <$> parse
      propertyKeyword = do
        PropertyKeyword <$> 
{-# LINE 776 "ccweb.org" #-}
                            do
                              _ <- P.try (P.string "#+PROPERTY:") *> spaces
                              p <- symbol <* spaces1
                              kvs <- 
{-# LINE 785 "ccweb.org" #-}
                                     line (Map.fromList
                                           <$> P.sepEndBy
                                            (
{-# LINE 793 "ccweb.org" #-}
                                             do
                                               k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                                               v <- parse :: Parser SExpr
                                               return (k, v)
{-# LINE 787 "ccweb.org" #-}
                                                            )
                                            spaces1)
{-# LINE 780 "ccweb.org" #-}
                              return $ Map.singleton p kvs
{-# LINE 968 "ccweb.org" #-}
      titleKeyword = do
        keywordStart "#+TITLE:"
        TitleKeyword <$> parse
      otherKeyword = do
        _ <- P.try $ P.string "#+"
        t <- P.many anyChar
        _ <- endOfLine
        return $ OtherKeyword t
      keywordStart h = void $ P.try (P.string h *> spaces)
{-# LINE 981 "ccweb.org" #-}
headerProperties :: [Keyword] -> Properties
headerProperties = foldl acc mempty where
    acc a (PropertyKeyword p) = Map.union p a
    acc a _ = a
{-# LINE 991 "ccweb.org" #-}
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
{-# LINE 1008 "ccweb.org" #-}
readOrgFile :: LogLevel -> FilePath -> IO (OrgFile, Document)
readOrgFile lvl fp = do
  ingested <- fromList <$> ingest fp :: IO OrgFile
  h <- getHostName
  let doc = fromEither $ P.runParser
        (parse :: Parser Document)
        initialParserState{ hostName = h, parserLogLevel = lvl }
        fp
        ingested
  return (ingested, doc)
{-# LINE 237 "ccweb.org" #-}
data Options = Options
  { optionQuiet :: Bool
  , optionVerbosity :: Int
  , dryRun :: Bool
  , listInputFiles :: Bool
  , listOutputFiles :: Bool
  , inputFile :: String
  }

{-# LINE 246 "ccweb.org" #-}
instance Pretty Options where
  pretty o = pretty $ PrettyStruct "Options"
    [ ("quiet", pretty $ optionQuiet o)
    , ("verbosity", pretty $ optionVerbosity o)
    , ("dry-run", pretty $ dryRun o)
    , ("list-input-files", pretty $ listInputFiles o)
    , ("list-output-files", pretty $ listOutputFiles o)
    , ("input file", pretty $ inputFile o)
    ]

{-# LINE 256 "ccweb.org" #-}
userOptionParser :: O.Parser Options
userOptionParser = Options
  <$> (
{-# LINE 281 "ccweb.org" #-}
       O.switch
         ( O.short 'q'
           <> O.long "quiet"
           <> O.help "Only print things that were asked for."
         )
{-# LINE 258 "ccweb.org" #-}
          )
  <*> (
{-# LINE 290 "ccweb.org" #-}
       length
       <$>
       O.many
         ( O.flag' ()
           ( O.short 'v'
             <> O.long "verbose"
             <> O.help "Be verbose. Can be given multiple times for more verbosity."
           ))
{-# LINE 259 "ccweb.org" #-}
             )
  <*> dryRunParser
  <*> 
{-# LINE 319 "ccweb.org" #-}
      O.switch
        ( O.short 'I'
        <> O.long "list-input-files"
        <> O.help "If given, list the files that have to be read in, one per line."
        )
{-# LINE 262 "ccweb.org" #-}
  <*> 
{-# LINE 310 "ccweb.org" #-}
      O.switch
        ( O.short 'O'
        <> O.long "list-output-files"
        <> O.help "If given, list the files that would be written out, one per line."
        )
{-# LINE 263 "ccweb.org" #-}
  <*> (
{-# LINE 302 "ccweb.org" #-}
       O.argument O.str
         ( O.metavar "FILE"
         <> O.help "The name of the input file."
         )
{-# LINE 263 "ccweb.org" #-}
          )
{-# LINE 268 "ccweb.org" #-}
logLevel :: Options -> LogLevel
logLevel Options{ optionQuiet = q, optionVerbosity = v } =
  case (if q then 0 else 3) + v of
    0 -> Quiet
    1 -> Error
    2 -> Warning
    3 -> Info
    4 -> Debug
    _ -> Trace
{-# LINE 328 "ccweb.org" #-}
dryRunParser :: O.Parser Bool
dryRunParser = O.switch
  ( O.short 'n'
  <> O.long "dry-run"
  <> O.help "If given, do not write any files. Useful when debugging."
  )
{-# LINE 4 "org/weave.org" #-}

{-# LINE 6 "org/weave.org" #-}
main :: IO ()
main = do
  opts <- O.execParser optParser
  logM (logLevel opts) Info $ "This is CCWEAVE"

{-# LINE 11 "org/weave.org" #-}
  (ingested, doc) <- readOrgFile (logLevel opts) (inputFile opts)
  logM (logLevel opts) Debug $ PP.render (pretty ingested)
  logM (logLevel opts) Debug $ PP.render (pretty doc)

{-# LINE 15 "org/weave.org" #-}
  let part = 
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
{-# LINE 15 "org/weave.org" #-}
                          $ doc
           :: DocumentPartition
      texFile = F.replaceExtension (inputFile opts) "tex"
      idxFile = F.replaceExtension (inputFile opts) "idx"
      scnFile = F.replaceExtension (inputFile opts) "scn"
{-# LINE 52 "org/weave.org" #-}
  when (listOutputFiles opts) $ mapM_ putStrLn [texFile, idxFile, scnFile]
  when (listInputFiles opts)  $ mapM_ putStrLn (
    nub . sort .
    map (\(OrgLine p _) -> P.sourceName p) $ (toList ingested)
    )
{-# LINE 20 "org/weave.org" #-}

{-# LINE 22 "org/weave.org" #-}
  let tex = fst $ runState (weave doc)
        WeaveState { backReferences = Map.empty
                   , docPartition = part
                   }
  if dryRun opts
    then logM (logLevel opts) Info tex
    else do
      logM (logLevel opts) Info $ "Writing the output file..."
      F.fileExist texFile >>= (`when` F.removeLink texFile)
      writeFile texFile tex

{-# LINE 33 "org/weave.org" #-}
      logM (logLevel opts) Info $ "Writing the index..."
      F.fileExist idxFile >>= (`when` F.removeLink idxFile)
      writeFile idxFile []

{-# LINE 37 "org/weave.org" #-}
      F.fileExist scnFile >>= (`when` F.removeLink scnFile)
{-# LINE 248 "org/weave.org" #-}
      do
        writeFile scnFile []
{-# LINE 38 "org/weave.org" #-}

{-# LINE 40 "org/weave.org" #-}
      logM (logLevel opts) Info $ "Done."
  where
    optParser = O.info (userOptionParser <**> O.helper)
      ( O.fullDesc
      <> O.progDesc "Weave the input FILE"
      <> O.header "ccweave - A literate programming weaver" )

{-# LINE 87 "org/weave.org" #-}
data WeaveState = WeaveState
  { backReferences :: Map.Map SourceBlockId [Section]
  , docPartition :: DocumentPartition
  }
{-# LINE 95 "org/weave.org" #-}
type Weaver a = State WeaveState a

{-# LINE 97 "org/weave.org" #-}
class Weave a where
  weave :: a -> Weaver String
{-# LINE 103 "org/weave.org" #-}
instance Weave Char where
  weave '&' = return "{\\AM}"
  weave '\\' = return "{\\BS}"
  weave '{' = return "{\\LB}"
  weave '}' = return "{\\RB}"
  weave '~' = return "{\\TL}"
  weave '_' = return "{\\UL}"
  weave '^' = return "{\\CF}"
  weave '#' = return "{\\#}"
  weave '$' = return "{\\$}"
  weave '%' = return "{\\%}"
  weave c = return [c]
{-# LINE 119 "org/weave.org" #-}
instance Weave TextElement where
  weave = \case
    (Plain s) -> concatMapM weave s
    (Bold s) -> between "{\\bf " "}" <$> concatMapM weave s
    (Italics s) -> between "{\\it " "\\/}" <$> concatMapM weave s
    (InlineCode s) -> between "\\hbox{\\tentex " "}" <$> concatMapM weave s
    (Verbatim s) -> between "\\hbox{\\tentex " "}" <$> concatMapM weave s
    (StrikeThrough _) -> error $ "not implemented: StrikeThrough"
    (TeXMath s) -> return $ between "$" "$" s
    where between before after s = before <> s <> after

{-# LINE 130 "org/weave.org" #-}
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f as = concat <$> mapM f as
{-# LINE 136 "org/weave.org" #-}
instance Weave Text where
  weave (Text ts) = concat <$> mapM weave ts
{-# LINE 145 "org/weave.org" #-}
instance Weave Section where
  weave s = do
    header <- (
{-# LINE 155 "org/weave.org" #-}
               do
                 concat <$>
                   case sectionHeadline s of
                     Nothing ->
                       return [ "\\M{", show (sectionNumber s), "}"]
                     Just (Headline d t) -> do
                       title <- weave t :: Weaver String
                       return [ "\\N{", show d, "}"
                              , "{", show (sectionNumber s), "}"
                              , title, "."
                              ]
{-# LINE 147 "org/weave.org" #-}
                               ) :: Weaver String
    text <- (
{-# LINE 170 "org/weave.org" #-}
             mapM weave (documentation s)
{-# LINE 148 "org/weave.org" #-}
                                         ) :: Weaver [String]
    code <- (
{-# LINE 175 "org/weave.org" #-}
             case sectionSourceBlock s of
               Nothing -> return []
               Just b -> do
                 part <- docPartition <$> get
                 let id = sourceBlockId b
                     sections = (Map.!) part <$> id
                     firstNo = sectionNumber . head <$> sections
                     secno = sectionNumber s
                 n <- (
{-# LINE 215 "org/weave.org" #-}
                       \ref -> case sourceBlockId (fromJust $ sectionSourceBlock ref) of
                                Nothing -> return "\\X:$\\bullet$\\X"
                                Just id -> weave id
{-# LINE 183 "org/weave.org" #-}
                                                   ) s :: Weaver String
                 ls <- mapM weave (blockLines b) :: Weaver [String]
                 let head = concat
                       [ if null text then [] else "\\Y"
                       , "\\B\\4"
                       , n
                       , "${}"
                       , if secno == fromMaybe 0 firstNo then [] else "\\mathrel+"
                       , "\\equiv{}$"
                       ]
                 return $ intercalate "\\6" (head : ls) ++ "\\par"
{-# LINE 149 "org/weave.org" #-}
                                                                  ) :: Weaver String
    return $ unlines (header : text) ++ code ++ "\\fi\n"
{-# LINE 198 "org/weave.org" #-}
instance Weave CodeElement where
  weave (Literal _ s) = do
    s' <- concatMapM weave s
    return $ "\\hbox{\\tentex "
      ++ map (\c -> if c == ' ' then '~' else c) s'
      ++ "}"
  weave (SectionReference _ r) = weave (NamedBlock r)
{-# LINE 209 "org/weave.org" #-}
instance Weave CodeLine where
  weave (CodeLine _ es) = concatMapM weave es
{-# LINE 222 "org/weave.org" #-}
instance Weave SourceBlockId where
  weave id = do
    firstNo <- sectionNumber . head . (flip (Map.!) id) . docPartition <$> get
    tangledName <- case id of
      FileBlock file -> ("$\\Rightarrow\\,$" ++) <$> weave (Verbatim file)
      NamedBlock name -> weave name
    return . concat $ ["\\X", show firstNo, ":", tangledName, "\\X"]
{-# LINE 233 "org/weave.org" #-}
instance Weave Document where
  weave doc = do
    secs <- concat <$> mapM weave (sections doc)
    return $ unlines
      [ "\\input cwebmac"
      , secs
      , "\\inx"
      , "\\fin"
      , "\\con"
      ]
{-# LINE 47 "org/weave.org" #-}
