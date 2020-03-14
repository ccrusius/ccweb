{-# LINE 63 "ccweb.org" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 48 "ccweb.org" #-}
import Control.Applicative ((<|>))
import Control.Monad (liftM2, void, when)
import Control.Monad.State (State, get, modify, runState)
import Data.Foldable (foldlM, foldrM)
import Data.Char (isPrint, isSpace, ord)
import Data.List (intercalate, isPrefixOf, isSuffixOf, nub, sort)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Debug.Trace (trace)
import qualified Data.Map as Map
import GHC.Exts (IsList(..), groupWith, sortWith)
import Network.HostName (getHostName)
{-# LINE 74 "org/parser.org" #-}
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
{-# LINE 377 "org/parser.org" #-}
import qualified System.Info as Sys
import qualified Control.Exception as E
{-# LINE 426 "org/tangle.org" #-}
import qualified System.Directory as D
import qualified System.Environment as Env
import qualified System.FilePath as F
import qualified System.Posix.Files as F
{-# LINE 33 "org/scaffold.org" #-}
import qualified Options.Applicative as O
{-# LINE 163 "org/scaffold.org" #-}
import Paths_ccweb (version)
import Data.Version (showVersion)
{-# LINE 205 "org/scaffold.org" #-}
import qualified Text.PrettyPrint as PP
{-# LINE 125 "ccweb.org" #-}
import qualified System.Console.ANSI as ANSI
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
{-# LINE 62 "org/doc.org" #-}
headerProperties :: [Keyword] -> Properties
headerProperties = foldl acc mempty where
    acc a (PropertyKeyword p) = Map.union p a
    acc a _ = a
{-# LINE 79 "org/doc.org" #-}
data Section = Section
  { sectionNumber :: Int
  , sectionHeadline :: Headline
  , documentation :: [Text]
  , sectionSourceBlock :: Maybe SourceBlock
  , sectionProps :: Properties
  , sectionDerivedProperties :: Properties
  }
{-# LINE 111 "org/doc.org" #-}
data Headline = Headline Int Text | EmptyHeadline deriving Show
{-# LINE 133 "org/doc.org" #-}
type Property   = Map.Map String SExpr
type Properties = Map.Map String Property
{-# LINE 142 "org/doc.org" #-}
data SExpr =
  Atom String
  | IntAtom Int
  | SExpr [SExpr]
  deriving (Eq, Ord)
{-# LINE 154 "org/doc.org" #-}
fromBool :: Bool -> SExpr
fromBool False = SExpr []
fromBool True = Atom "t"
{-# LINE 165 "org/doc.org" #-}
toBool :: SExpr -> Bool
toBool (Atom _) = True
toBool (IntAtom _) = True
toBool _ = False
{-# LINE 200 "org/doc.org" #-}
class Eval a where
  eval :: ParserState -> a -> a
{-# LINE 208 "org/doc.org" #-}
instance Eval Property where; eval s = Map.map (eval s)
instance Eval Properties where; eval s = Map.map (eval s)
{-# LINE 217 "org/doc.org" #-}
instance Eval SExpr where
{-# LINE 225 "org/doc.org" #-}
  eval s (SExpr [Atom "identity", expr]) = eval s expr
{-# LINE 233 "org/doc.org" #-}
  eval _ (Atom ('#':'o':x:y:z:[])) =
    IntAtom $ 8 * ((8 * oct x) + oct y) + oct z
    where oct c = ord c - ord '0'
{-# LINE 242 "org/doc.org" #-}
  eval s (SExpr [Atom "eq", e1, e2]) = fromBool $ (eval s e1) == (eval s e2)
  
{-# LINE 244 "org/doc.org" #-}
  eval s (SExpr [Atom "string-prefix-p", prefix, expr]) =
    case (eval s prefix, eval s expr) of
      (Atom s', Atom str) -> fromBool $ isPrefixOf s' str
      (e1, e2) -> SExpr [Atom "string-prefix-p", e1, e2]
  
{-# LINE 249 "org/doc.org" #-}
  eval s (SExpr [Atom "string-suffix-p", suffix, expr]) =
    case (eval s suffix, eval s expr) of
      (Atom s', Atom str) -> fromBool $ isSuffixOf s' str
      (e1, e2) -> SExpr [Atom "string-suffix-p", e1, e2]
{-# LINE 259 "org/doc.org" #-}
  eval s (SExpr [Atom "not", expr]) =
    fromBool . not . toBool . eval s $ expr
  
{-# LINE 262 "org/doc.org" #-}
  eval s (SExpr (Atom "and" : exprs)) =
    fromBool . and . map (toBool . eval s) $ exprs
  
{-# LINE 265 "org/doc.org" #-}
  eval s (SExpr (Atom "or" : exprs)) =
    fromBool . or . map (toBool . eval s) $ exprs
{-# LINE 273 "org/doc.org" #-}
  eval s (SExpr [Atom "when", expr, result]) =
    if toBool (eval s expr)
    then eval s result
    else fromBool False
  
{-# LINE 278 "org/doc.org" #-}
  eval s (SExpr [Atom "unless", expr, result]) =
    if toBool (eval s expr)
    then fromBool False
    else eval s result
{-# LINE 289 "org/doc.org" #-}
  eval s expr = Map.findWithDefault expr expr (evalContext s)
{-# LINE 302 "org/doc.org" #-}
class HeaderArgs a where
  headerArgs :: a -> Property
  headerArg :: String -> a -> Maybe SExpr
  headerArg k a = Map.lookup k $ headerArgs a

{-# LINE 307 "org/doc.org" #-}
instance HeaderArgs Properties where
  headerArgs = Map.findWithDefault Map.empty "header-args"

{-# LINE 310 "org/doc.org" #-}
instance HeaderArgs Section where
  headerArgs = headerArgs . sectionDerivedProperties
{-# LINE 326 "org/doc.org" #-}
data SourceBlock = SourceBlock
  { blockName :: Maybe Text
  , blockLanguage :: String
  , blockLines :: [CodeLine]
  , blockLocation :: P.SourcePos
  , blockProperties :: Property
  , blockDerivedProperties :: Property
  }
{-# LINE 355 "org/doc.org" #-}
instance HeaderArgs SourceBlock where
  headerArgs = blockDerivedProperties
{-# LINE 366 "org/doc.org" #-}
data SourceBlockId = FileBlock FilePath | NamedBlock Text deriving Eq

{-# LINE 368 "org/doc.org" #-}
instance Ord SourceBlockId where
  (<=) (FileBlock p1) (FileBlock p2) = p1 <= p2
  (<=) (NamedBlock p1) (NamedBlock p2) = p1 <= p2
  (<=) (FileBlock _) (NamedBlock _) = False
  (<=) _ _ = True
{-# LINE 381 "org/doc.org" #-}
sourceBlockId :: SourceBlock -> Maybe SourceBlockId
sourceBlockId SourceBlock{ blockName = (Just name) } = Just $ NamedBlock name
sourceBlockId block = case headerArg ":tangle" block of
    Nothing -> Nothing
    Just (Atom "t") -> Nothing
    Just (SExpr []) -> Nothing
    Just (Atom f) -> Just $ FileBlock f
    Just e -> error $ "unsupported tangle destination: " ++ show e
{-# LINE 396 "org/doc.org" #-}
newtype CodeLine = CodeLine [CodeElement]
data CodeElement = Literal P.SourcePos String
                 | SectionReference P.SourcePos Text
{-# LINE 425 "org/doc.org" #-}
data Text = Text [TextElement] deriving (Eq, Ord, Show)

{-# LINE 427 "org/doc.org" #-}
data TextElement =
  Bold Text
  | HyperLink String Text
  | InlineCode String
  | Italics Text
  | Plain String
  | StrikeThrough String
  | InlineMath String
  | DisplayMath String
  | Verbatim String
  deriving (Eq, Ord, Show)
{-# LINE 470 "org/doc.org" #-}
trim :: Text -> Text
trim (Text (Plain []:ys))       = trim $ Text ys
trim (Text (Plain (' ':xs):ys)) = trim $ Text (Plain xs:ys)
trim  t                         = t
{-# LINE 480 "org/doc.org" #-}
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
{-# LINE 93 "org/doc.org" #-}
instance Pretty Section where
  pretty = prettyStruct "Section"
             [ ("number", pretty . sectionNumber)
             , ("headline", pretty . sectionHeadline)
             , ("text", pretty . documentation)
             , ("code", pretty . sectionSourceBlock)
             , ("properties", pretty . sectionProps)
             , ("derived properties", pretty . sectionDerivedProperties)
             ]
{-# LINE 118 "org/doc.org" #-}
instance Pretty Headline where
  pretty (Headline l t) = PP.hcat [PP.char '*', PP.braces (pretty l), pretty t]
  pretty EmptyHeadline = PP.char '§'
{-# LINE 176 "org/doc.org" #-}
instance Show SExpr where
  show (Atom x) = x
  show (IntAtom x) = show x
  show (SExpr []) = "nil"
  show (SExpr xs) = "(" ++ intercalate " " (map show xs) ++ ")"

{-# LINE 182 "org/doc.org" #-}
instance Pretty SExpr where
  pretty = PP.text . show
{-# LINE 340 "org/doc.org" #-}
instance Pretty SourceBlock where
  pretty = prettyStruct "SourceBlock"
             [ ("name", pretty . blockName)
             , ("language", pretty . blockLanguage)
             , ("properties", pretty . blockProperties)
             , ("derived properties", pretty . blockDerivedProperties)
             , ("location", pretty . blockLocation)
             , ("lines", pretty . blockLines)
             ]
{-# LINE 407 "org/doc.org" #-}
instance Pretty CodeLine where
  pretty (CodeLine xs) = pretty xs

{-# LINE 410 "org/doc.org" #-}
instance Pretty CodeElement where
  pretty (Literal p s) =
    PP.parens $ PP.hcat [pretty p, PP.colon, pretty s]
  pretty (SectionReference p t) =
    PP.hcat [pretty p, PP.colon, PP.char '〈', pretty t, PP.char '〉']
{-# LINE 446 "org/doc.org" #-}
instance Pretty TextElement where
  pretty = \case
    (Bold a)          -> pretty' "bold" a
    (InlineCode a)    -> pretty' "code" a
    (Italics a)       -> pretty' "italic" a
    (Plain a)         -> pretty a
    (StrikeThrough a) -> pretty' "strike" a
    (InlineMath a)    -> pretty' "$TeX$" a
    (DisplayMath a)   -> pretty' "$$TeX$$" a
    (Verbatim a)      -> pretty' "verbatim" a
    (HyperLink a b)   -> PP.hcat [ pretty "link"
                                , PP.braces (pretty a)
                                , PP.braces (pretty b)
                                ]
    where
      pretty' lbl str = PP.hcat [pretty lbl, PP.braces (pretty str)]
{-# LINE 23 "org/parser.org" #-}
instance Pretty ParserState where
  pretty = prettyStruct "ParserState"
    [ ("counter", pretty . sectionCounter)
    , ("eval context", pretty . evalContext)
    , ("log level", pretty . parserLogLevel)
    , ("property stack", pretty . propertyStack)
    ]
{-# LINE 64 "org/parser.org" #-}
instance Pretty OrgLine where
  pretty (OrgLine p l) =
    PP.hcat [pretty p, PP.colon, PP.text (takeWhile (/= '\n') l), PP.char '␤']
{-# LINE 158 "org/parser.org" #-}
instance Pretty OrgLines where
  pretty (OrgLines ls) = pretty ls
{-# LINE 210 "org/scaffold.org" #-}
class Pretty a where
  pretty :: a -> PP.Doc
  prettyList :: [a] -> PP.Doc
  prettyList = PP.brackets . PP.fcat . PP.punctuate PP.comma . map pretty

{-# LINE 215 "org/scaffold.org" #-}
instance Pretty PP.Doc where
  pretty = id

{-# LINE 218 "org/scaffold.org" #-}
instance Pretty Bool where
  pretty True = PP.text "True"
  pretty False = PP.text "False"

{-# LINE 222 "org/scaffold.org" #-}
instance Pretty Char where
  pretty = PP.char
  prettyList = PP.text

{-# LINE 226 "org/scaffold.org" #-}
instance Pretty Int where
  pretty = PP.int

{-# LINE 229 "org/scaffold.org" #-}
instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = PP.text "Nothing"
  pretty (Just a) = pretty a

{-# LINE 233 "org/scaffold.org" #-}
instance Pretty () where
  pretty _ = PP.text ""

{-# LINE 236 "org/scaffold.org" #-}
instance (Pretty a, Pretty b) => Pretty (a,b) where
  pretty (a,b) = PP.parens . PP.fcat . PP.punctuate PP.comma $ [pretty a, pretty b]

{-# LINE 239 "org/scaffold.org" #-}
instance Pretty a => Pretty [a] where
  pretty = prettyList

{-# LINE 242 "org/scaffold.org" #-}
instance Pretty P.ParseError where
  pretty = PP.text . show
{-# LINE 248 "org/scaffold.org" #-}
instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
  pretty = PP.brackets
           . PP.fcat
           . PP.punctuate PP.comma
           . map (\(k,v) -> pretty k PP.<> PP.equals PP.<> pretty v)
           . Map.toList
{-# LINE 258 "org/scaffold.org" #-}
data PrettyStruct = PrettyStruct String [(String, PP.Doc)]

{-# LINE 260 "org/scaffold.org" #-}
instance Pretty PrettyStruct where
  pretty (PrettyStruct t ps) =
    let fields = map (\(n,p) -> PP.text n PP.<> PP.colon PP.<+> p) ps
    in PP.text t PP.<> PP.braces (PP.fcat $ PP.punctuate PP.comma fields)

{-# LINE 265 "org/scaffold.org" #-}
prettyStruct :: String -> [(String, (a -> PP.Doc))] -> a -> PP.Doc
prettyStruct name kfs a = pretty (PrettyStruct name (map (\(k,f) -> (k,f a)) kfs))
{-# LINE 271 "org/scaffold.org" #-}
instance Pretty P.SourcePos where
  pretty p = pretty (P.sourceName p)
             PP.<> PP.colon PP.<> pretty (P.sourceLine p)
             PP.<> PP.colon PP.<> pretty (P.sourceColumn p)
{-# LINE 150 "ccweb.org" #-}
instance Pretty Text where
  pretty (Text xs) = PP.text "Text:" PP.<+> PP.hcat (map pretty xs)
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
  , evalContext    :: Map.Map SExpr SExpr
  , parserLogLevel :: LogLevel
  }
{-# LINE 39 "org/parser.org" #-}
type Parser = P.Parsec OrgLines ParserState

{-# LINE 41 "org/parser.org" #-}
class Parse a where
  parse :: Parser a
{-# LINE 56 "org/parser.org" #-}
data OrgLine = OrgLine P.SourcePos String
{-# LINE 86 "org/parser.org" #-}
ingest :: FilePath -> IO [OrgLine]
ingest file = do
    zipWith OrgLine [P.newPos file l 1 | l <- [1..]] . map (++"\n") . lines
      <$> readFile file
      >>= foldrM scan []
  where
    scan l@(OrgLine pos s) ls =
      maybe (return (l : ls)) (fmap (++ ls) . ingest)
      $ (
{-# LINE 104 "org/parser.org" #-}
         fromEither . P.parse
         (P.setPosition pos *>
             P.optionMaybe (
               P.try (P.spaces *> P.string "#+INCLUDE:" *> P.spaces)
               *> P.char '"'
               *> P.manyTill anyChar (P.char '"')
             ))
         (P.sourceName pos)
{-# LINE 94 "org/parser.org" #-}
                            :: String -> Maybe FilePath) s
{-# LINE 128 "org/parser.org" #-}
anyChar :: (P.Stream s m Char) => P.ParsecT s u m Char
anyChar = P.noneOf "\n\r"
{-# LINE 137 "org/parser.org" #-}
instance Monad m => P.Stream OrgLine m Char where
  uncons (OrgLine _ []) =
    return Nothing
  uncons (OrgLine p (x:xs)) =
    return $ Just (x, OrgLine (P.updatePosChar p x) xs)
{-# LINE 151 "org/parser.org" #-}
newtype OrgLines = OrgLines [OrgLine]
{-# LINE 168 "org/parser.org" #-}
instance Monad m => P.Stream OrgLines m Char where
  uncons (OrgLines (x:[])) = P.uncons x >>= \case
    Nothing -> return Nothing
    Just (x',xs') -> return $ Just (x', OrgLines (xs':[]))
  uncons (OrgLines (x:xs)) = P.uncons x >>= \case
    Nothing -> P.uncons (fromList xs :: OrgLines)
    Just (x',OrgLine _ []) -> return $ Just (x', OrgLines xs)
    Just (x',xs') -> return $ Just (x', OrgLines (xs':xs))
  uncons (OrgLines []) =
    error $ "(internal): uncons of empty OrgLines instance"
{-# LINE 188 "org/parser.org" #-}
space :: Parser Char
space = P.satisfy (\c -> isSpace c && notElem c "\n\r")

{-# LINE 191 "org/parser.org" #-}
spaces :: Parser String
spaces = P.many space

{-# LINE 194 "org/parser.org" #-}
spaces1 :: Parser String
spaces1 = P.many1 space
{-# LINE 205 "org/parser.org" #-}
symbolChar :: Parser Char
symbolChar = P.satisfy (\c -> isPrint c && not (isSpace c) && notElem c "()=")

{-# LINE 208 "org/parser.org" #-}
symbol :: Parser String
symbol = P.many1 symbolChar
{-# LINE 222 "org/parser.org" #-}
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P.tokenPrim
            (\c -> show [c])
            (\pos _ (OrgLines rest) ->
               case rest of
                 (OrgLine pos' _):_ -> pos'
                 [] -> error $ show pos ++ ": (internal) empty OrgLines")
            (\c -> if f c then Just c else Nothing)
{-# LINE 240 "org/parser.org" #-}
char :: Char -> Parser Char
char c = satisfy (==c) P.<?> show [c]

{-# LINE 243 "org/parser.org" #-}
newline :: Parser Char
newline = char '\n' P.<?> "lf new-line"

{-# LINE 246 "org/parser.org" #-}
crlf :: Parser Char
crlf = char '\r' *> char '\n' P.<?> "crlf new-line"

{-# LINE 249 "org/parser.org" #-}
endOfLine :: Parser Char
endOfLine = newline <|> crlf P.<?> "new-line"
{-# LINE 260 "org/parser.org" #-}
beginningOfLine :: Parser ()
beginningOfLine = do
  c <- P.sourceColumn <$> P.getPosition
  succeedWhen (c == 1)
{-# LINE 271 "org/parser.org" #-}
succeedWhen :: Bool -> Parser ()
succeedWhen True = return ()
succeedWhen False = void $ satisfy (\_ -> False)

{-# LINE 275 "org/parser.org" #-}
failWhen :: Bool -> Parser ()
failWhen = succeedWhen . not
{-# LINE 288 "org/parser.org" #-}
manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p = P.manyTill p . P.lookAhead

{-# LINE 291 "org/parser.org" #-}
many1Till :: Parser a -> Parser end -> Parser [a]
many1Till p = liftM2 (:) p . manyTill p
{-# LINE 297 "org/parser.org" #-}
eitherMany1 :: Parser end -> Parser a -> Parser (Either end [a])
eitherMany1 end p =
  (Left <$> P.try end) <|>
  (Right <$> many1Till p (void endOfLine <|> void end))
{-# LINE 311 "org/parser.org" #-}
instance Parse Document where
  parse = do
    _ <- parserTrace "Document" ()
    hs <- P.many parse :: Parser [Keyword]
    P.updateState $ \s -> s{ propertyStack = Stack [headerProperties hs] }
    skipEmptyLines
    _ <- parserTrace "Document:Sections" ()
    ss <- P.many . P.try $ parse :: Parser [Section]
    _ <- parserTrace "Document:/Sections" ()
    skipEmptyLines *> P.eof
    parserTrace "/Document" $ Document { keywords = hs , sections = ss }
{-# LINE 330 "org/parser.org" #-}
readOrgFile :: LogLevel -> FilePath -> IO (OrgLines, Document)
readOrgFile lvl fp = do
  ingested <- fromList <$> ingest fp :: IO OrgLines
{-# LINE 348 "org/parser.org" #-}
  let styp = ( Atom "system-type"
             , Atom $ case Sys.os of
                        "linux" -> "gnu/linux"
                        "mingw32" -> "windows-nt"
                        o -> o
             )
      atomize k = fmap ((Atom k,) . Atom)
  hnam <- (SExpr [Atom "system-name"],) . Atom <$> getHostName
  pfam <- atomize "dmi/product-family"  <$> (
{-# LINE 367 "org/parser.org" #-}
                                             \p -> either (\_ -> Nothing) (Just . reverse . dropWhile isSpace . reverse)
                                                  <$> (E.try (readFile $ "/sys/devices/virtual/dmi/id" F.</> p)
                                                       :: IO (Either E.IOException String))
{-# LINE 356 "org/parser.org" #-}
                                                                                           ) "product_family"
  pnam <- atomize "dmi/product-name"    <$> (
{-# LINE 367 "org/parser.org" #-}
                                             \p -> either (\_ -> Nothing) (Just . reverse . dropWhile isSpace . reverse)
                                                  <$> (E.try (readFile $ "/sys/devices/virtual/dmi/id" F.</> p)
                                                       :: IO (Either E.IOException String))
{-# LINE 357 "org/parser.org" #-}
                                                                                           ) "product_name"
  pver <- atomize "dmi/product-version" <$> (
{-# LINE 367 "org/parser.org" #-}
                                             \p -> either (\_ -> Nothing) (Just . reverse . dropWhile isSpace . reverse)
                                                  <$> (E.try (readFile $ "/sys/devices/virtual/dmi/id" F.</> p)
                                                       :: IO (Either E.IOException String))
{-# LINE 358 "org/parser.org" #-}
                                                                                           ) "product_version"
{-# LINE 358 "org/parser.org" #-}
  
{-# LINE 360 "org/parser.org" #-}
  let ctx = Map.fromList $ styp : hnam : catMaybes [pfam, pnam, pver]
{-# LINE 334 "org/parser.org" #-}
  let doc = fromEither $ P.runParser
        (parse :: Parser Document)
        initialParserState{ evalContext = ctx, parserLogLevel = lvl }
        fp
        ingested
  return (ingested, doc)
{-# LINE 390 "org/parser.org" #-}
instance Parse Keyword where
  parse = propertyKeyword <|> titleKeyword <|> authorKeyword <|> otherKeyword
    where
      propertyKeyword = do
        PropertyKeyword <$> 
{-# LINE 429 "org/parser.org" #-}
                            do
                              _ <- P.try (P.string "#+PROPERTY:") *> spaces
                              liftM2 Map.singleton
                                (symbol <* spaces1)
                                (
{-# LINE 456 "org/parser.org" #-}
                                 line (Map.fromList
                                       <$> P.sepEndBy
                                        (liftM2 (,)
                                          (symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces)))
                                          (parse :: Parser SExpr))
                                        spaces1)
{-# LINE 433 "org/parser.org" #-}
                                                )
{-# LINE 395 "org/parser.org" #-}
      authorKeyword = do
        keywordStart "#+AUTHOR:"
        AuthorKeyword <$> parse
      titleKeyword = do
        keywordStart "#+TITLE:"
        TitleKeyword <$> parse
      otherKeyword = do
        _ <- P.try $ P.string "#+"
        t <- P.many anyChar
        _ <- endOfLine
        return $ OtherKeyword t
      keywordStart h = void $ P.try (P.string h *> spaces)
{-# LINE 416 "org/parser.org" #-}
instance Parse Properties where
  parse = do
    _ <- parserTrace "Properties" ()
    ps <- (
{-# LINE 429 "org/parser.org" #-}
           do
             _ <- P.try (P.string "#+PROPERTY:") *> spaces
             liftM2 Map.singleton
               (symbol <* spaces1)
               (
{-# LINE 456 "org/parser.org" #-}
                line (Map.fromList
                      <$> P.sepEndBy
                       (liftM2 (,)
                         (symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces)))
                         (parse :: Parser SExpr))
                       spaces1)
{-# LINE 433 "org/parser.org" #-}
                               )
{-# LINE 419 "org/parser.org" #-}
                                ) <|> (
{-# LINE 441 "org/parser.org" #-}
                                       do
                                         _ <- P.try (spaces *> P.string ":PROPERTIES:" *> spaces *> endOfLine)
                                         Map.fromList <$> P.manyTill
                                           (liftM2 (,)
                                              (spaces *> (enclosed ':' <* spaces))
                                              (
{-# LINE 456 "org/parser.org" #-}
                                               line (Map.fromList
                                                     <$> P.sepEndBy
                                                      (liftM2 (,)
                                                        (symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces)))
                                                        (parse :: Parser SExpr))
                                                      spaces1)
{-# LINE 446 "org/parser.org" #-}
                                                              ))
                                           (P.try (spaces *> P.string ":END:" *> spaces *> endOfLine))
{-# LINE 419 "org/parser.org" #-}
                                                                                                      )
    _ <- parserTrace "/Properties" ()
    return ps
{-# LINE 472 "org/parser.org" #-}
instance Parse SExpr where
  parse = (
{-# LINE 482 "org/parser.org" #-}
           SExpr <$> ( P.try (P.char '(')
                        *> spaces
                        *> (P.sepEndBy (parse :: Parser SExpr) spaces1 <* P.char ')')
                      )
{-# LINE 473 "org/parser.org" #-}
                       )
          <|> (
{-# LINE 492 "org/parser.org" #-}
               P.try (P.string "nil" *> P.lookAhead P.space *> return (SExpr []))
               <|>
               P.try (P.string "no" *> P.lookAhead P.space *> return (SExpr []))
               <|>
               P.try (P.string "yes" *> P.lookAhead P.space *> return (Atom "t"))
{-# LINE 474 "org/parser.org" #-}
                                                                                 )
          <|> (
{-# LINE 504 "org/parser.org" #-}
               Atom <$> (enclosed '"' <|> (char '\'' *> symbol) <|> symbol)
{-# LINE 475 "org/parser.org" #-}
                                                                           )
{-# LINE 601 "org/parser.org" #-}
textFromString :: P.SourcePos -> String -> Text
textFromString p s = fromEither $ P.runParser
    (parse :: Parser Text)
    initialParserState
    (P.sourceName p)
    (OrgLines [OrgLine p s])
{-# LINE 615 "org/parser.org" #-}
textTill :: Parser end -> Parser Text
textTill end = do
  _ <- parserTrace "TextTill" ()
  p <- P.getPosition
  t <- P.manyTill anyChar end
  parserTrace "/TextTill" $ textFromString p t
{-# LINE 665 "org/parser.org" #-}
instance Parse Text where
  parse = do
    _ <- parserTrace "Text" ()
    ret <- reverse . snd <$> scanText ([], [])
    failWhen (null ret) P.<?> "empty text"
    parserTrace "/Text" $ Text ret
{-# LINE 697 "org/parser.org" #-}
scanText :: (String, [TextElement]) -> Parser (String, [TextElement])
scanText acc@(plain, text) =
{-# LINE 723 "org/parser.org" #-}
  (P.eof *> parserTrace "Text:/EOF"
    ((
{-# LINE 681 "org/parser.org" #-}
      \(newChar, newMarkup) -> case (newChar, fst acc, newMarkup) of
        (Nothing, [], Nothing) -> ([], snd acc)
        (Nothing, cs, Nothing) -> ([], Plain (reverse cs) : snd acc)
        (Just c,  cs, Nothing) -> ([], Plain (reverse $ c : cs) : snd acc)
        (Nothing, [], Just m)  -> ([], m : snd acc)
        (Nothing, cs, Just m)  -> ([], m : Plain (reverse cs) : snd acc)
        (Just c,  cs, Just m)  -> ([], m : Plain (reverse $ c : cs) : snd acc)
{-# LINE 724 "org/parser.org" #-}
                                                                              ) (Nothing, Nothing)))
  <|> (endOfLine *> parserTrace "Text:/EOL"
    ((
{-# LINE 681 "org/parser.org" #-}
      \(newChar, newMarkup) -> case (newChar, fst acc, newMarkup) of
        (Nothing, [], Nothing) -> ([], snd acc)
        (Nothing, cs, Nothing) -> ([], Plain (reverse cs) : snd acc)
        (Just c,  cs, Nothing) -> ([], Plain (reverse $ c : cs) : snd acc)
        (Nothing, [], Just m)  -> ([], m : snd acc)
        (Nothing, cs, Just m)  -> ([], m : Plain (reverse cs) : snd acc)
        (Just c,  cs, Just m)  -> ([], m : Plain (reverse $ c : cs) : snd acc)
{-# LINE 726 "org/parser.org" #-}
                                                                              ) (Nothing, Nothing)))
{-# LINE 700 "org/parser.org" #-}
  <|> (
{-# LINE 734 "org/parser.org" #-}
       do
         beginningOfLine
         P.lookAhead . P.choice $ map P.try
           [ void (spaces *> P.string "#+NAME")
           , void (spaces *> P.string "#+BEGIN_SRC")
           , void (parse :: Parser Headline)
           ]
         parserTrace "Text:/Break" $ (
{-# LINE 681 "org/parser.org" #-}
                                      \(newChar, newMarkup) -> case (newChar, fst acc, newMarkup) of
                                        (Nothing, [], Nothing) -> ([], snd acc)
                                        (Nothing, cs, Nothing) -> ([], Plain (reverse cs) : snd acc)
                                        (Just c,  cs, Nothing) -> ([], Plain (reverse $ c : cs) : snd acc)
                                        (Nothing, [], Just m)  -> ([], m : snd acc)
                                        (Nothing, cs, Just m)  -> ([], m : Plain (reverse cs) : snd acc)
                                        (Just c,  cs, Just m)  -> ([], m : Plain (reverse $ c : cs) : snd acc)
{-# LINE 741 "org/parser.org" #-}
                                                                                                              ) (Nothing, Nothing)
{-# LINE 700 "org/parser.org" #-}
                                                                                                                                  )
  <|>
  (do
      markup <- P.try (
{-# LINE 645 "org/parser.org" #-}
                       do
                         url <- P.string "[[" *> many1Till anyChar (P.string "][" <|> P.string "]]")
                         txt <- P.string "][" <|> P.string "]]" >>= \case
                           "]]" -> return $ Text [Verbatim url]
                           _    -> textTill (P.string "]]")
                         return $ HyperLink url txt
{-# LINE 703 "org/parser.org" #-}
                                                   ) >>= parserTrace "Text/Link"
      scanText $ (
{-# LINE 681 "org/parser.org" #-}
                  \(newChar, newMarkup) -> case (newChar, fst acc, newMarkup) of
                    (Nothing, [], Nothing) -> ([], snd acc)
                    (Nothing, cs, Nothing) -> ([], Plain (reverse cs) : snd acc)
                    (Just c,  cs, Nothing) -> ([], Plain (reverse $ c : cs) : snd acc)
                    (Nothing, [], Just m)  -> ([], m : snd acc)
                    (Nothing, cs, Just m)  -> ([], m : Plain (reverse cs) : snd acc)
                    (Just c,  cs, Just m)  -> ([], m : Plain (reverse $ c : cs) : snd acc)
{-# LINE 704 "org/parser.org" #-}
                                                                                          ) (Nothing, Just markup)
  )
  <|>
  (do
      markup <- P.try (
{-# LINE 630 "org/parser.org" #-}
                       (DisplayMath <$>
                         (P.try (P.string "$$")
                          *> P.manyTill P.anyChar (P.try $ P.string "$$")))
                       <|>
                       (InlineMath <$>
                         (P.char '$'
                          *> P.manyTill P.anyChar (P.char '$')))
                       :: Parser TextElement
{-# LINE 708 "org/parser.org" #-}
                                            ) >>= parserTrace "Text/TeX"
      scanText $ (
{-# LINE 681 "org/parser.org" #-}
                  \(newChar, newMarkup) -> case (newChar, fst acc, newMarkup) of
                    (Nothing, [], Nothing) -> ([], snd acc)
                    (Nothing, cs, Nothing) -> ([], Plain (reverse cs) : snd acc)
                    (Just c,  cs, Nothing) -> ([], Plain (reverse $ c : cs) : snd acc)
                    (Nothing, [], Just m)  -> ([], m : snd acc)
                    (Nothing, cs, Just m)  -> ([], m : Plain (reverse cs) : snd acc)
                    (Just c,  cs, Just m)  -> ([], m : Plain (reverse $ c : cs) : snd acc)
{-# LINE 709 "org/parser.org" #-}
                                                                                          ) (Nothing, Just markup)
  )
  <|>
  (do
      (maybePre, markup) <- (
{-# LINE 577 "org/parser.org" #-}
                             P.try $ do
                               (pre, marker) <-
                                 (>>= parserTrace "Text:Markup:/Start")
                                 (
{-# LINE 550 "org/parser.org" #-}
                                  P.try $ liftM2 (,)
                                    (
{-# LINE 527 "org/parser.org" #-}
                                     (beginningOfLine *> return Nothing)
                                     <|> (Just <$> satisfy (\c -> isSpace c || elem c "({'\""))
                                     :: Parser (Maybe Char)
{-# LINE 551 "org/parser.org" #-}
                                                           )
                                    (P.oneOf "*~/+=")
                                  :: Parser (Maybe Char, Char)
{-# LINE 580 "org/parser.org" #-}
                                                              )
                               p <- P.getPosition
                               body <-
                                 (>>= parserTrace "Text:Markup:/BodyA")
                                 (manyTill anyChar (
{-# LINE 562 "org/parser.org" #-}
                                                    P.try $
                                                      void (P.char marker)
                                                      <* P.lookAhead (
{-# LINE 537 "org/parser.org" #-}
                                                                      (P.eof *> return Nothing)
                                                                      <|> Just <$> satisfy (\c -> isSpace c || elem c "-.,:!?)}'\"")
                                                                      <|> Just <$> endOfLine
                                                                      :: Parser (Maybe Char)
{-# LINE 564 "org/parser.org" #-}
                                                                                            )
                                                    :: Parser ()
{-# LINE 584 "org/parser.org" #-}
                                                                ))
                               _ <- char marker
                               return . (pre,) $ case marker of
                                 '*' -> Bold $ textFromString p body
                                 '~' -> InlineCode body
                                 '/' -> Italics $ textFromString p body
                                 '+' -> StrikeThrough body
                                 '=' -> Verbatim body
                                 e   -> error $ "unknown markup marker '" ++ show e ++ "'"
                             :: Parser (Maybe Char, TextElement)
{-# LINE 713 "org/parser.org" #-}
                                                                ) >>= parserTrace "Text:/Markup"
      scanText $ (
{-# LINE 681 "org/parser.org" #-}
                  \(newChar, newMarkup) -> case (newChar, fst acc, newMarkup) of
                    (Nothing, [], Nothing) -> ([], snd acc)
                    (Nothing, cs, Nothing) -> ([], Plain (reverse cs) : snd acc)
                    (Just c,  cs, Nothing) -> ([], Plain (reverse $ c : cs) : snd acc)
                    (Nothing, [], Just m)  -> ([], m : snd acc)
                    (Nothing, cs, Just m)  -> ([], m : Plain (reverse cs) : snd acc)
                    (Just c,  cs, Just m)  -> ([], m : Plain (reverse $ c : cs) : snd acc)
{-# LINE 714 "org/parser.org" #-}
                                                                                          ) (maybePre, Just markup)
  )
  <|> (anyChar >>= scanText . (,text) . (:plain))
{-# LINE 176 "org/scaffold.org" #-}
newtype Stack a = Stack [a]

{-# LINE 178 "org/scaffold.org" #-}
instance Pretty a => Pretty (Stack a) where
  pretty (Stack xs) = pretty xs

{-# LINE 181 "org/scaffold.org" #-}
top :: Stack a -> a
top (Stack xs) = head xs

{-# LINE 184 "org/scaffold.org" #-}
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack $ x:xs

{-# LINE 187 "org/scaffold.org" #-}
pop :: Stack a -> Stack a
pop (Stack []) = error $ "popping empty stack"
pop (Stack (_:xs)) = Stack xs

{-# LINE 191 "org/scaffold.org" #-}
resize :: Int -> Stack a -> Stack a
resize i (Stack xs)
  | i == l = Stack xs
  | i < l = Stack $ reverse . take i . reverse $ xs
  | i > l && l > 0 = Stack $ replicate (i - l) (head xs) ++ xs
  | otherwise = error $ "resizing empty stack"
  where
    l = length xs
{-# LINE 87 "ccweb.org" #-}
initialParserState :: ParserState
initialParserState = ParserState
  { sectionCounter = 0
  , propertyStack  = Stack []
  , parserLogLevel = Debug
  , evalContext    = Map.empty
  }
{-# LINE 98 "ccweb.org" #-}
parserDebug :: (Pretty a, Pretty b) => a -> b -> Parser b
parserDebug l s = do
  lvl <- parserLogLevel <$> P.getState
  pos <- P.getPosition
  let label = PP.hcat [pretty pos, PP.colon, pretty red, pretty l, pretty reset, PP.colon]
  if lvl >= Trace
  then trace (PP.render (PP.hang label 4 (pretty s))) $ return s
  else return s

{-# LINE 107 "ccweb.org" #-}
parserTrace :: (Pretty a, Pretty b) => a -> b -> Parser b
parserTrace l s = do
  (OrgLines ls) <- P.stateInput <$> P.getParserState
  u <- P.getState
  let user = pretty "user state" PP.<> PP.colon PP.<> pretty u
      input = pretty "looking at" PP.<> PP.colon PP.<> pretty (take 3 ls)
  _ <- parserDebug l (PP.fcat [pretty s, user, input])
  return s

{-# LINE 116 "ccweb.org" #-}
red :: String
red = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red]

{-# LINE 119 "ccweb.org" #-}
reset :: String
reset = ANSI.setSGRCode [ANSI.Reset]
{-# LINE 132 "ccweb.org" #-}
instance IsList OrgLines where
  type Item OrgLines = OrgLine
  fromList xs = OrgLines xs
  toList (OrgLines xs) = xs
{-# LINE 142 "ccweb.org" #-}
line :: Parser a -> Parser a
line p = p <* endOfLine
{-# LINE 158 "ccweb.org" #-}
instance Parse CodeElement where
  parse = do
    pos <- P.getPosition
    either id (Literal pos) <$> eitherMany1 (P.try $ 
{-# LINE 166 "ccweb.org" #-}
                                                     do
                                                       _ <- P.try $ P.string "<<"
                                                       p <- P.getPosition
                                                       SectionReference p <$> textTill (P.string ">>")
{-# LINE 161 "ccweb.org" #-}
                                                                                                      ) anyChar
{-# LINE 174 "ccweb.org" #-}
instance Parse CodeLine where
  parse = line (CodeLine <$> (P.many parse :: Parser [CodeElement]))
{-# LINE 182 "ccweb.org" #-}
instance Parse Headline where
  parse = do
    _ <- parserTrace "Headline" ()
    (char '§' *> spaces *> return EmptyHeadline)
    <|> (do
            s <- P.try (char '*') *> P.many (char '*')
            _ <- spaces1
            t <- parse :: Parser Text
            _ <- parserTrace "/Headline" ()
            return $ Headline (length s) t
        )
{-# LINE 197 "ccweb.org" #-}
headlineLevel :: Headline -> Int
headlineLevel (Headline x _) = x
headlineLevel EmptyHeadline = error $ "no level for empty headline"
{-# LINE 206 "ccweb.org" #-}
instance Parse SourceBlock where
  parse = do
    _ <- parserTrace "SourceBlock" ()
    n  <- P.optionMaybe (
{-# LINE 238 "ccweb.org" #-}
                         P.try (spaces *> P.string "#+NAME:") *> (trim <$> parse)
                         >>= parserTrace "SourceBlock:/Name"
                         :: Parser Text
{-# LINE 209 "ccweb.org" #-}
                                       )
    i  <- P.try (spaces <* (P.string "#+BEGIN_SRC" *> spaces1))
    _ <- parserTrace "SourceBlock:Language" ()
    l  <- symbol <* spaces
    _ <- parserTrace "SourceBlock:Properties" ()
    ps <- 
{-# LINE 456 "org/parser.org" #-}
          line (Map.fromList
                <$> P.sepEndBy
                 (liftM2 (,)
                   (symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces)))
                   (parse :: Parser SExpr))
                 spaces1)
{-# LINE 215 "ccweb.org" #-}
    ps' <- Map.union ps . headerArgs . top . propertyStack <$> P.getState
    p  <- P.getPosition
    _ <- parserTrace "SourceBlock:Lines" ()
    ls <- P.manyTill (P.string i *> parse :: Parser CodeLine)
         (P.try (line (P.string i *> P.string "#+END_SRC" *> spaces)))
    _ <- parserTrace "/SourceBlock" ()
    state <- P.getState
    return $ SourceBlock
      { blockName = n
      , blockLanguage = l
      , blockProperties = ps
      , blockDerivedProperties = eval state ps'
      , blockLocation = p
      , blockLines = ls
      }
{-# LINE 247 "ccweb.org" #-}
skipEmptyLines :: Parser ()
skipEmptyLines = do
  _ <- parserTrace "SkipEmptyLines" ()
  P.skipMany $ P.try (spaces *> endOfLine)
{-# LINE 259 "ccweb.org" #-}
enclosed :: Char -> Parser String
enclosed d = P.try (P.char d) *> P.manyTill anyChar (P.char d)
{-# LINE 267 "ccweb.org" #-}
instance Parse Section where
  parse = do
    _ <- parserTrace "Section" ()

{-# LINE 271 "ccweb.org" #-}
    skipEmptyLines
    _ <- parserTrace "Section:Maybe Headline" ()
    h <- fromMaybe EmptyHeadline <$> P.optionMaybe parse :: Parser Headline

{-# LINE 275 "ccweb.org" #-}
    let emptyHead = case h of { EmptyHeadline -> True; _ -> False }

{-# LINE 277 "ccweb.org" #-}
    skipEmptyLines
    _ <- parserTrace "Section:Properties" ()
    ps <- if emptyHead
      then return Map.empty
      else parse <|> return Map.empty :: Parser Properties
    _ <- parserDebug "Section:/Properties" ps

{-# LINE 284 "ccweb.org" #-}
    when (not emptyHead) $ do
      stk' <- resize (1 + (headlineLevel h)) . propertyStack <$> P.getState
      P.updateState (\s -> s{ propertyStack = stk' })
      ps' <- Map.unionWith Map.union ps . top . propertyStack <$> P.getState
      P.updateState (\s -> s{ propertyStack = push ps' stk' })
      P.getState >>= void . parserDebug "Section:Parser properties" . propertyStack

{-# LINE 291 "ccweb.org" #-}
    skipEmptyLines
    _ <- parserTrace "Section:Maybe [Text]" ()
    tss <- P.optionMaybe . P.many1 $ (P.many1 (P.try parse) <* skipEmptyLines)
          :: Parser (Maybe [[Text]])
    let ts = intercalate [Text [Plain []]] <$> tss
    _ <- parserTrace "Section:/Maybe [Text]" tss

{-# LINE 298 "ccweb.org" #-}
    skipEmptyLines
    _ <- parserTrace "Section:Maybe SourceBlock" ()
    c <- case (ts, h) of
          (Nothing, EmptyHeadline) -> Just <$> (parse :: Parser SourceBlock)
          (_, _) -> P.optionMaybe $ parse :: Parser (Maybe SourceBlock)

{-# LINE 304 "ccweb.org" #-}
    skipEmptyLines

{-# LINE 306 "ccweb.org" #-}
    _ <- parserTrace "/Section" ()

{-# LINE 308 "ccweb.org" #-}
    succeedWhen ((not emptyHead) || isJust ts || isJust c)

{-# LINE 310 "ccweb.org" #-}
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
{-# LINE 38 "org/scaffold.org" #-}
data Options = Options
  { optionQuiet :: Bool
  , optionVerbosity :: Int
  , dryRun :: Bool
  , listInputFiles :: Bool
  , listOutputFiles :: Bool
  , listMakeDeps :: Bool
  , inputFile :: String
  }

{-# LINE 48 "org/scaffold.org" #-}
instance Pretty Options where
  pretty o = pretty $ PrettyStruct "Options"
    [ ("quiet", pretty $ optionQuiet o)
    , ("verbosity", pretty $ optionVerbosity o)
    , ("dry-run", pretty $ dryRun o)
    , ("list-input-files", pretty $ listInputFiles o)
    , ("list-output-files", pretty $ listOutputFiles o)
    , ("list-make-deps", pretty $ listMakeDeps o)
    , ("input file", pretty $ inputFile o)
    ]

{-# LINE 59 "org/scaffold.org" #-}
userOptionParser :: O.Parser Options
userOptionParser =
  O.helper
  <*> 
{-# LINE 155 "org/scaffold.org" #-}
      O.infoOption (showVersion version)
        ( O.long "version"
        <> O.help "Output version information and exit."
        )
{-# LINE 63 "org/scaffold.org" #-}
  <*> ( Options
        <$> (
{-# LINE 89 "org/scaffold.org" #-}
             O.switch
               ( O.short 'q'
                 <> O.long "quiet"
                 <> O.help "Only print things that were asked for."
               )
{-# LINE 64 "org/scaffold.org" #-}
                )
        <*> (
{-# LINE 98 "org/scaffold.org" #-}
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
{-# LINE 127 "org/scaffold.org" #-}
            O.switch
              ( O.short 'I'
              <> O.long "list-input-files"
              <> O.help "If given, list the files that have to be read in, one per line."
              )
{-# LINE 68 "org/scaffold.org" #-}
        <*> 
{-# LINE 118 "org/scaffold.org" #-}
            O.switch
              ( O.short 'O'
              <> O.long "list-output-files"
              <> O.help "If given, list the files that would be written out, one per line."
              )
{-# LINE 69 "org/scaffold.org" #-}
        <*> 
{-# LINE 136 "org/scaffold.org" #-}
            O.switch
              ( O.short 'M'
              <> O.long "list-make-dependencies"
              <> O.help "If given, list the input and output files in a Makefile format."
              )
{-# LINE 70 "org/scaffold.org" #-}
        <*> (
{-# LINE 110 "org/scaffold.org" #-}
             O.argument O.str
               ( O.metavar "FILE"
               <> O.help "The name of the input file."
               )
{-# LINE 70 "org/scaffold.org" #-}
                )
      )
{-# LINE 76 "org/scaffold.org" #-}
logLevel :: Options -> LogLevel
logLevel Options{ optionQuiet = q, optionVerbosity = v } =
  case (if q then 0 else 3) + v of
    0 -> Quiet
    1 -> Error
    2 -> Warning
    3 -> Info
    4 -> Debug
    _ -> Trace
{-# LINE 145 "org/scaffold.org" #-}
dryRunParser :: O.Parser Bool
dryRunParser = O.switch
  ( O.short 'n'
  <> O.long "dry-run"
  <> O.help "If given, do not write any files. Useful when debugging."
  )
{-# LINE 119 "org/parser.org" #-}
fromEither :: Pretty a => Either a b -> b
fromEither = either (error . PP.render . pretty) id
{-# LINE 10 "org/tangle.org" #-}
main :: IO ()
main = do
  opts <- O.execParser optParser
  logM (logLevel opts) Info $ "This is CCTANGLE " ++ showVersion version

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
{-# LINE 485 "org/doc.org" #-}
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
  let genFiles = map fst outs
      inpFiles = nub . sort . map (\(OrgLine p _) -> P.sourceName p) $ (toList ingested)
  when (listOutputFiles opts) $
    mapM_ putStrLn genFiles
  when (listInputFiles opts) $
    mapM_ putStrLn inpFiles
  when (listMakeDeps opts) $
    mapM_ putStrLn (map ((intercalate " " genFiles ++ ":") ++) inpFiles)
{-# LINE 22 "org/tangle.org" #-}
  logM (logLevel opts) Debug $ PP.render (pretty outs)

{-# LINE 24 "org/tangle.org" #-}
  mapM_ (tangleFile opts doc) outs
  logM (logLevel opts) Info $ "Done."
  where
    optParser = O.info userOptionParser
      ( O.fullDesc
      <> O.progDesc "Tangle the input FILE"
      <> O.header "cctangle - A literate programming tangler" )
{-# LINE 73 "org/tangle.org" #-}
data Line = CodeText String | LinePragma String P.SourcePos
{-# LINE 81 "org/tangle.org" #-}
instance Show Line where
  show (CodeText str) = str ++ "\n"
{-# LINE 91 "org/tangle.org" #-}
  show (LinePragma "haskell" pos) = unwords
    [ "{-# LINE", show (P.sourceLine pos)
    , "\"" ++ P.sourceName pos ++ "\""
    , "#-}\n" ]
{-# LINE 101 "org/tangle.org" #-}
  show (LinePragma "c" pos) = unwords
    [ "#line", show (P.sourceLine pos)
    , "\"" ++ P.sourceName pos ++ "\"\n"
    ]
  show (LinePragma "c++" pos) = show (LinePragma "c" pos)
{-# LINE 113 "org/tangle.org" #-}
  show (LinePragma "conf" pos) = show (LinePragma "c" pos)
{-# LINE 121 "org/tangle.org" #-}
  show (LinePragma ('>':'>':pre) pos) =
    pre ++ P.sourceName pos ++ ":" ++ show (P.sourceLine pos) ++ "\n"
{-# LINE 84 "org/tangle.org" #-}
  show (LinePragma _ _) = []
{-# LINE 150 "org/tangle.org" #-}
type Indent = String
{-# LINE 161 "org/tangle.org" #-}
data TangleState = TangleState
  { document :: Document
  , indent :: Indent
  , blocks :: Stack SourceBlock
  , pragmaLanguage :: String
  }
{-# LINE 174 "org/tangle.org" #-}
type Tangler a = State TangleState a

{-# LINE 176 "org/tangle.org" #-}
getTopBlock :: Tangler SourceBlock
getTopBlock = top . blocks <$> get

{-# LINE 179 "org/tangle.org" #-}
getIndent :: Tangler Indent
getIndent = indent <$> get

{-# LINE 182 "org/tangle.org" #-}
setIndentFrom :: String -> Tangler ()
setIndentFrom str = modify
  (\s -> s{ indent = replicate (length str) ' ' })

{-# LINE 186 "org/tangle.org" #-}
addIndentFrom :: String -> Tangler ()
addIndentFrom str = modify
  (\s -> s{ indent = indent s ++ replicate (length str) ' ' })
{-# LINE 197 "org/tangle.org" #-}
tangleSourceBlock :: SourceBlock -> Tangler [Line]
tangleSourceBlock block = do
  modify (\s -> s{ blocks = push block (blocks s) })
  ls <- concat <$> mapM tangleCodeLine (blockLines block)
  modify (\s -> s{ blocks = pop (blocks s) })
  maybeAddLinePragma ls (blockLocation block) block
{-# LINE 207 "org/tangle.org" #-}
maybeAddLinePragma :: [Line] -> P.SourcePos -> SourceBlock -> Tangler [Line]
maybeAddLinePragma ls pos block = do
  lang <- pragmaLanguage <$> get
  return $ case headerArg ":comments" block of
    Nothing -> ls
    Just comments ->
      if not (toBool comments) then ls
      else
        let pragma = case comments of
              Atom "t" -> LinePragma lang pos
              Atom "yes" -> LinePragma lang pos
              Atom prefix -> LinePragma ('>':'>':prefix) pos
              _ -> LinePragma lang pos
        in case (show pragma) of
             (_:_) -> pragma:ls
             _ -> ls
{-# LINE 231 "org/tangle.org" #-}
tangleCodeLine :: CodeLine -> Tangler [Line]
tangleCodeLine (CodeLine []) = (:[]) . CodeText <$> getIndent
{-# LINE 260 "org/tangle.org" #-}
tangleCodeLine (CodeLine elements) =
  do
    initialIndent <- getIndent
    ls <- removeLeadingEmptyLine . 
{-# LINE 253 "org/tangle.org" #-}
                                   reverse <$> foldlM
                                     (
{-# LINE 296 "org/tangle.org" #-}
                                      \acc element -> do
                                        let (accPragmas, accRest) = breakCodeText acc
                                        case (accPragmas, accRest, element) of
{-# LINE 309 "org/tangle.org" #-}
                                          ([], (CodeText l:ls), Literal _ s) -> do
                                            addIndentFrom s
                                            return $ CodeText (l ++ s) : ls
{-# LINE 321 "org/tangle.org" #-}
                                          (_, _, Literal pos s) -> do
                                            i <- getIndent
                                            addIndentFrom s
                                            getTopBlock >>= maybeAddLinePragma (CodeText (i ++ s) : acc) pos
{-# LINE 340 "org/tangle.org" #-}
                                          (_, (CodeText l:ls), SectionReference pos name) -> do
                                            refLines <- 
{-# LINE 360 "org/tangle.org" #-}
                                                        do
                                                          bs <- 
{-# LINE 368 "org/tangle.org" #-}
                                                                filter (\b -> case (blockName b) of
                                                                               Nothing -> False
                                                                               Just t -> t == name)
                                                                  . mapMaybe sectionSourceBlock
                                                                  . sections
                                                                  . document
                                                                  <$> get
{-# LINE 361 "org/tangle.org" #-}
                                                                          :: Tangler [SourceBlock]
                                                          when (null bs) (error $ "source block not defined anywhere: " ++ show name)
                                                          concat <$> mapM tangleSourceBlock bs :: Tangler [Line]
{-# LINE 342 "org/tangle.org" #-}
                                            case breakCodeText refLines of
                                              (refPragmas, (CodeText first : rest)) -> do
                                                acc' <- case (refPragmas, accPragmas) of
                                                  ([], []) -> do
                                                    unindented <- 
{-# LINE 379 "org/tangle.org" #-}
                                                                  (\str -> do
                                                                     i <- getIndent
                                                                     return $ if isPrefixOf i str; then drop (length i) str; else str
                                                                  )
{-# LINE 346 "org/tangle.org" #-}
                                                                    first
                                                    addIndentFrom unindented
                                                    return $ reverse rest ++ (CodeText(l ++ unindented) : ls)
                                                  _ -> do
                                                    let reverseRefLines = reverse refLines
                                                        CodeText lst = head . snd $ breakCodeText reverseRefLines
                                                    setIndentFrom lst
                                                    return $ reverseRefLines ++ acc
                                                getTopBlock >>= maybeAddLinePragma acc' pos
                                              _ -> error $ "empty section reference: " ++ show name
{-# LINE 300 "org/tangle.org" #-}
                                          _ -> error $ "unreachable element tangling case"
{-# LINE 254 "org/tangle.org" #-}
                                                                                          )
                                     [CodeText initialIndent] elements
{-# LINE 264 "org/tangle.org" #-}
    modify (\s -> s{ indent = initialIndent })
    getTopBlock >>= maybeAddLinePragma ls ((
{-# LINE 276 "org/tangle.org" #-}
                                            \case
                                              (Literal p _) -> p
                                              (SectionReference p _) -> p
{-# LINE 265 "org/tangle.org" #-}
                                                                         ) $ head elements)
      where
        removeLeadingEmptyLine :: [Line] -> [Line]
        removeLeadingEmptyLine a@(CodeText l:p@(LinePragma _ _):ls)
          | all isSpace l = p:ls
          | otherwise = a
        removeLeadingEmptyLine a = a
{-# LINE 286 "org/tangle.org" #-}
breakCodeText :: [Line] -> ([Line],[Line])
breakCodeText = break (\case { (CodeText _) -> True; _ -> False })
{-# LINE 390 "org/tangle.org" #-}
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
{-# LINE 132 "org/tangle.org" #-}
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
{-# LINE 403 "org/tangle.org" #-}
                                           ls
  logM (logLevel opts) Info $ "Writing one output file (" ++ fp ++ ")..."
  if dryRun opts
    then logM (logLevel opts) Info contents
    else do
      let dir = F.takeDirectory fp
      when (mkDir && dir /= ".") $ D.createDirectoryIfMissing True dir

{-# LINE 411 "org/tangle.org" #-}
      F.fileExist fp >>= (`when` F.removeLink fp)
      writeFile fp contents

{-# LINE 414 "org/tangle.org" #-}
      case headerArg ":tangle-mode" block of
        Nothing -> return ()
        Just (IntAtom mode) -> F.setFileMode fp $ fromIntegral mode
        Just e -> error $ "unsupported tangle-mode: " ++ show e
  where
    mkDir = case (toBool <$> headerArg ":mkdirp" (head bs)) of
              Just x -> x
              _ -> False
{-# LINE 5 "org/tangle.org" #-}
