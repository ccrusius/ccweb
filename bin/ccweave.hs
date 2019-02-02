{-# LINE 67 "ccweb.org" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 52 "ccweb.org" #-}
import Control.Applicative ((<|>), (<**>))
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
{-# LINE 384 "org/parser.org" #-}
import qualified System.Info as Sys
import qualified Control.Exception as E
{-# LINE 397 "org/tangle.org" #-}
import qualified System.Directory as D
import qualified System.Environment as Env
import qualified System.FilePath as F
import qualified System.Posix.Files as F
{-# LINE 33 "org/scaffold.org" #-}
import qualified Options.Applicative as O
{-# LINE 180 "org/scaffold.org" #-}
import qualified Text.PrettyPrint as PP
{-# LINE 137 "ccweb.org" #-}
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
{-# LINE 68 "org/doc.org" #-}
data Section = Section
  { sectionNumber :: Int
  , sectionHeadline :: Headline
  , documentation :: [Text]
  , sectionSourceBlock :: Maybe SourceBlock
  , sectionProps :: Properties
  , sectionDerivedProperties :: Properties
  }
{-# LINE 100 "org/doc.org" #-}
data Headline = Headline Int Text | EmptyHeadline deriving Show
{-# LINE 122 "org/doc.org" #-}
type Property   = Map.Map String SExpr
type Properties = Map.Map String Property
{-# LINE 131 "org/doc.org" #-}
data SExpr =
  Atom String
  | IntAtom Int
  | SExpr [SExpr]
  deriving (Eq, Ord)
{-# LINE 143 "org/doc.org" #-}
fromBool :: Bool -> SExpr
fromBool False = SExpr []
fromBool True = Atom "t"
{-# LINE 154 "org/doc.org" #-}
toBool :: SExpr -> Bool
toBool (Atom _) = True
toBool (IntAtom _) = True
toBool _ = False
{-# LINE 189 "org/doc.org" #-}
class Eval a where
  eval :: ParserState -> a -> a
{-# LINE 197 "org/doc.org" #-}
instance Eval Property where; eval s = Map.map (eval s)
instance Eval Properties where; eval s = Map.map (eval s)
{-# LINE 206 "org/doc.org" #-}
instance Eval SExpr where
{-# LINE 214 "org/doc.org" #-}
  eval s (SExpr [Atom "identity", expr]) = eval s expr
{-# LINE 222 "org/doc.org" #-}
  eval _ (Atom ('#':'o':x:y:z:[])) =
    IntAtom $ 8 * ((8 * oct x) + oct y) + oct z
    where oct c = ord c - ord '0'
{-# LINE 231 "org/doc.org" #-}
  eval s (SExpr [Atom "eq", e1, e2]) = fromBool $ (eval s e1) == (eval s e2)
  
{-# LINE 233 "org/doc.org" #-}
  eval s (SExpr [Atom "string-prefix-p", prefix, expr]) =
    case (eval s prefix, eval s expr) of
      (Atom s', Atom str) -> fromBool $ isPrefixOf s' str
      (e1, e2) -> SExpr [Atom "string-prefix-p", e1, e2]
  
{-# LINE 238 "org/doc.org" #-}
  eval s (SExpr [Atom "string-suffix-p", suffix, expr]) =
    case (eval s suffix, eval s expr) of
      (Atom s', Atom str) -> fromBool $ isSuffixOf s' str
      (e1, e2) -> SExpr [Atom "string-suffix-p", e1, e2]
{-# LINE 248 "org/doc.org" #-}
  eval s (SExpr [Atom "when", expr, result]) =
    if toBool (eval s expr)
    then eval s result
    else fromBool False
  
{-# LINE 253 "org/doc.org" #-}
  eval s (SExpr [Atom "unless", expr, result]) =
    if toBool (eval s expr)
    then fromBool False
    else eval s result
{-# LINE 264 "org/doc.org" #-}
  eval s expr = Map.findWithDefault expr expr (evalContext s)
{-# LINE 277 "org/doc.org" #-}
class HeaderArgs a where
  headerArgs :: a -> Property
  headerArg :: String -> a -> Maybe SExpr
  headerArg k a = Map.lookup k $ headerArgs a

{-# LINE 282 "org/doc.org" #-}
instance HeaderArgs Properties where
  headerArgs = Map.findWithDefault Map.empty "header-args"

{-# LINE 285 "org/doc.org" #-}
instance HeaderArgs Section where
  headerArgs = headerArgs . sectionDerivedProperties
{-# LINE 301 "org/doc.org" #-}
data SourceBlock = SourceBlock
  { blockName :: Maybe Text
  , blockLanguage :: String
  , blockLines :: [CodeLine]
  , blockLocation :: P.SourcePos
  , blockProperties :: Property
  , blockDerivedProperties :: Property
  }
{-# LINE 330 "org/doc.org" #-}
instance HeaderArgs SourceBlock where
  headerArgs = blockDerivedProperties
{-# LINE 341 "org/doc.org" #-}
data SourceBlockId = FileBlock FilePath | NamedBlock Text deriving Eq

{-# LINE 343 "org/doc.org" #-}
instance Ord SourceBlockId where
  (<=) (FileBlock p1) (FileBlock p2) = p1 <= p2
  (<=) (NamedBlock p1) (NamedBlock p2) = p1 <= p2
  (<=) (FileBlock _) (NamedBlock _) = False
  (<=) _ _ = True
{-# LINE 356 "org/doc.org" #-}
sourceBlockId :: SourceBlock -> Maybe SourceBlockId
sourceBlockId SourceBlock{ blockName = (Just name) } = Just $ NamedBlock name
sourceBlockId block = case headerArg ":tangle" block of
    Nothing -> Nothing
    Just (Atom "t") -> Nothing
    Just (SExpr []) -> Nothing
    Just (Atom f) -> Just $ FileBlock f
    Just e -> error $ "unsupported tangle destination: " ++ show e
{-# LINE 371 "org/doc.org" #-}
newtype CodeLine = CodeLine [CodeElement]
data CodeElement = Literal P.SourcePos String
                 | SectionReference P.SourcePos Text
{-# LINE 400 "org/doc.org" #-}
data Text = Text [TextElement] deriving (Eq, Ord, Show)

{-# LINE 402 "org/doc.org" #-}
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
{-# LINE 445 "org/doc.org" #-}
trim :: Text -> Text
trim (Text (Plain []:ys))       = trim $ Text ys
trim (Text (Plain (' ':xs):ys)) = trim $ Text (Plain xs:ys)
trim  t                         = t
{-# LINE 455 "org/doc.org" #-}
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
  pretty EmptyHeadline = PP.char '§'
{-# LINE 165 "org/doc.org" #-}
instance Show SExpr where
  show (Atom x) = x
  show (IntAtom x) = show x
  show (SExpr []) = "nil"
  show (SExpr xs) = "(" ++ intercalate " " (map show xs) ++ ")"

{-# LINE 171 "org/doc.org" #-}
instance Pretty SExpr where
  pretty = PP.text . show
{-# LINE 315 "org/doc.org" #-}
instance Pretty SourceBlock where
  pretty = prettyStruct "SourceBlock"
             [ ("name", pretty . blockName)
             , ("language", pretty . blockLanguage)
             , ("properties", pretty . blockProperties)
             , ("derived properties", pretty . blockDerivedProperties)
             , ("location", pretty . blockLocation)
             , ("lines", pretty . blockLines)
             ]
{-# LINE 382 "org/doc.org" #-}
instance Pretty CodeLine where
  pretty (CodeLine xs) = pretty xs

{-# LINE 385 "org/doc.org" #-}
instance Pretty CodeElement where
  pretty (Literal p s) =
    PP.parens $ PP.hcat [pretty p, PP.colon, pretty s]
  pretty (SectionReference p t) =
    PP.hcat [pretty p, PP.colon, PP.char '〈', pretty t, PP.char '〉']
{-# LINE 421 "org/doc.org" #-}
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
{-# LINE 165 "org/parser.org" #-}
instance Pretty OrgLines where
  pretty (OrgLines ls) = pretty ls
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
instance Pretty () where
  pretty _ = PP.text ""

{-# LINE 216 "org/scaffold.org" #-}
instance (Pretty a, Pretty b) => Pretty (a,b) where
  pretty (a,b) = PP.parens . PP.fcat . PP.punctuate PP.comma $ [pretty a, pretty b]

{-# LINE 219 "org/scaffold.org" #-}
instance Pretty a => Pretty [a] where
  pretty = prettyList

{-# LINE 222 "org/scaffold.org" #-}
instance Pretty P.ParseError where
  pretty = PP.text . show
{-# LINE 228 "org/scaffold.org" #-}
instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
  pretty = PP.brackets
           . PP.fcat
           . PP.punctuate PP.comma
           . map (\(k,v) -> pretty k PP.<> PP.equals PP.<> pretty v)
           . Map.toList
{-# LINE 238 "org/scaffold.org" #-}
data PrettyStruct = PrettyStruct String [(String, PP.Doc)]

{-# LINE 240 "org/scaffold.org" #-}
instance Pretty PrettyStruct where
  pretty (PrettyStruct t ps) =
    let fields = map (\(n,p) -> PP.text n PP.<> PP.colon PP.<+> p) ps
    in PP.text t PP.<> PP.braces (PP.fcat $ PP.punctuate PP.comma fields)

{-# LINE 245 "org/scaffold.org" #-}
prettyStruct :: String -> [(String, (a -> PP.Doc))] -> a -> PP.Doc
prettyStruct name kfs a = pretty (PrettyStruct name (map (\(k,f) -> (k,f a)) kfs))
{-# LINE 251 "org/scaffold.org" #-}
instance Pretty P.SourcePos where
  pretty p = pretty (P.sourceName p)
             PP.<> PP.colon PP.<> pretty (P.sourceLine p)
             PP.<> PP.colon PP.<> pretty (P.sourceColumn p)
{-# LINE 167 "ccweb.org" #-}
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
{-# LINE 93 "org/parser.org" #-}
ingest :: FilePath -> IO [OrgLine]
ingest file = do
    zipWith OrgLine [P.newPos file l 1 | l <- [1..]] . map (++"\n") . lines
      <$> readFile file
      >>= foldrM scan []
  where
    scan l@(OrgLine pos s) ls =
      maybe (return (l : ls)) (fmap (++ ls) . ingest)
      $ (
{-# LINE 111 "org/parser.org" #-}
         fromEither . P.parse
         (P.setPosition pos *>
             P.optionMaybe (
               P.try (P.spaces *> P.string "#+INCLUDE:" *> P.spaces)
               *> P.char '"'
               *> P.manyTill anyChar (P.char '"')
             ))
         (P.sourceName pos)
{-# LINE 101 "org/parser.org" #-}
                            :: String -> Maybe FilePath) s
{-# LINE 135 "org/parser.org" #-}
anyChar :: (P.Stream s m Char) => P.ParsecT s u m Char
anyChar = P.noneOf "\n\r"
{-# LINE 144 "org/parser.org" #-}
instance Monad m => P.Stream OrgLine m Char where
  uncons (OrgLine _ []) =
    return Nothing
  uncons (OrgLine p (x:xs)) =
    return $ Just (x, OrgLine (P.updatePosChar p x) xs)
{-# LINE 158 "org/parser.org" #-}
newtype OrgLines = OrgLines [OrgLine]
{-# LINE 175 "org/parser.org" #-}
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
{-# LINE 267 "org/parser.org" #-}
beginningOfLine :: Parser ()
beginningOfLine = do
  c <- P.sourceColumn <$> P.getPosition
  succeedWhen (c == 1)
{-# LINE 278 "org/parser.org" #-}
succeedWhen :: Bool -> Parser ()
succeedWhen True = return ()
succeedWhen False = void $ satisfy (\_ -> False)

{-# LINE 282 "org/parser.org" #-}
failWhen :: Bool -> Parser ()
failWhen = succeedWhen . not
{-# LINE 295 "org/parser.org" #-}
manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p = P.manyTill p . P.lookAhead

{-# LINE 298 "org/parser.org" #-}
many1Till :: Parser a -> Parser end -> Parser [a]
many1Till p = liftM2 (:) p . manyTill p
{-# LINE 304 "org/parser.org" #-}
eitherMany1 :: Parser end -> Parser a -> Parser (Either end [a])
eitherMany1 end p =
  (Left <$> P.try end) <|>
  (Right <$> many1Till p (void endOfLine <|> void end))
{-# LINE 318 "org/parser.org" #-}
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
{-# LINE 337 "org/parser.org" #-}
readOrgFile :: LogLevel -> FilePath -> IO (OrgLines, Document)
readOrgFile lvl fp = do
  ingested <- fromList <$> ingest fp :: IO OrgLines
{-# LINE 355 "org/parser.org" #-}
  let styp = ( Atom "system-type"
             , Atom $ case Sys.os of
                        "linux" -> "gnu/linux"
                        "mingw32" -> "windows-nt"
                        o -> o
             )
      atomize k = fmap ((Atom k,) . Atom)
  hnam <- (SExpr [Atom "system-name"],) . Atom <$> getHostName
  pfam <- atomize "dmi/product-family"  <$> (
{-# LINE 374 "org/parser.org" #-}
                                             \p -> either (\_ -> Nothing) (Just . reverse . dropWhile isSpace . reverse)
                                                  <$> (E.try (readFile $ "/sys/devices/virtual/dmi/id" F.</> p)
                                                       :: IO (Either E.IOException String))
{-# LINE 363 "org/parser.org" #-}
                                                                                           ) "product_family"
  pnam <- atomize "dmi/product-name"    <$> (
{-# LINE 374 "org/parser.org" #-}
                                             \p -> either (\_ -> Nothing) (Just . reverse . dropWhile isSpace . reverse)
                                                  <$> (E.try (readFile $ "/sys/devices/virtual/dmi/id" F.</> p)
                                                       :: IO (Either E.IOException String))
{-# LINE 364 "org/parser.org" #-}
                                                                                           ) "product_name"
  pver <- atomize "dmi/product-version" <$> (
{-# LINE 374 "org/parser.org" #-}
                                             \p -> either (\_ -> Nothing) (Just . reverse . dropWhile isSpace . reverse)
                                                  <$> (E.try (readFile $ "/sys/devices/virtual/dmi/id" F.</> p)
                                                       :: IO (Either E.IOException String))
{-# LINE 365 "org/parser.org" #-}
                                                                                           ) "product_version"
{-# LINE 365 "org/parser.org" #-}
  
{-# LINE 367 "org/parser.org" #-}
  let ctx = Map.fromList $ styp : hnam : catMaybes [pfam, pnam, pver]
{-# LINE 341 "org/parser.org" #-}
  let doc = fromEither $ P.runParser
        (parse :: Parser Document)
        initialParserState{ evalContext = ctx, parserLogLevel = lvl }
        fp
        ingested
  return (ingested, doc)
{-# LINE 483 "org/parser.org" #-}
textFromString :: P.SourcePos -> String -> Text
textFromString p s = fromEither $ P.runParser
    (parse :: Parser Text)
    initialParserState
    (P.sourceName p)
    (OrgLines [OrgLine p s])
{-# LINE 497 "org/parser.org" #-}
textTill :: Parser end -> Parser Text
textTill end = do
  _ <- parserTrace "TextTill" ()
  p <- P.getPosition
  t <- P.manyTill anyChar end
  parserTrace "/TextTill" $ textFromString p t
{-# LINE 547 "org/parser.org" #-}
instance Parse Text where
  parse = do
    _ <- parserTrace "Text" ()
    ret <- reverse . snd <$> scanText ([], [])
    failWhen (null ret) P.<?> "empty text"
    parserTrace "/Text" $ Text ret
{-# LINE 579 "org/parser.org" #-}
scanText :: (String, [TextElement]) -> Parser (String, [TextElement])
scanText acc@(plain, text) =
{-# LINE 605 "org/parser.org" #-}
  (P.eof *> parserTrace "Text:/EOF"
    ((
{-# LINE 563 "org/parser.org" #-}
      \(newChar, newMarkup) -> case (newChar, fst acc, newMarkup) of
        (Nothing, [], Nothing) -> ([], snd acc)
        (Nothing, cs, Nothing) -> ([], Plain (reverse cs) : snd acc)
        (Just c,  cs, Nothing) -> ([], Plain (reverse $ c : cs) : snd acc)
        (Nothing, [], Just m)  -> ([], m : snd acc)
        (Nothing, cs, Just m)  -> ([], m : Plain (reverse cs) : snd acc)
        (Just c,  cs, Just m)  -> ([], m : Plain (reverse $ c : cs) : snd acc)
{-# LINE 606 "org/parser.org" #-}
                                                                              ) (Nothing, Nothing)))
  <|> (endOfLine *> parserTrace "Text:/EOL"
    ((
{-# LINE 563 "org/parser.org" #-}
      \(newChar, newMarkup) -> case (newChar, fst acc, newMarkup) of
        (Nothing, [], Nothing) -> ([], snd acc)
        (Nothing, cs, Nothing) -> ([], Plain (reverse cs) : snd acc)
        (Just c,  cs, Nothing) -> ([], Plain (reverse $ c : cs) : snd acc)
        (Nothing, [], Just m)  -> ([], m : snd acc)
        (Nothing, cs, Just m)  -> ([], m : Plain (reverse cs) : snd acc)
        (Just c,  cs, Just m)  -> ([], m : Plain (reverse $ c : cs) : snd acc)
{-# LINE 608 "org/parser.org" #-}
                                                                              ) (Nothing, Nothing)))
{-# LINE 582 "org/parser.org" #-}
  <|> (
{-# LINE 616 "org/parser.org" #-}
       do
         beginningOfLine
         P.lookAhead . P.choice $ map P.try
           [ void (spaces *> P.string "#+NAME")
           , void (spaces *> P.string "#+BEGIN_SRC")
           , void (parse :: Parser Headline)
           ]
         parserTrace "Text:/Break" $ (
{-# LINE 563 "org/parser.org" #-}
                                      \(newChar, newMarkup) -> case (newChar, fst acc, newMarkup) of
                                        (Nothing, [], Nothing) -> ([], snd acc)
                                        (Nothing, cs, Nothing) -> ([], Plain (reverse cs) : snd acc)
                                        (Just c,  cs, Nothing) -> ([], Plain (reverse $ c : cs) : snd acc)
                                        (Nothing, [], Just m)  -> ([], m : snd acc)
                                        (Nothing, cs, Just m)  -> ([], m : Plain (reverse cs) : snd acc)
                                        (Just c,  cs, Just m)  -> ([], m : Plain (reverse $ c : cs) : snd acc)
{-# LINE 623 "org/parser.org" #-}
                                                                                                              ) (Nothing, Nothing)
{-# LINE 582 "org/parser.org" #-}
                                                                                                                                  )
  <|>
  (do
      markup <- P.try (
{-# LINE 527 "org/parser.org" #-}
                       do
                         url <- P.string "[[" *> many1Till anyChar (P.string "][" <|> P.string "]]")
                         txt <- P.string "][" <|> P.string "]]" >>= \case
                           "]]" -> return $ Text [Verbatim url]
                           _    -> textTill (P.string "]]")
                         return $ HyperLink url txt
{-# LINE 585 "org/parser.org" #-}
                                                   ) >>= parserTrace "Text/Link"
      scanText $ (
{-# LINE 563 "org/parser.org" #-}
                  \(newChar, newMarkup) -> case (newChar, fst acc, newMarkup) of
                    (Nothing, [], Nothing) -> ([], snd acc)
                    (Nothing, cs, Nothing) -> ([], Plain (reverse cs) : snd acc)
                    (Just c,  cs, Nothing) -> ([], Plain (reverse $ c : cs) : snd acc)
                    (Nothing, [], Just m)  -> ([], m : snd acc)
                    (Nothing, cs, Just m)  -> ([], m : Plain (reverse cs) : snd acc)
                    (Just c,  cs, Just m)  -> ([], m : Plain (reverse $ c : cs) : snd acc)
{-# LINE 586 "org/parser.org" #-}
                                                                                          ) (Nothing, Just markup)
  )
  <|>
  (do
      markup <- P.try (
{-# LINE 512 "org/parser.org" #-}
                       (DisplayMath <$>
                         (P.try (P.string "$$")
                          *> P.manyTill P.anyChar (P.try $ P.string "$$")))
                       <|>
                       (InlineMath <$>
                         (P.char '$'
                          *> P.manyTill P.anyChar (P.char '$')))
                       :: Parser TextElement
{-# LINE 590 "org/parser.org" #-}
                                            ) >>= parserTrace "Text/TeX"
      scanText $ (
{-# LINE 563 "org/parser.org" #-}
                  \(newChar, newMarkup) -> case (newChar, fst acc, newMarkup) of
                    (Nothing, [], Nothing) -> ([], snd acc)
                    (Nothing, cs, Nothing) -> ([], Plain (reverse cs) : snd acc)
                    (Just c,  cs, Nothing) -> ([], Plain (reverse $ c : cs) : snd acc)
                    (Nothing, [], Just m)  -> ([], m : snd acc)
                    (Nothing, cs, Just m)  -> ([], m : Plain (reverse cs) : snd acc)
                    (Just c,  cs, Just m)  -> ([], m : Plain (reverse $ c : cs) : snd acc)
{-# LINE 591 "org/parser.org" #-}
                                                                                          ) (Nothing, Just markup)
  )
  <|>
  (do
      (maybePre, markup) <- (
{-# LINE 459 "org/parser.org" #-}
                             P.try $ do
                               (pre, marker) <-
                                 (>>= parserTrace "Text:Markup:/Start")
                                 (
{-# LINE 432 "org/parser.org" #-}
                                  P.try $ liftM2 (,)
                                    (
{-# LINE 409 "org/parser.org" #-}
                                     (beginningOfLine *> return Nothing)
                                     <|> (Just <$> satisfy (\c -> isSpace c || elem c "({'\""))
                                     :: Parser (Maybe Char)
{-# LINE 433 "org/parser.org" #-}
                                                           )
                                    (P.oneOf "*~/+=")
                                  :: Parser (Maybe Char, Char)
{-# LINE 462 "org/parser.org" #-}
                                                              )
                               p <- P.getPosition
                               body <-
                                 (>>= parserTrace "Text:Markup:/BodyA")
                                 (manyTill anyChar (
{-# LINE 444 "org/parser.org" #-}
                                                    P.try $
                                                      void (P.char marker)
                                                      <* P.lookAhead (
{-# LINE 419 "org/parser.org" #-}
                                                                      (P.eof *> return Nothing)
                                                                      <|> Just <$> satisfy (\c -> isSpace c || elem c "-.,:!?)}'\"")
                                                                      <|> Just <$> endOfLine
                                                                      :: Parser (Maybe Char)
{-# LINE 446 "org/parser.org" #-}
                                                                                            )
                                                    :: Parser ()
{-# LINE 466 "org/parser.org" #-}
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
{-# LINE 595 "org/parser.org" #-}
                                                                ) >>= parserTrace "Text:/Markup"
      scanText $ (
{-# LINE 563 "org/parser.org" #-}
                  \(newChar, newMarkup) -> case (newChar, fst acc, newMarkup) of
                    (Nothing, [], Nothing) -> ([], snd acc)
                    (Nothing, cs, Nothing) -> ([], Plain (reverse cs) : snd acc)
                    (Just c,  cs, Nothing) -> ([], Plain (reverse $ c : cs) : snd acc)
                    (Nothing, [], Just m)  -> ([], m : snd acc)
                    (Nothing, cs, Just m)  -> ([], m : Plain (reverse cs) : snd acc)
                    (Just c,  cs, Just m)  -> ([], m : Plain (reverse $ c : cs) : snd acc)
{-# LINE 596 "org/parser.org" #-}
                                                                                          ) (maybePre, Just markup)
  )
  <|> (anyChar >>= scanText . (,text) . (:plain))
{-# LINE 633 "org/parser.org" #-}
instance Parse Properties where
  parse = do
    _ <- parserTrace "Properties" ()
    ps <- (
{-# LINE 646 "org/parser.org" #-}
           do
             _ <- P.try (P.string "#+PROPERTY:") *> spaces
             liftM2 Map.singleton
               (symbol <* spaces1)
               (
{-# LINE 673 "org/parser.org" #-}
                line (Map.fromList
                      <$> P.sepEndBy
                       (liftM2 (,)
                         (symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces)))
                         (parse :: Parser SExpr))
                       spaces1)
{-# LINE 650 "org/parser.org" #-}
                               )
{-# LINE 636 "org/parser.org" #-}
                                ) <|> (
{-# LINE 658 "org/parser.org" #-}
                                       do
                                         _ <- P.try (spaces *> P.string ":PROPERTIES:" *> spaces *> endOfLine)
                                         Map.fromList <$> P.manyTill
                                           (liftM2 (,)
                                              (spaces *> (enclosed ':' <* spaces))
                                              (
{-# LINE 673 "org/parser.org" #-}
                                               line (Map.fromList
                                                     <$> P.sepEndBy
                                                      (liftM2 (,)
                                                        (symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces)))
                                                        (parse :: Parser SExpr))
                                                      spaces1)
{-# LINE 663 "org/parser.org" #-}
                                                              ))
                                           (P.try (spaces *> P.string ":END:" *> spaces *> endOfLine))
{-# LINE 636 "org/parser.org" #-}
                                                                                                      )
    _ <- parserTrace "/Properties" ()
    return ps
{-# LINE 689 "org/parser.org" #-}
instance Parse SExpr where
  parse = (
{-# LINE 699 "org/parser.org" #-}
           SExpr <$> ( P.try (P.char '(')
                        *> spaces
                        *> (P.sepEndBy (parse :: Parser SExpr) spaces1 <* P.char ')')
                      )
{-# LINE 690 "org/parser.org" #-}
                       )
          <|> (
{-# LINE 709 "org/parser.org" #-}
               P.try (P.string "nil" *> P.lookAhead P.space *> return (SExpr []))
               <|>
               P.try (P.string "no" *> P.lookAhead P.space *> return (SExpr []))
               <|>
               P.try (P.string "yes" *> P.lookAhead P.space *> return (Atom "t"))
{-# LINE 691 "org/parser.org" #-}
                                                                                 )
          <|> (
{-# LINE 721 "org/parser.org" #-}
               Atom <$> (enclosed '"' <|> (char '\'' *> symbol) <|> symbol)
{-# LINE 692 "org/parser.org" #-}
                                                                           )
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
{-# LINE 99 "ccweb.org" #-}
initialParserState :: ParserState
initialParserState = ParserState
  { sectionCounter = 0
  , propertyStack  = Stack []
  , parserLogLevel = Debug
  , evalContext    = Map.empty
  }
{-# LINE 110 "ccweb.org" #-}
parserDebug :: (Pretty a, Pretty b) => a -> b -> Parser b
parserDebug l s = do
  lvl <- parserLogLevel <$> P.getState
  pos <- P.getPosition
  let label = PP.hcat [pretty pos, PP.colon, pretty red, pretty l, pretty reset, PP.colon]
  if lvl >= Trace
  then trace (PP.render (PP.hang label 4 (pretty s))) $ return s
  else return s

{-# LINE 119 "ccweb.org" #-}
parserTrace :: (Pretty a, Pretty b) => a -> b -> Parser b
parserTrace l s = do
  (OrgLines ls) <- P.stateInput <$> P.getParserState
  u <- P.getState
  let user = pretty "user state" PP.<> PP.colon PP.<> pretty u
      input = pretty "looking at" PP.<> PP.colon PP.<> pretty (take 3 ls)
  _ <- parserDebug l (PP.fcat [pretty s, user, input])
  return s

{-# LINE 128 "ccweb.org" #-}
red :: String
red = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red]

{-# LINE 131 "ccweb.org" #-}
reset :: String
reset = ANSI.setSGRCode [ANSI.Reset]
{-# LINE 149 "ccweb.org" #-}
instance IsList OrgLines where
  type Item OrgLines = OrgLine
  fromList xs = OrgLines xs
  toList (OrgLines xs) = xs
{-# LINE 159 "ccweb.org" #-}
line :: Parser a -> Parser a
line p = p <* endOfLine
{-# LINE 175 "ccweb.org" #-}
instance Parse CodeElement where
  parse = do
    pos <- P.getPosition
    either id (Literal pos) <$> eitherMany1 (P.try $ 
{-# LINE 183 "ccweb.org" #-}
                                                     do
                                                       _ <- P.try $ P.string "<<"
                                                       p <- P.getPosition
                                                       SectionReference p <$> textTill (P.string ">>")
{-# LINE 178 "ccweb.org" #-}
                                                                                                      ) anyChar
{-# LINE 191 "ccweb.org" #-}
instance Parse CodeLine where
  parse = line (CodeLine <$> (P.many parse :: Parser [CodeElement]))
{-# LINE 199 "ccweb.org" #-}
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
{-# LINE 214 "ccweb.org" #-}
headlineLevel :: Headline -> Int
headlineLevel (Headline x _) = x
headlineLevel EmptyHeadline = error $ "no level for empty headline"
{-# LINE 223 "ccweb.org" #-}
instance Parse SourceBlock where
  parse = do
    _ <- parserTrace "SourceBlock" ()
    n  <- P.optionMaybe (
{-# LINE 255 "ccweb.org" #-}
                         P.try (spaces *> P.string "#+NAME:") *> (trim <$> parse)
                         >>= parserTrace "SourceBlock:/Name"
                         :: Parser Text
{-# LINE 226 "ccweb.org" #-}
                                       )
    i  <- P.try (spaces <* (P.string "#+BEGIN_SRC" *> spaces1))
    _ <- parserTrace "SourceBlock:Language" ()
    l  <- symbol <* spaces
    _ <- parserTrace "SourceBlock:Properties" ()
    ps <- 
{-# LINE 673 "org/parser.org" #-}
          line (Map.fromList
                <$> P.sepEndBy
                 (liftM2 (,)
                   (symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces)))
                   (parse :: Parser SExpr))
                 spaces1)
{-# LINE 232 "ccweb.org" #-}
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
{-# LINE 264 "ccweb.org" #-}
skipEmptyLines :: Parser ()
skipEmptyLines = do
  _ <- parserTrace "SkipEmptyLines" ()
  P.skipMany $ P.try (spaces *> endOfLine)
{-# LINE 276 "ccweb.org" #-}
enclosed :: Char -> Parser String
enclosed d = P.try (P.char d) *> P.manyTill anyChar (P.char d)
{-# LINE 284 "ccweb.org" #-}
instance Parse Section where
  parse = do
    _ <- parserTrace "Section" ()

{-# LINE 288 "ccweb.org" #-}
    skipEmptyLines
    _ <- parserTrace "Section:Maybe Headline" ()
    h <- fromMaybe EmptyHeadline <$> P.optionMaybe parse :: Parser Headline

{-# LINE 292 "ccweb.org" #-}
    let emptyHead = case h of { EmptyHeadline -> True; _ -> False }

{-# LINE 294 "ccweb.org" #-}
    skipEmptyLines
    _ <- parserTrace "Section:Properties" ()
    ps <- if emptyHead
      then return Map.empty
      else parse <|> return Map.empty :: Parser Properties
    _ <- parserDebug "Section:/Properties" ps

{-# LINE 301 "ccweb.org" #-}
    when (not emptyHead) $ do
      stk' <- resize (1 + (headlineLevel h)) . propertyStack <$> P.getState
      P.updateState (\s -> s{ propertyStack = stk' })
      ps' <- Map.unionWith Map.union ps . top . propertyStack <$> P.getState
      P.updateState (\s -> s{ propertyStack = push ps' stk' })
      P.getState >>= void . parserDebug "Section:Parser properties" . propertyStack

{-# LINE 308 "ccweb.org" #-}
    skipEmptyLines
    _ <- parserTrace "Section:Maybe [Text]" ()
    tss <- P.optionMaybe . P.many1 $ (P.many1 (P.try parse) <* skipEmptyLines)
          :: Parser (Maybe [[Text]])
    let ts = intercalate [Text [Plain []]] <$> tss
    _ <- parserTrace "Section:/Maybe [Text]" tss

{-# LINE 315 "ccweb.org" #-}
    skipEmptyLines
    _ <- parserTrace "Section:Maybe SourceBlock" ()
    c <- case (ts, h) of
          (Nothing, EmptyHeadline) -> Just <$> (parse :: Parser SourceBlock)
          (_, _) -> P.optionMaybe $ parse :: Parser (Maybe SourceBlock)

{-# LINE 321 "ccweb.org" #-}
    skipEmptyLines

{-# LINE 323 "ccweb.org" #-}
    _ <- parserTrace "/Section" ()

{-# LINE 325 "ccweb.org" #-}
    succeedWhen ((not emptyHead) || isJust ts || isJust c)

{-# LINE 327 "ccweb.org" #-}
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
{-# LINE 345 "ccweb.org" #-}
instance Parse Keyword where
  parse = propertyKeyword <|> titleKeyword <|> authorKeyword <|> otherKeyword
    where
      authorKeyword = do
        keywordStart "#+AUTHOR:"
        AuthorKeyword <$> parse
      propertyKeyword = do
        PropertyKeyword <$> 
{-# LINE 646 "org/parser.org" #-}
                            do
                              _ <- P.try (P.string "#+PROPERTY:") *> spaces
                              liftM2 Map.singleton
                                (symbol <* spaces1)
                                (
{-# LINE 673 "org/parser.org" #-}
                                 line (Map.fromList
                                       <$> P.sepEndBy
                                        (liftM2 (,)
                                          (symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces)))
                                          (parse :: Parser SExpr))
                                        spaces1)
{-# LINE 650 "org/parser.org" #-}
                                                )
{-# LINE 353 "ccweb.org" #-}
      titleKeyword = do
        keywordStart "#+TITLE:"
        TitleKeyword <$> parse
      otherKeyword = do
        _ <- P.try $ P.string "#+"
        t <- P.many anyChar
        _ <- endOfLine
        return $ OtherKeyword t
      keywordStart h = void $ P.try (P.string h *> spaces)
{-# LINE 366 "ccweb.org" #-}
headerProperties :: [Keyword] -> Properties
headerProperties = foldl acc mempty where
    acc a (PropertyKeyword p) = Map.union p a
    acc a _ = a
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
{-# LINE 126 "org/parser.org" #-}
fromEither :: Pretty a => Either a b -> b
fromEither = either (error . PP.render . pretty) id
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
{-# LINE 460 "org/doc.org" #-}
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
{-# LINE 266 "org/weave.org" #-}
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

{-# LINE 116 "org/weave.org" #-}
instance Weave String where
  weave = concatMapM (
    \case
      '&' -> return "{\\AM}"
      '\\' -> return "{\\BS}"
      '{' -> return "{\\LB}"
      '}' -> return "{\\RB}"
      '~' -> return "{\\TL}"
      '_' -> return "{\\UL}"
      '^' -> return "{\\CF}"
      '#' -> return "{\\#}"
      '$' -> return "{\\$}"
      '%' -> return "{\\%}"
      '§' -> return "{\\S}"
      c -> return [c]
    )
{-# LINE 136 "org/weave.org" #-}
instance Weave TextElement where
  weave = \case
    (Plain s) -> weave s
    (Bold s) -> between "{\\bf " "}" <$> weave s
    (Italics s) -> between "{\\it " "\\/}" <$> weave s
    (InlineCode s) -> between "\\hbox{\\tentex " "}" <$> weave s
    (Verbatim s) -> between "\\hbox{\\tentex " "}" <$> weave s
    (StrikeThrough _) -> error $ "not implemented: StrikeThrough"
    (InlineMath s) -> return $ between "$" "$" s
    (DisplayMath s) -> return $ between "$$" "$$" s
    (HyperLink a d) -> (\t -> concat ["\\pdfURL{", t, "}{" , a, "}"]) <$> weave d
    where between before after s = before <> s <> after

{-# LINE 149 "org/weave.org" #-}
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f as = concat <$> mapM f as
{-# LINE 155 "org/weave.org" #-}
instance Weave Text where
  weave (Text ts) = concat <$> mapM weave ts
{-# LINE 164 "org/weave.org" #-}
instance Weave Section where
  weave s = do
    header <- (
{-# LINE 174 "org/weave.org" #-}
               do
                 concat <$>
                   case sectionHeadline s of
                     EmptyHeadline ->
                       return [ "\\M{", show (sectionNumber s), "}"]
                     Headline d t -> do
                       title <- weave t :: Weaver String
                       return [ "\\N{", show d, "}"
                              , "{", show (sectionNumber s), "}"
                              , title, "."
                              ]
{-# LINE 166 "org/weave.org" #-}
                               ) :: Weaver String
    text <- (
{-# LINE 189 "org/weave.org" #-}
             mapM weave (documentation s)
{-# LINE 167 "org/weave.org" #-}
                                         ) :: Weaver [String]
    code <- (
{-# LINE 194 "org/weave.org" #-}
             case sectionSourceBlock s of
               Nothing -> return []
               Just b -> do
                 part <- docPartition <$> get
                 let id = sourceBlockId b
                     sections = (Map.!) part <$> id
                     firstNo = sectionNumber . head <$> sections
                     secno = sectionNumber s
                 n <- (
{-# LINE 234 "org/weave.org" #-}
                       \ref -> case sourceBlockId (fromJust $ sectionSourceBlock ref) of
                                Nothing -> return "\\X:$\\bullet$\\X"
                                Just id -> weave id
{-# LINE 202 "org/weave.org" #-}
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
{-# LINE 168 "org/weave.org" #-}
                                                                  ) :: Weaver String
    return $ unlines (header : text) ++ code ++ "\\fi\n"
{-# LINE 217 "org/weave.org" #-}
instance Weave CodeElement where
  weave (Literal _ s) = do
    s' <- weave s
    return $ "\\hbox{\\tentex "
      ++ map (\c -> if c == ' ' then '~' else c) s'
      ++ "}"
  weave (SectionReference _ r) = weave (NamedBlock r)
{-# LINE 228 "org/weave.org" #-}
instance Weave CodeLine where
  weave (CodeLine es) = concatMapM weave es
{-# LINE 241 "org/weave.org" #-}
instance Weave SourceBlockId where
  weave id = do
    firstNo <- sectionNumber . head . (flip (Map.!) id) . docPartition <$> get
    tangledName <- case id of
      FileBlock file -> ("$\\Rightarrow\\,$" ++) <$> weave (Verbatim file)
      NamedBlock name -> weave name
    return . concat $ ["\\X", show firstNo, ":", tangledName, "\\X"]
{-# LINE 252 "org/weave.org" #-}
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
