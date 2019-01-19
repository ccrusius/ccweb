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
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Debug.Trace (trace)
import qualified Data.Map as Map
import GHC.Exts (IsList(..), groupWith, sortWith)
import Network.HostName (getHostName)
{-# LINE 74 "org/parser.org" #-}
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
{-# LINE 397 "org/tangle.org" #-}
import qualified System.Directory as D
import qualified System.Environment as Env
import qualified System.FilePath as F
import qualified System.Posix.Files as F
{-# LINE 33 "org/scaffold.org" #-}
import qualified Options.Applicative as O
{-# LINE 180 "org/scaffold.org" #-}
import qualified Text.PrettyPrint as PP
{-# LINE 134 "ccweb.org" #-}
import qualified System.Console.ANSI as ANSI
{-# LINE 512 "ccweb.org" #-}
import qualified System.Info as Sys
import qualified Control.Exception as E
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
{-# LINE 194 "org/doc.org" #-}
  eval s (SExpr [Atom "identity", expr]) = eval s expr
{-# LINE 202 "org/doc.org" #-}
  eval _ (Atom ('#':'o':x:y:z:[])) =
    IntAtom $ 8 * ((8 * oct x) + oct y) + oct z
    where oct c = ord c - ord '0'
{-# LINE 211 "org/doc.org" #-}
  eval s (SExpr [Atom "eq", e1, e2]) = BoolAtom $ (eval s e1) == (eval s e2)
  
{-# LINE 213 "org/doc.org" #-}
  eval s (SExpr [Atom "string-prefix-p", prefix, expr]) =
    case (eval s prefix, eval s expr) of
      (Atom s', Atom str) -> BoolAtom $ isPrefixOf s' str
      (e1, e2) -> SExpr [Atom "string-prefix-p", e1, e2]
  
{-# LINE 218 "org/doc.org" #-}
  eval s (SExpr [Atom "string-suffix-p", suffix, expr]) =
    case (eval s suffix, eval s expr) of
      (Atom s', Atom str) -> BoolAtom $ isSuffixOf s' str
      (e1, e2) -> SExpr [Atom "string-suffix-p", e1, e2]
{-# LINE 228 "org/doc.org" #-}
  eval s (SExpr [Atom "when", expr, result]) =
    case eval s expr of
      BoolAtom True -> eval s result
      expr' -> expr'
  
{-# LINE 233 "org/doc.org" #-}
  eval s (SExpr [Atom "unless", expr, result]) =
    case eval s expr of
      BoolAtom False -> eval s result
      BoolAtom True  -> BoolAtom False
      expr' -> expr'
{-# LINE 245 "org/doc.org" #-}
  eval s expr = Map.findWithDefault expr expr (evalContext s)
{-# LINE 258 "org/doc.org" #-}
class HeaderArgs a where
  headerArgs :: a -> Property
  headerArg :: String -> a -> Maybe SExpr
  headerArg k a = Map.lookup k $ headerArgs a

{-# LINE 263 "org/doc.org" #-}
instance HeaderArgs Properties where
  headerArgs = Map.findWithDefault Map.empty "header-args"

{-# LINE 266 "org/doc.org" #-}
instance HeaderArgs Section where
  headerArgs = headerArgs . sectionDerivedProperties
{-# LINE 282 "org/doc.org" #-}
data SourceBlock = SourceBlock
  { blockName :: Maybe Text
  , blockLanguage :: String
  , blockLines :: [CodeLine]
  , blockLocation :: P.SourcePos
  , blockProperties :: Property
  , blockDerivedProperties :: Property
  }
{-# LINE 311 "org/doc.org" #-}
instance HeaderArgs SourceBlock where
  headerArgs = blockDerivedProperties
{-# LINE 322 "org/doc.org" #-}
data SourceBlockId = FileBlock FilePath | NamedBlock Text deriving Eq

{-# LINE 324 "org/doc.org" #-}
instance Ord SourceBlockId where
  (<=) (FileBlock p1) (FileBlock p2) = p1 <= p2
  (<=) (NamedBlock p1) (NamedBlock p2) = p1 <= p2
  (<=) (FileBlock _) (NamedBlock _) = False
  (<=) _ _ = True
{-# LINE 337 "org/doc.org" #-}
sourceBlockId :: SourceBlock -> Maybe SourceBlockId
sourceBlockId SourceBlock{ blockName = (Just name) } = Just $ NamedBlock name
sourceBlockId block = case headerArg ":tangle" block of
    Nothing -> Nothing
    Just (Atom f) -> Just $ FileBlock f
    Just (BoolAtom _) -> Nothing
    Just e -> error $ "unsupported tangle destination: " ++ show e
{-# LINE 351 "org/doc.org" #-}
newtype CodeLine = CodeLine [CodeElement]
data CodeElement = Literal P.SourcePos String
                 | SectionReference P.SourcePos Text
{-# LINE 380 "org/doc.org" #-}
data Text = Text [TextElement] deriving (Eq, Ord, Show)

{-# LINE 382 "org/doc.org" #-}
data TextElement =
  Bold Text
  | HyperLink String Text
  | InlineCode String
  | Italics Text
  | Plain String
  | StrikeThrough String
  | TeXMath String
  | Verbatim String
  deriving (Eq, Ord, Show)
{-# LINE 423 "org/doc.org" #-}
trim :: Text -> Text
trim (Text (Plain []:ys))       = trim $ Text ys
trim (Text (Plain (' ':xs):ys)) = trim $ Text (Plain xs:ys)
trim  t                         = t
{-# LINE 433 "org/doc.org" #-}
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
{-# LINE 296 "org/doc.org" #-}
instance Pretty SourceBlock where
  pretty = prettyStruct "SourceBlock"
             [ ("name", pretty . blockName)
             , ("language", pretty . blockLanguage)
             , ("properties", pretty . blockProperties)
             , ("derived properties", pretty . blockDerivedProperties)
             , ("location", pretty . blockLocation)
             , ("lines", pretty . blockLines)
             ]
{-# LINE 362 "org/doc.org" #-}
instance Pretty CodeLine where
  pretty (CodeLine xs) = pretty xs

{-# LINE 365 "org/doc.org" #-}
instance Pretty CodeElement where
  pretty (Literal p s) =
    PP.parens $ PP.hcat [pretty p, PP.colon, pretty s]
  pretty (SectionReference p t) =
    PP.hcat [pretty p, PP.colon, PP.char '〈', pretty t, PP.char '〉']
{-# LINE 400 "org/doc.org" #-}
instance Pretty TextElement where
  pretty = \case
    (Bold a)          -> pretty' "bold" a
    (InlineCode a)    -> pretty' "code" a
    (Italics a)       -> pretty' "italic" a
    (Plain a)         -> pretty a
    (StrikeThrough a) -> pretty' "strike" a
    (TeXMath a)       -> pretty' "TeX" a
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
    PP.hcat [pretty p, PP.colon, PP.text (takeWhile (/= '\n') l)]
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
{-# LINE 164 "ccweb.org" #-}
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
{-# LINE 262 "org/parser.org" #-}
beginningOfLine :: Parser ()
beginningOfLine = do
  c <- P.sourceColumn <$> P.getPosition
  --_ <- parserTrace ("BOL:" ++ show c ++ ":" ++ show (c == 1))
  succeedWhen (c == 1)
  --_ <- parserTrace ("/BOL:" ++ show c ++ ":" ++ show (c == 1))
{-# LINE 272 "org/parser.org" #-}
succeedWhen :: Bool -> Parser ()
succeedWhen True = return ()
succeedWhen False = void $ satisfy (\_ -> False)

{-# LINE 276 "org/parser.org" #-}
failWhen :: Bool -> Parser ()
failWhen = succeedWhen . not
{-# LINE 289 "org/parser.org" #-}
manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p = P.manyTill p . P.lookAhead

{-# LINE 292 "org/parser.org" #-}
many1Till :: Parser a -> Parser end -> Parser [a]
many1Till p = liftA2 (:) p . manyTill p
{-# LINE 298 "org/parser.org" #-}
eitherMany1 :: Parser end -> Parser a -> Parser (Either end [a])
eitherMany1 end p =
  (Left <$> P.try end) <|>
  (Right <$> many1Till p (void endOfLine <|> void end))
{-# LINE 403 "org/parser.org" #-}
upx :: Maybe Char -> String -> [TextElement] -> [TextElement]
upx Nothing  []    elts = elts
upx Nothing  plain elts = Plain (reverse plain) : elts
upx (Just c) plain elts = Plain (reverse $ c : plain) : elts
{-# LINE 411 "org/parser.org" #-}
textAcc :: (String, [TextElement]) -> Parser (String, [TextElement])
textAcc (plain, text) =
{-# LINE 426 "org/parser.org" #-}
  (P.eof *> parserTrace "Text:/EOF" ([], upx Nothing plain text))
  <|> (endOfLine *> parserTrace "Text:/EOL" ([], upx Nothing plain text))
{-# LINE 414 "org/parser.org" #-}
  <|> (
{-# LINE 432 "org/parser.org" #-}
       do
         beginningOfLine
         P.lookAhead . P.choice $ map P.try
           [ void (spaces *> P.string "#+NAME")
           , void (spaces *> P.string "#+BEGIN_SRC")
           , void (parse :: Parser Headline)
           ]
         parserTrace "Text:/Break" ([], upx Nothing plain text)
{-# LINE 414 "org/parser.org" #-}
                                                               )
  <|> (
{-# LINE 444 "org/parser.org" #-}
       P.try $ do
         url <- P.string "[[" *> many1Till anyChar (P.string "][" <|> P.string "]]")
         txt <- P.string "][" <|> P.string "]]" >>= \case
           "]]" -> return $ Text [Verbatim url]
           _    -> textTill (P.string "]]")
         let hyp = HyperLink url txt
         _ <- parserTrace "Text:/HyperLink" hyp
         textAcc $ ([], hyp : upx Nothing plain text)
{-# LINE 415 "org/parser.org" #-}
                                                     )
  <|>
  (do
      (c, markup) <- (
{-# LINE 391 "org/parser.org" #-}
                      P.choice $ map (P.try . (
{-# LINE 365 "org/parser.org" #-}
                                               \marker ->
                                                 do
                                                   pre <- 
{-# LINE 342 "org/parser.org" #-}
                                                          P.try $
                                                            (
{-# LINE 324 "org/parser.org" #-}
                                                             (beginningOfLine *> return Nothing)
                                                             <|> (Just <$> satisfy (\c -> isSpace c || elem c "({'\""))
                                                             :: Parser (Maybe Char)
{-# LINE 343 "org/parser.org" #-}
                                                                                   )
                                                            <* P.char marker
                                                            <* P.lookAhead (
{-# LINE 313 "org/parser.org" #-}
                                                                            satisfy (\c -> not (isSpace c) && notElem c ",'\"")
                                                                            :: Parser Char
{-# LINE 345 "org/parser.org" #-}
                                                                                          )
                                                          :: Parser (Maybe Char)
{-# LINE 368 "org/parser.org" #-}
                                                   p <- P.getPosition
                                                   body <- manyTill anyChar (
{-# LINE 356 "org/parser.org" #-}
                                                                             P.try $
                                                                               (
{-# LINE 313 "org/parser.org" #-}
                                                                                satisfy (\c -> not (isSpace c) && notElem c ",'\"")
                                                                                :: Parser Char
{-# LINE 357 "org/parser.org" #-}
                                                                                              )
                                                                               <* P.char marker
                                                                               <* P.lookAhead (
{-# LINE 334 "org/parser.org" #-}
                                                                                               (P.eof *> return Nothing)
                                                                                               <|> Just <$> satisfy (\c -> isSpace c || elem c "-.,:!?)}'\"")
                                                                                               <|> Just <$> endOfLine
                                                                                               :: Parser (Maybe Char)
{-# LINE 359 "org/parser.org" #-}
                                                                                                                     )
                                                                             :: Parser Char
{-# LINE 369 "org/parser.org" #-}
                                                                                           )
                                                   border <- anyChar
                                                   _ <- P.char marker
                                                   let body' = body ++ [border]
                                                       body'' = fromEither $ P.runParser
                                                                  (parse :: Parser Text)
                                                                  initialParserState
                                                                  (P.sourceName p)
                                                                  (fromList [OrgLine p body'] :: OrgLines)
                                                   return . (pre,) $ case marker of
                                                     '*' -> Bold body''
                                                     '~' -> InlineCode body'
                                                     '/' -> Italics body''
                                                     '+' -> StrikeThrough body'
                                                     '$' -> TeXMath body'
                                                     '=' -> Verbatim body'
                                                     e   -> error $ "unknown markup marker '" ++ show e ++ "'"
                                                 :: Parser (Maybe Char, TextElement)
{-# LINE 391 "org/parser.org" #-}
                                                                                    )) "*~/+$="
                      :: Parser (Maybe Char, TextElement)
{-# LINE 418 "org/parser.org" #-}
                                                         ) >>= parserTrace "Text:/Markup"
      textAcc $ ([], markup : upx c plain text)
  )
  <|> (anyChar >>= textAcc . (,text) . (:plain))
{-# LINE 456 "org/parser.org" #-}
textTill :: Parser end -> Parser Text
textTill end = do
  _ <- parserTrace "TextTill" ()
  p <- P.getPosition
  t <- P.manyTill anyChar end
  parserTrace "/TextTill" . fromEither $ P.runParser
    (parse :: Parser Text)
    initialParserState
    (P.sourceName p)
    (fromList [OrgLine p t] :: OrgLines)
{-# LINE 470 "org/parser.org" #-}
instance Parse Text where
  parse = do
    _ <- parserTrace "Text" ()
    ret <- reverse . snd <$> textAcc ([], [])
    failWhen (null ret) P.<?> "empty text"
    _ <- parserDebug "/Text" ret

{-# LINE 477 "org/parser.org" #-}
    return $ Text ret
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
  , evalContext    = Map.empty
  }
{-# LINE 107 "ccweb.org" #-}
parserDebug :: (Pretty a, Pretty b) => a -> b -> Parser b
parserDebug l s = do
  lvl <- parserLogLevel <$> P.getState
  pos <- P.getPosition
  let label = PP.hcat [pretty pos, PP.colon, pretty red, pretty l, pretty reset, PP.colon]
  if lvl >= Trace
  then trace (PP.render (PP.hang label 4 (pretty s))) $ return s
  else return s

{-# LINE 116 "ccweb.org" #-}
parserTrace :: (Pretty a, Pretty b) => a -> b -> Parser b
parserTrace l s = do
  (OrgLines ls) <- P.stateInput <$> P.getParserState
  u <- P.getState
  let user = pretty "user state" PP.<> PP.colon PP.<> pretty u
      input = pretty "looking at" PP.<> PP.colon PP.<> pretty (take 3 ls)
  _ <- parserDebug l (PP.fcat [pretty s, user, input])
  return s

{-# LINE 125 "ccweb.org" #-}
red :: String
red = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red]

{-# LINE 128 "ccweb.org" #-}
reset :: String
reset = ANSI.setSGRCode [ANSI.Reset]
{-# LINE 146 "ccweb.org" #-}
instance IsList OrgLines where
  type Item OrgLines = OrgLine
  fromList xs = OrgLines xs
  toList (OrgLines xs) = xs
{-# LINE 156 "ccweb.org" #-}
line :: Parser a -> Parser a
line p = p <* endOfLine
{-# LINE 172 "ccweb.org" #-}
instance Parse CodeElement where
  parse = do
    pos <- P.getPosition
    either id (Literal pos) <$> eitherMany1 (P.try $ 
{-# LINE 180 "ccweb.org" #-}
                                                     do
                                                       _ <- P.try $ P.string "<<"
                                                       p <- P.getPosition
                                                       SectionReference p <$> textTill (P.string ">>")
{-# LINE 175 "ccweb.org" #-}
                                                                                                      ) anyChar
{-# LINE 188 "ccweb.org" #-}
instance Parse CodeLine where
  parse = line (CodeLine <$> (P.many parse :: Parser [CodeElement]))
{-# LINE 196 "ccweb.org" #-}
instance Parse SExpr where
  parse =
    (
{-# LINE 207 "ccweb.org" #-}
     SExpr <$> ( P.try (P.char '(')
                  *> spaces
                  *> (P.sepEndBy parse spaces1 <* P.char ')')
                )
{-# LINE 198 "ccweb.org" #-}
                 )
    <|>
    P.try (
{-# LINE 215 "ccweb.org" #-}
           BoolAtom <$> (
             (P.string "no" *> P.lookAhead P.space *> return False)
             <|>
             (P.string "yes" *> P.lookAhead P.space *> return True)
             )
{-# LINE 200 "ccweb.org" #-}
              )
    <|>
    (
{-# LINE 224 "ccweb.org" #-}
     Atom <$> (enclosed '"' <|> symbol)
{-# LINE 202 "ccweb.org" #-}
                                       )
{-# LINE 231 "ccweb.org" #-}
instance Parse Properties where
  parse = do
    _ <- parserTrace "Properties" ()
    ps <- (
{-# LINE 241 "ccweb.org" #-}
           do
             _ <- P.try (P.string "#+PROPERTY:") *> spaces
             p <- symbol <* spaces1
             kvs <- 
{-# LINE 250 "ccweb.org" #-}
                    line (Map.fromList
                          <$> P.sepEndBy
                           (
{-# LINE 258 "ccweb.org" #-}
                            do
                              k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                              v <- parse :: Parser SExpr
                              return (k, v)
{-# LINE 252 "ccweb.org" #-}
                                           )
                           spaces1)
{-# LINE 245 "ccweb.org" #-}
             return $ Map.singleton p kvs
{-# LINE 234 "ccweb.org" #-}
                                         ) <|> (
{-# LINE 266 "ccweb.org" #-}
                                                do
                                                  _ <- P.try (spaces *> P.string ":PROPERTIES:" *> spaces *> endOfLine)
                                                  Map.fromList <$> P.manyTill
                                                    (
{-# LINE 275 "ccweb.org" #-}
                                                     do
                                                       p <- spaces *> (enclosed ':' <* spaces)
                                                       kvs <- 
{-# LINE 250 "ccweb.org" #-}
                                                              line (Map.fromList
                                                                    <$> P.sepEndBy
                                                                     (
{-# LINE 258 "ccweb.org" #-}
                                                                      do
                                                                        k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                                                                        v <- parse :: Parser SExpr
                                                                        return (k, v)
{-# LINE 252 "ccweb.org" #-}
                                                                                     )
                                                                     spaces1)
{-# LINE 278 "ccweb.org" #-}
                                                       return (p, kvs)
{-# LINE 269 "ccweb.org" #-}
                                                                      )
                                                    (P.try (spaces *> P.string ":END:" *> spaces *> endOfLine))
{-# LINE 234 "ccweb.org" #-}
                                                                                                               )
    _ <- parserTrace "/Properties" ()
    return ps
{-# LINE 285 "ccweb.org" #-}
instance Parse Headline where
  parse = do
    _ <- parserTrace "Headline" ()
    s <- P.try (char '*') *> P.many (char '*')
    _ <- spaces1
    t <- parse :: Parser Text
    _ <- parserTrace "/Headline" ()
    return $ Headline (length s) t
{-# LINE 297 "ccweb.org" #-}
headlineLevel :: Headline -> Int
headlineLevel (Headline x _) = x
{-# LINE 305 "ccweb.org" #-}
instance Parse SourceBlock where
  parse = do
    _ <- parserTrace "SourceBlock" ()
    n  <- P.optionMaybe (
{-# LINE 337 "ccweb.org" #-}
                         P.try (spaces *> P.string "#+NAME:") *> (trim <$> parse)
                         >>= parserTrace "SourceBlock:/Name"
                         :: Parser Text
{-# LINE 308 "ccweb.org" #-}
                                       )
    i  <- P.try (spaces <* (P.string "#+BEGIN_SRC" *> spaces1))
    _ <- parserTrace "SourceBlock:Language" ()
    l  <- symbol <* spaces
    _ <- parserTrace "SourceBlock:Properties" ()
    ps <- 
{-# LINE 250 "ccweb.org" #-}
          line (Map.fromList
                <$> P.sepEndBy
                 (
{-# LINE 258 "ccweb.org" #-}
                  do
                    k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                    v <- parse :: Parser SExpr
                    return (k, v)
{-# LINE 252 "ccweb.org" #-}
                                 )
                 spaces1)
{-# LINE 314 "ccweb.org" #-}
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
{-# LINE 346 "ccweb.org" #-}
skipEmptyLines :: Parser ()
skipEmptyLines = do
  _ <- parserTrace "SkipEmptyLines" ()
  P.skipMany $ P.try (spaces *> endOfLine)
{-# LINE 358 "ccweb.org" #-}
enclosed :: Char -> Parser String
enclosed d = P.try (P.char d) *> P.manyTill anyChar (P.char d)
{-# LINE 366 "ccweb.org" #-}
instance Parse Section where
  parse = do
    _ <- parserTrace "Section" ()

{-# LINE 370 "ccweb.org" #-}
    skipEmptyLines
    _ <- parserTrace "Section:Maybe Headline" ()
    h <- P.optionMaybe parse :: Parser (Maybe Headline)

{-# LINE 374 "ccweb.org" #-}
    skipEmptyLines
    _ <- parserTrace "Section:Properties" ()
    ps <- case h of
      Nothing -> return Map.empty
      _ -> parse <|> return Map.empty :: Parser Properties
    _ <- parserDebug "Section:/Properties" ps

{-# LINE 381 "ccweb.org" #-}
    when (isJust h) $ do
      stk' <- resize (1 + (headlineLevel $ fromJust h)) . propertyStack <$> P.getState
      P.updateState (\s -> s{ propertyStack = stk' })
      ps' <- Map.unionWith Map.union ps . top . propertyStack <$> P.getState
      P.updateState (\s -> s{ propertyStack = push ps' stk' })
      P.getState >>= void . parserDebug "Section:Parser properties" . propertyStack

{-# LINE 388 "ccweb.org" #-}
    skipEmptyLines
    _ <- parserTrace "Section:Maybe [Text]" ()
    tss <- P.optionMaybe . P.many1 $ (P.many1 (P.try parse) <* skipEmptyLines)
          :: Parser (Maybe [[Text]])
    let ts = intercalate [Text [Plain []]] <$> tss
    _ <- parserTrace "Section:/Maybe [Text]" tss

{-# LINE 395 "ccweb.org" #-}
    skipEmptyLines
    _ <- parserTrace "Section:Maybe SourceBlock" ()
    c <- case (ts, h) of
          (Nothing, Nothing) -> Just <$> (parse :: Parser SourceBlock)
          (_, _) -> P.optionMaybe $ parse :: Parser (Maybe SourceBlock)

{-# LINE 401 "ccweb.org" #-}
    skipEmptyLines

{-# LINE 403 "ccweb.org" #-}
    _ <- parserTrace "/Section" ()

{-# LINE 405 "ccweb.org" #-}
    succeedWhen (isJust h || isJust ts || isJust c)

{-# LINE 407 "ccweb.org" #-}
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
{-# LINE 425 "ccweb.org" #-}
instance Parse Keyword where
  parse = propertyKeyword <|> titleKeyword <|> authorKeyword <|> otherKeyword
    where
      authorKeyword = do
        keywordStart "#+AUTHOR:"
        AuthorKeyword <$> parse
      propertyKeyword = do
        PropertyKeyword <$> 
{-# LINE 241 "ccweb.org" #-}
                            do
                              _ <- P.try (P.string "#+PROPERTY:") *> spaces
                              p <- symbol <* spaces1
                              kvs <- 
{-# LINE 250 "ccweb.org" #-}
                                     line (Map.fromList
                                           <$> P.sepEndBy
                                            (
{-# LINE 258 "ccweb.org" #-}
                                             do
                                               k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                                               v <- parse :: Parser SExpr
                                               return (k, v)
{-# LINE 252 "ccweb.org" #-}
                                                            )
                                            spaces1)
{-# LINE 245 "ccweb.org" #-}
                              return $ Map.singleton p kvs
{-# LINE 433 "ccweb.org" #-}
      titleKeyword = do
        keywordStart "#+TITLE:"
        TitleKeyword <$> parse
      otherKeyword = do
        _ <- P.try $ P.string "#+"
        t <- P.many anyChar
        _ <- endOfLine
        return $ OtherKeyword t
      keywordStart h = void $ P.try (P.string h *> spaces)
{-# LINE 446 "ccweb.org" #-}
headerProperties :: [Keyword] -> Properties
headerProperties = foldl acc mempty where
    acc a (PropertyKeyword p) = Map.union p a
    acc a _ = a
{-# LINE 456 "ccweb.org" #-}
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
{-# LINE 471 "ccweb.org" #-}
readOrgFile :: LogLevel -> FilePath -> IO (OrgLines, Document)
readOrgFile lvl fp = do
  ingested <- fromList <$> ingest fp :: IO OrgLines
{-# LINE 485 "ccweb.org" #-}
  let styp = ( Atom "system-type"
             , Atom $ case Sys.os of
                        "linux" -> "gnu/linux"
                        "mingw32" -> "windows-nt"
                        o -> o
             )
      atomize k = fmap ((Atom k,) . Atom)
  hnam <- (SExpr [Atom "system-name"],) . Atom <$> getHostName
  pfam <- atomize "dmi/product-family"  <$> (
{-# LINE 502 "ccweb.org" #-}
                                             \p -> either (\_ -> Nothing) (Just . reverse . dropWhile isSpace . reverse)
                                                  <$> (E.try (readFile $ "/sys/devices/virtual/dmi/id" F.</> p)
                                                       :: IO (Either E.IOException String))
{-# LINE 493 "ccweb.org" #-}
                                                                                           ) "product_family"
  pnam <- atomize "dmi/product-name"    <$> (
{-# LINE 502 "ccweb.org" #-}
                                             \p -> either (\_ -> Nothing) (Just . reverse . dropWhile isSpace . reverse)
                                                  <$> (E.try (readFile $ "/sys/devices/virtual/dmi/id" F.</> p)
                                                       :: IO (Either E.IOException String))
{-# LINE 494 "ccweb.org" #-}
                                                                                           ) "product_name"
  pver <- atomize "dmi/product-version" <$> (
{-# LINE 502 "ccweb.org" #-}
                                             \p -> either (\_ -> Nothing) (Just . reverse . dropWhile isSpace . reverse)
                                                  <$> (E.try (readFile $ "/sys/devices/virtual/dmi/id" F.</> p)
                                                       :: IO (Either E.IOException String))
{-# LINE 495 "ccweb.org" #-}
                                                                                           ) "product_version"
{-# LINE 495 "ccweb.org" #-}
  
{-# LINE 497 "ccweb.org" #-}
  let ctx = Map.fromList $ styp : hnam : catMaybes [pfam, pnam, pver]
{-# LINE 475 "ccweb.org" #-}
  let doc = fromEither $ P.runParser
        (parse :: Parser Document)
        initialParserState{ evalContext = ctx, parserLogLevel = lvl }
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
{-# LINE 126 "org/parser.org" #-}
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
{-# LINE 438 "org/doc.org" #-}
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
tangleCodeLine (CodeLine []) = (:[]) . CodeText <$> getIndent
{-# LINE 231 "org/tangle.org" #-}
tangleCodeLine (CodeLine elements) =
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
{-# LINE 331 "org/tangle.org" #-}
                                                        do
                                                          bs <- 
{-# LINE 339 "org/tangle.org" #-}
                                                                filter (\b -> case (blockName b) of
                                                                               Nothing -> False
                                                                               Just t -> t == name)
                                                                  . mapMaybe sectionSourceBlock
                                                                  . sections
                                                                  . document
                                                                  <$> get
{-# LINE 332 "org/tangle.org" #-}
                                                                          :: Tangler [SourceBlock]
                                                          when (null bs) (error $ "source block not defined anywhere: " ++ show name)
                                                          concat <$> mapM tangleSourceBlock bs :: Tangler [Line]
{-# LINE 313 "org/tangle.org" #-}
                                            case breakCodeText refLines of
                                              (refPragmas, (CodeText first : rest)) -> do
                                                acc' <- case (refPragmas, accPragmas) of
                                                  ([], []) -> do
                                                    unindented <- 
{-# LINE 350 "org/tangle.org" #-}
                                                                  (\str -> do
                                                                     i <- getIndent
                                                                     return $ if isPrefixOf i str; then drop (length i) str; else str
                                                                  )
{-# LINE 317 "org/tangle.org" #-}
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
{-# LINE 361 "org/tangle.org" #-}
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
{-# LINE 374 "org/tangle.org" #-}
                                           ls
  logM (logLevel opts) Info $ "Writing one output file (" ++ fp ++ ")..."
  if dryRun opts
    then logM (logLevel opts) Info contents
    else do
      let dir = F.takeDirectory fp
      when (mkDir && dir /= ".") $ D.createDirectoryIfMissing True dir

{-# LINE 382 "org/tangle.org" #-}
      F.fileExist fp >>= (`when` F.removeLink fp)
      writeFile fp contents

{-# LINE 385 "org/tangle.org" #-}
      case headerArg ":tangle-mode" block of
        Nothing -> return ()
        Just (IntAtom mode) -> F.setFileMode fp $ fromIntegral mode
        Just e -> error $ "unsupported tangle-mode: " ++ show e
  where
    mkDir = case headerArg ":mkdirp" (head bs) of
              Just (BoolAtom x) -> x
              _ -> False
{-# LINE 5 "org/tangle.org" #-}
