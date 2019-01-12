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
{-# LINE 388 "org/tangle.org" #-}
import qualified System.Directory as D
import qualified System.Environment as Env
import qualified System.FilePath as F
import qualified System.Posix.Files as F
import qualified System.Posix.Types as F
{-# LINE 123 "ccweb.org" #-}
import qualified Text.PrettyPrint as PP
{-# LINE 224 "ccweb.org" #-}
import qualified Options.Applicative as O
{-# LINE 333 "ccweb.org" #-}
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
{-# LINE 818 "ccweb.org" #-}
import qualified System.Info as Sys
{-# LINE 5 "org/doc.org" #-}
data Document = Document
  { orgKeywords :: [Keyword]
  , orgSections :: [Section]
  }
{-# LINE 13 "org/doc.org" #-}
data Keyword = AuthorKeyword Text
             | PropertyKeyword Properties
             | TitleKeyword Text
             | OtherKeyword String
{-# LINE 21 "org/doc.org" #-}
data Section = Section
  { sectionNumber :: Int
  , sectionHeadline :: Maybe Headline
  , sectionProps :: Properties
  , sectionDerivedProperties :: Properties
  , sectionDocumentation :: [Text]
  , sectionSourceBlock :: Maybe SourceBlock
  }
{-# LINE 33 "org/doc.org" #-}
data Headline = Headline Int Text deriving Show
{-# LINE 38 "org/doc.org" #-}
data Text = Text [TextElement] deriving (Eq, Ord, Show)

{-# LINE 40 "org/doc.org" #-}
data TextElement =
  Bold String
  | InlineCode String
  | Italics String
  | Plain String
  | StrikeThrough String
  | Verbatim String
  deriving (Eq, Ord, Show)
{-# LINE 52 "org/doc.org" #-}
data SourceBlock = SourceBlock
  { blockName :: Maybe Text
  , blockLanguage :: String
  , blockProperties :: Property
  , blockDerivedProperties :: Property
  , blockLocation :: P.SourcePos
  , blockLines :: [CodeLine]
  }
{-# LINE 64 "org/doc.org" #-}
data CodeElement = Literal P.SourcePos String
                 | SectionReference P.SourcePos Text

{-# LINE 67 "org/doc.org" #-}
data CodeLine = CodeLine P.SourcePos [CodeElement]
{-# LINE 72 "org/doc.org" #-}
type Property = Map.Map String SExpression
type Properties = Map.Map String Property
{-# LINE 78 "org/doc.org" #-}
data SExpression =
  Atom String
  | Series [SExpression]
  | YesNo Bool
  deriving (Eq, Ord)
{-# LINE 87 "org/doc.org" #-}
class HeaderArgs a where
  headerArgs :: a -> Property
  headerArg :: String -> a -> Maybe SExpression
  headerArg k a = Map.lookup k $ headerArgs a

{-# LINE 92 "org/doc.org" #-}
instance HeaderArgs Properties where
  headerArgs = Map.findWithDefault Map.empty "header-args"

{-# LINE 95 "org/doc.org" #-}
instance HeaderArgs SourceBlock where
  headerArgs = blockDerivedProperties
{-# LINE 101 "org/doc.org" #-}
data SourceBlockId = FileBlock FilePath | NamedBlock Text deriving Eq

{-# LINE 103 "org/doc.org" #-}
instance Ord SourceBlockId where
  (<=) (FileBlock p1) (FileBlock p2) = p1 <= p2
  (<=) (NamedBlock p1) (NamedBlock p2) = p1 <= p2
  (<=) (FileBlock _) (NamedBlock _) = False
  (<=) _ _ = True

{-# LINE 109 "org/doc.org" #-}
sourceBlockId :: SourceBlock -> Maybe SourceBlockId
sourceBlockId SourceBlock{ blockName = (Just name) } = Just $ NamedBlock name
sourceBlockId block = case headerArg ":tangle" block of
    Nothing -> Nothing
    Just (Atom f) -> Just $ FileBlock f
    Just (YesNo _) -> Nothing
    Just e -> error $ "unsupported tangle destination: " ++ show e
{-# LINE 120 "org/doc.org" #-}
type DocumentPartition = [(SourceBlockId,[Section])]
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
{-# LINE 188 "ccweb.org" #-}
instance Pretty P.SourcePos where
  pretty p = pretty (P.sourceName p)
             PP.<> PP.colon PP.<> pretty (P.sourceLine p)
             PP.<> PP.colon PP.<> pretty (P.sourceColumn p)
{-# LINE 453 "ccweb.org" #-}
instance Pretty OrgLine where
  pretty (OrgLine p l) =
    pretty p
    PP.<> PP.colon
    PP.<> PP.text (takeWhile (/= '\NUL') l)
{-# LINE 503 "ccweb.org" #-}
instance Pretty OrgFile where
  pretty (OrgFile ls) = pretty ls
{-# LINE 617 "ccweb.org" #-}
instance Pretty TextElement where
  pretty (Bold a) = PP.text "Bold:" PP.<+> pretty a
  pretty (InlineCode a) = PP.text "InlineCode:" PP.<+> pretty a
  pretty (Italics a) = PP.text "Italics:" PP.<+> pretty a
  pretty (Plain a) = pretty a
  pretty (StrikeThrough a) = PP.text "StrikeThrough:" PP.<+> pretty a
  pretty (Verbatim a) = PP.text "Verbatim:" PP.<+> pretty a
{-# LINE 645 "ccweb.org" #-}
instance Pretty Text where
  pretty (Text xs) = PP.text "Text:" PP.<+> PP.hcat (map pretty xs)
{-# LINE 666 "ccweb.org" #-}
instance Pretty CodeElement where
  pretty (Literal p s) = PP.parens $ pretty p PP.<> PP.colon PP.<> pretty s
  pretty (SectionReference p t) = pretty p PP.<> PP.colon PP.<> PP.char '«' PP.<> pretty t PP.<> PP.char '»'
{-# LINE 707 "ccweb.org" #-}
instance Pretty CodeLine where
  pretty (CodeLine p xs) = pretty p PP.<> PP.colon PP.<> pretty xs
{-# LINE 727 "ccweb.org" #-}
instance Show SExpression where
  show (Atom x) = x
  show (Series xs) = "(" ++ intercalate " " (map show xs) ++ ")"
  show (YesNo x) = show x

{-# LINE 732 "ccweb.org" #-}
instance Pretty SExpression where
  pretty (Atom x) = PP.text x
  pretty (Series xs) = PP.parens $ PP.fsep (map pretty xs)
  pretty (YesNo x) = PP.text (show x)
{-# LINE 893 "ccweb.org" #-}
instance Pretty Headline where
  pretty (Headline l t) =
    PP.text "Headline"
    PP.<> PP.braces (pretty l)
    PP.<+> pretty t
{-# LINE 924 "ccweb.org" #-}
instance Pretty SourceBlock where
  pretty s = pretty $ PrettyStruct "SourceBlock"
               [ ("name", pretty (blockName s))
               , ("language", pretty (blockLanguage s))
               , ("properties", pretty (blockProperties s))
               , ("derived properties", pretty (blockDerivedProperties s))
               , ("location", pretty (blockLocation s))
               , ("lines", pretty (blockLines s))
               ]
{-# LINE 1009 "ccweb.org" #-}
instance Pretty Section where
  pretty s = pretty $ PrettyStruct "Section"
               [ ("number", pretty (sectionNumber s))
               , ("headline", pretty (sectionHeadline s))
               , ("properties", pretty (sectionProps s))
               , ("derived properties", pretty (sectionDerivedProperties s))
               , ("text", pretty (sectionDocumentation s))
               , ("code", pretty (sectionSourceBlock s))
               ]
{-# LINE 1118 "ccweb.org" #-}
instance Pretty Document where
  pretty d = pretty $ PrettyStruct "Document"
               [ ("keywords", pretty (orgKeywords d))
               , ("sections", pretty (orgSections d))
               ]
{-# LINE 198 "ccweb.org" #-}
data LogLevel = Quiet | Error | Warning | Info | Debug | Trace deriving (Eq,Show)

{-# LINE 200 "ccweb.org" #-}
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

{-# LINE 213 "ccweb.org" #-}
instance Pretty LogLevel where
  pretty = PP.text . show

{-# LINE 216 "ccweb.org" #-}
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
{-# LINE 344 "ccweb.org" #-}
type Parser = P.Parsec OrgFile ParserState

{-# LINE 346 "ccweb.org" #-}
class Parse a where
  parse :: Parser a
{-# LINE 352 "ccweb.org" #-}
data ParserState = ParserState
  { sectionCounter :: Int
  , propertyStack  :: Stack Properties
  , hostName       :: String
  , parserLogLevel :: LogLevel
  }

{-# LINE 359 "ccweb.org" #-}
instance Pretty ParserState where
  pretty s = pretty $ PrettyStruct "ParserState"
      [ ("counter", pretty (sectionCounter s))
      , ("host", pretty (hostName s))
      , ("log level", pretty (parserLogLevel s))
      , ("property stack", pretty (propertyStack s))
      ]

{-# LINE 367 "ccweb.org" #-}
initialParserState :: ParserState
initialParserState = ParserState
  { sectionCounter = 0
  , propertyStack  = Stack []
  , parserLogLevel = Debug
  , hostName       = []
  }
{-# LINE 378 "ccweb.org" #-}
parserDebug :: (Pretty a, Pretty b) => a -> b -> Parser ()
parserDebug l s = do
  lvl <- parserLogLevel <$> P.getState
  pos <- P.getPosition
  let label = pretty pos PP.<> PP.colon PP.<> pretty l PP.<> PP.colon
  if lvl >= Trace
  then trace (PP.render (PP.hang label 4 (pretty s))) $ return ()
  else return ()

{-# LINE 387 "ccweb.org" #-}
parserTrace :: Pretty a => a -> Parser ()
parserTrace l = do
  (OrgFile ls) <- P.stateInput <$> P.getParserState
  u <- P.getState
  let user = pretty "user state" PP.<> PP.colon PP.<> pretty u
      input = pretty "looking at" PP.<> PP.colon PP.<> pretty (take 3 ls)
  parserDebug l (PP.fcat [user, input])
{-# LINE 400 "ccweb.org" #-}
class Location a where
  newPosition :: P.SourcePos -> Char -> a -> P.SourcePos
  getPosition :: a -> P.SourcePos
  getPosition = error $ "getPosition not implemented"
{-# LINE 408 "ccweb.org" #-}
instance Location String where
  newPosition pos c _cs = P.updatePosChar pos c
{-# LINE 416 "ccweb.org" #-}
data OrgLine = OrgLine P.SourcePos String

{-# LINE 418 "ccweb.org" #-}
instance Location OrgLine where
  getPosition (OrgLine p _) = p
  newPosition _ _ (OrgLine p _) = p
{-# LINE 437 "ccweb.org" #-}
instance Monad m => P.Stream OrgLine m Char where
  -- The actual end of the stream
  uncons (
{-# LINE 432 "ccweb.org" #-}
          OrgLine _ ('\NUL':_)
{-# LINE 439 "ccweb.org" #-}
                              ) =
    return Nothing
  -- An empty string --- insert last newline
  uncons (OrgLine p []) =
    return $ Just ('\n', OrgLine (P.updatePosChar p '\n') "\NUL")
  -- Uncons a character
  uncons (OrgLine p (x:xs)) =
    return $ Just (x, OrgLine (P.updatePosChar p x) xs)
{-# LINE 465 "ccweb.org" #-}
newtype OrgFile = OrgFile [OrgLine]
{-# LINE 474 "ccweb.org" #-}
instance Monad m => P.Stream OrgFile m Char where
  uncons (OrgFile (
{-# LINE 432 "ccweb.org" #-}
                   OrgLine _ ('\NUL':_)
{-# LINE 475 "ccweb.org" #-}
                                       :[])) = return Nothing
  uncons (OrgFile (x:xs)) = P.uncons x >>= \case
    Nothing -> P.uncons (fromList xs :: OrgFile)
    Just (x',xs') -> return $ Just (x', OrgFile (xs':xs))
  uncons (OrgFile []) =
    error $ "(internal): uncons of empty OrgFile instance"
{-# LINE 485 "ccweb.org" #-}
instance Location OrgFile where
  getPosition (OrgFile (x:_)) = getPosition x
  getPosition _ = error $ "(internal) getPosition of empty OrgFile"

{-# LINE 489 "ccweb.org" #-}
  newPosition pos c (OrgFile (x:_)) = newPosition pos c x
  newPosition _ _ (OrgFile []) = error $ "(internal): location of empty OrgFile"
{-# LINE 495 "ccweb.org" #-}
instance IsList OrgFile where
  type Item OrgFile = OrgLine
  fromList xs = OrgFile xs
  toList (OrgFile xs) = xs
{-# LINE 515 "ccweb.org" #-}
satisfy :: (Location s, P.Stream s m Char) => (Char -> Bool) -> P.ParsecT s u m Char
satisfy f = P.tokenPrim
            (\c -> show [c])
            (\pos c cs -> newPosition pos c cs)
            (\c -> if f c then Just c else Nothing)
{-# LINE 530 "ccweb.org" #-}
char :: (Location s, P.Stream s m Char) => Char -> P.ParsecT s u m Char
char c = satisfy (==c) P.<?> show [c]

{-# LINE 533 "ccweb.org" #-}
newline :: (Location s, P.Stream s m Char) => P.ParsecT s u m Char
newline = char '\n' P.<?> "lf new-line"

{-# LINE 536 "ccweb.org" #-}
crlf :: (Location s, P.Stream s m Char) => P.ParsecT s u m Char
crlf = char '\r' *> char '\n' P.<?> "crlf new-line"

{-# LINE 539 "ccweb.org" #-}
endOfLine :: (Location s, P.Stream s m Char) => P.ParsecT s u m Char
endOfLine = newline <|> crlf P.<?> "new-line"
{-# LINE 549 "ccweb.org" #-}
anyChar :: (Location s, P.Stream s m Char) => P.ParsecT s u m Char
anyChar = P.noneOf "\n\r"

{-# LINE 552 "ccweb.org" #-}
space :: P.Stream s m Char => P.ParsecT s u m Char
space = P.satisfy (\c -> isSpace c && notElem c "\n\r")

{-# LINE 555 "ccweb.org" #-}
spaces :: P.Stream s m Char => P.ParsecT s u m String
spaces = P.many space

{-# LINE 558 "ccweb.org" #-}
spaces1 :: P.Stream s m Char => P.ParsecT s u m String
spaces1 = P.many1 space
{-# LINE 564 "ccweb.org" #-}
symbolChar :: P.Stream s m Char => P.ParsecT s u m Char
symbolChar = P.satisfy (\c -> isPrint c && not (isSpace c) && notElem c "()=")

{-# LINE 567 "ccweb.org" #-}
symbol :: P.Stream s m Char => P.ParsecT s u m String
symbol = P.many1 symbolChar
{-# LINE 573 "ccweb.org" #-}
line :: Parser a -> Parser a
line p = p <* endOfLine
{-# LINE 579 "ccweb.org" #-}
fromEither :: Pretty a => Either a b -> b
fromEither = either (error . PP.render . pretty) id

{-# LINE 582 "ccweb.org" #-}
ingest :: FilePath -> IO [OrgLine]
ingest fp = do
  ls <- fromEither
       <$> P.parse (P.many1 $ 
{-# LINE 601 "ccweb.org" #-}
                              liftA2 OrgLine P.getPosition (P.manyTill anyChar endOfLine)
{-# LINE 585 "ccweb.org" #-}
                                                                                         ) fp
       <$> readFile fp
  foldrM scan [] ls
  where
    scan l@(OrgLine p s) acc =
      case fromEither
           $ P.parse
           (P.setPosition p *> 
{-# LINE 606 "ccweb.org" #-}
                               P.optionMaybe (
                                 P.try (P.spaces *> P.string "#+INCLUDE:" *> P.spaces)
                                 *> P.char '"'
                                 *> P.manyTill anyChar (P.char '"')
                               )
{-# LINE 592 "ccweb.org" #-}
                                )
           (P.sourceName p) s
      of
        Nothing -> return (l : acc)
        Just fp' -> (++ acc) <$> ingest fp'
{-# LINE 628 "ccweb.org" #-}
instance Parse TextElement where
  parse = literalOr (return . Plain) (
{-# LINE 634 "ccweb.org" #-}
                                      P.try (Bold <$> enclosed '*')
                                      <|> P.try (InlineCode <$> enclosed '~')
                                      <|> P.try (Italics <$> enclosed '/')
                                      <|> P.try (StrikeThrough <$> enclosed '+')
                                      <|> P.try (Verbatim <$> enclosed '=')
{-# LINE 629 "ccweb.org" #-}
                                                                           )
{-# LINE 653 "ccweb.org" #-}
instance Parse Text where
  parse = do
    --parserTrace "Text: parse"
    P.notFollowedBy (parse :: Parser Headline)
    P.notFollowedBy (spaces *> P.string "#+NAME")
    P.notFollowedBy (spaces *> P.string "#+BEGIN_SRC")
    Text <$> line (P.many1 parse :: Parser [TextElement])
{-# LINE 673 "ccweb.org" #-}
instance Location CodeElement where
  getPosition (Literal p _) = p
  getPosition (SectionReference p _) = p

{-# LINE 677 "ccweb.org" #-}
  newPosition _ _ _ = error $ "(internal) newPosition of CodeElement"
{-# LINE 682 "ccweb.org" #-}
instance Parse CodeElement where
  parse = literalOr
              (liftA2 Literal P.getPosition . return)
              (P.try $ 
{-# LINE 690 "ccweb.org" #-}
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
{-# LINE 685 "ccweb.org" #-}
                                 )
{-# LINE 713 "ccweb.org" #-}
instance Parse CodeLine where
  parse = line (liftA2 CodeLine
                     P.getPosition
                     (P.many parse :: Parser [CodeElement]))
{-# LINE 740 "ccweb.org" #-}
instance Parse SExpression where
  parse =
    (
{-# LINE 751 "ccweb.org" #-}
     Series <$> ( P.try (P.char '(')
                  *> spaces
                  *> (P.sepEndBy parse spaces1 <* P.char ')')
                )
{-# LINE 742 "ccweb.org" #-}
                 )
    <|>
    P.try (
{-# LINE 759 "ccweb.org" #-}
           YesNo <$> (
             (P.string "no" *> P.lookAhead P.space *> return False)
             <|>
             (P.string "yes" *> P.lookAhead P.space *> return True)
             )
{-# LINE 744 "ccweb.org" #-}
              )
    <|>
    (
{-# LINE 768 "ccweb.org" #-}
     Atom <$> (enclosed '"' <|> symbol)
{-# LINE 746 "ccweb.org" #-}
                                       )
{-# LINE 783 "ccweb.org" #-}
class Eval a where
  eval :: ParserState -> a -> a

{-# LINE 786 "ccweb.org" #-}
instance Eval SExpression where
  eval _ (Series [Atom "eq", Atom "system-type", Atom ('\'':t)]) =
    YesNo $ case (Sys.os, t) of
              ("linux", "gnu/linux") -> True
              ("mingw32", "windows-nt") -> True
              ("darwin", "darwin") -> True
              _ -> False

{-# LINE 794 "ccweb.org" #-}
  eval s (Series [Atom "unless", expr, result]) =
    case eval s expr of
      YesNo False -> eval s result
      YesNo True  -> YesNo False
      expr' -> expr'

{-# LINE 800 "ccweb.org" #-}
  eval s (Series [Atom "when", expr, result]) =
    case eval s expr of
      YesNo True -> eval s result
      expr' -> expr'

{-# LINE 805 "ccweb.org" #-}
  eval s (Series [Atom "system-name"]) =
    Atom $ hostName s 

{-# LINE 808 "ccweb.org" #-}
  eval s (Series [Atom "string-suffix-p", suffix, expr]) =
    case (eval s suffix, eval s expr) of
      (Atom s', Atom str) -> YesNo $ isSuffixOf s' str
      (e1, e2) -> Series [Atom "string-suffix-p", e1, e2]

{-# LINE 813 "ccweb.org" #-}
  eval _ x = x
{-# LINE 827 "ccweb.org" #-}
instance Eval Property where; eval s = Map.map (eval s)
instance Eval Properties where; eval s = Map.map (eval s)
{-# LINE 833 "ccweb.org" #-}
instance Parse Properties where
  parse = (
{-# LINE 840 "ccweb.org" #-}
           do
             _ <- P.try (P.string "#+PROPERTY:") *> spaces
             p <- symbol <* spaces1
             kvs <- 
{-# LINE 849 "ccweb.org" #-}
                    line (Map.fromList
                          <$> P.sepEndBy
                           (
{-# LINE 857 "ccweb.org" #-}
                            do
                              k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                              v <- parse :: Parser SExpression
                              return (k, v)
{-# LINE 851 "ccweb.org" #-}
                                           )
                           spaces1)
{-# LINE 844 "ccweb.org" #-}
             return $ Map.singleton p kvs
{-# LINE 834 "ccweb.org" #-}
                                         )
              <|> (
{-# LINE 865 "ccweb.org" #-}
                   do
                     _ <- P.try (spaces *> P.string ":PROPERTIES:" *> spaces *> endOfLine)
                     Map.fromList <$> P.manyTill
                       (
{-# LINE 874 "ccweb.org" #-}
                        do
                          p <- spaces *> (enclosed ':' <* spaces)
                          kvs <- 
{-# LINE 849 "ccweb.org" #-}
                                 line (Map.fromList
                                       <$> P.sepEndBy
                                        (
{-# LINE 857 "ccweb.org" #-}
                                         do
                                           k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                                           v <- parse :: Parser SExpression
                                           return (k, v)
{-# LINE 851 "ccweb.org" #-}
                                                        )
                                        spaces1)
{-# LINE 877 "ccweb.org" #-}
                          return (p, kvs)
{-# LINE 868 "ccweb.org" #-}
                                         )
                       (P.try (spaces *> P.string ":END:" *> spaces *> endOfLine))
{-# LINE 835 "ccweb.org" #-}
                                                                                  )
{-# LINE 882 "ccweb.org" #-}
fileMode :: SExpression -> F.FileMode
fileMode (Series (Atom "identity" : Atom ('#':'o':a:g:u:[]) : [])) =
  fromIntegral $ 8 * ((8 * oct a) + oct g) + oct u
  where oct c = ord c - ord '0'
fileMode _ = 420
{-# LINE 902 "ccweb.org" #-}
instance Parse Headline where
  parse = do
    s <- P.try (char '*') *> P.many (char '*')
    _ <- spaces1
    t <- parse :: Parser Text
    return $ Headline (length s) t
{-# LINE 912 "ccweb.org" #-}
headlineLevel :: Headline -> Int
headlineLevel (Headline x _) = x
{-# LINE 937 "ccweb.org" #-}
instance Parse SourceBlock where
  parse = do
    --parserTrace "SourceBlock: trying to find a code block here"
    n  <- P.optionMaybe (
{-# LINE 965 "ccweb.org" #-}
                         P.try (spaces *> P.string "#+NAME:")
                         *> spaces
                         *> parse :: Parser Text
{-# LINE 940 "ccweb.org" #-}
                                                )
    i  <- P.try (spaces <* (P.string "#+BEGIN_SRC" *> spaces1))
    --parserTrace "SourceBlock: found a code block, parsing language"
    l  <- symbol <* spaces
    --parserTrace "SourceBlock: found a code block, parsing properties"
    ps <- 
{-# LINE 849 "ccweb.org" #-}
          line (Map.fromList
                <$> P.sepEndBy
                 (
{-# LINE 857 "ccweb.org" #-}
                  do
                    k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                    v <- parse :: Parser SExpression
                    return (k, v)
{-# LINE 851 "ccweb.org" #-}
                                 )
                 spaces1)
{-# LINE 946 "ccweb.org" #-}
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
{-# LINE 977 "ccweb.org" #-}
emptyLine :: (Location s, P.Stream s m Char) => P.ParsecT s u m ()
emptyLine = void $ spaces *> endOfLine

{-# LINE 980 "ccweb.org" #-}
skipEmptyLines :: (Location s, P.Stream s m Char) => P.ParsecT s u m ()
skipEmptyLines = P.skipMany (P.try emptyLine)
{-# LINE 986 "ccweb.org" #-}
literalOr :: (Location s, P.Stream s m Char) => (String -> P.ParsecT s u m a) -> P.ParsecT s u m a -> P.ParsecT s u m a
literalOr f p = scan "" where
  scan [] = p <|> (anyChar >>= scan . (:""))
  scan acc =
    ((P.lookAhead p) *> (f $ reverse acc))
    <|> (anyChar >>= scan . (:acc))
    <|> (f $ reverse acc)
{-# LINE 1001 "ccweb.org" #-}
enclosed :: (Location s, P.Stream s m Char) => Char -> P.ParsecT s u m String
enclosed d = P.try (P.char d) *> P.manyTill anyChar (P.char d)
{-# LINE 1022 "ccweb.org" #-}
instance HeaderArgs Section where
  headerArgs = headerArgs . sectionDerivedProperties
{-# LINE 1028 "ccweb.org" #-}
instance Parse Section where
  parse = do
    parserTrace "start of section parsing"

{-# LINE 1032 "ccweb.org" #-}
    skipEmptyLines
    h <- P.optionMaybe parse :: Parser (Maybe Headline)

{-# LINE 1035 "ccweb.org" #-}
    skipEmptyLines
    ps <- case h of
      Nothing -> return Map.empty
      _ -> parse <|> return Map.empty :: Parser Properties
    parserDebug "section properties" ps

{-# LINE 1041 "ccweb.org" #-}
    when (isJust h) $ do
      stk' <- resize (1 + (headlineLevel $ fromJust h)) . propertyStack <$> P.getState
      P.updateState (\s -> s{ propertyStack = stk' })
      ps' <- Map.unionWith Map.union ps . top . propertyStack <$> P.getState
      P.updateState (\s -> s{ propertyStack = push ps' stk' })
    P.getState >>= parserDebug "updated parser properties" . propertyStack

{-# LINE 1048 "ccweb.org" #-}
    skipEmptyLines
    ts  <- P.optionMaybe $ P.many1 $ parse :: Parser (Maybe [Text])

{-# LINE 1051 "ccweb.org" #-}
    skipEmptyLines
    c <- case (ts, h) of
          (Nothing, Nothing) -> Just <$> (parse :: Parser SourceBlock)
          (_, _) -> P.optionMaybe $ parse :: Parser (Maybe SourceBlock)

{-# LINE 1056 "ccweb.org" #-}
    skipEmptyLines

{-# LINE 1058 "ccweb.org" #-}
    parserTrace "end of section parsing"

{-# LINE 1060 "ccweb.org" #-}
    state <- P.getState
    let n = 1 + sectionCounter state
        ps' = top . propertyStack $ state
    P.updateState (\s -> s{ sectionCounter = n })
    return Section
      { sectionNumber = n
      , sectionHeadline = h
      , sectionProps = ps
      , sectionDerivedProperties = eval state ps'
      , sectionDocumentation = fromMaybe [] ts
      , sectionSourceBlock = c
      }
{-# LINE 1078 "ccweb.org" #-}
instance Pretty Keyword where
  pretty (AuthorKeyword a) = PP.text "Author:" PP.<+> pretty a
  pretty (PropertyKeyword a) = PP.text "Property:" PP.<+> pretty a
  pretty (TitleKeyword a) = PP.text "Title:" PP.<+> pretty a
  pretty (OtherKeyword a) = PP.text "Other:" PP.<+> pretty a
{-# LINE 1087 "ccweb.org" #-}
instance Parse Keyword where
  parse = propertyKeyword <|> titleKeyword <|> authorKeyword <|> otherKeyword
    where
      authorKeyword = do
        keywordStart "#+AUTHOR:"
        AuthorKeyword <$> parse
      propertyKeyword = do
        PropertyKeyword <$> 
{-# LINE 840 "ccweb.org" #-}
                            do
                              _ <- P.try (P.string "#+PROPERTY:") *> spaces
                              p <- symbol <* spaces1
                              kvs <- 
{-# LINE 849 "ccweb.org" #-}
                                     line (Map.fromList
                                           <$> P.sepEndBy
                                            (
{-# LINE 857 "ccweb.org" #-}
                                             do
                                               k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                                               v <- parse :: Parser SExpression
                                               return (k, v)
{-# LINE 851 "ccweb.org" #-}
                                                            )
                                            spaces1)
{-# LINE 844 "ccweb.org" #-}
                              return $ Map.singleton p kvs
{-# LINE 1095 "ccweb.org" #-}
      titleKeyword = do
        keywordStart "#+TITLE:"
        TitleKeyword <$> parse
      otherKeyword = do
        _ <- P.try $ P.string "#+"
        t <- P.many anyChar
        _ <- endOfLine
        return $ OtherKeyword t
      keywordStart h = void $ P.try (P.string h *> spaces)
{-# LINE 1108 "ccweb.org" #-}
headerProperties :: [Keyword] -> Properties
headerProperties = foldl acc mempty where
    acc a (PropertyKeyword p) = Map.union p a
    acc a _ = a
{-# LINE 1127 "ccweb.org" #-}
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
      { orgKeywords = hs
      , orgSections = ss
      }
{-# LINE 1144 "ccweb.org" #-}
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
{-# LINE 234 "ccweb.org" #-}
data Options = Options
  { logLevel :: LogLevel
  , dryRun :: Bool
  , listInputFiles :: Bool
  , listOutputFiles :: Bool
  , inputFile :: String
  }

{-# LINE 242 "ccweb.org" #-}
instance Pretty Options where
  pretty o = pretty $ PrettyStruct "Options"
    [ ("log level", pretty $ logLevel o)
    , ("dry-run", pretty $ dryRun o)
    , ("list-input-files", pretty $ listInputFiles o)
    , ("list-output-files", pretty $ listOutputFiles o)
    , ("input file", pretty $ inputFile o)
    ]

{-# LINE 251 "ccweb.org" #-}
userOptionParser :: O.Parser Options
userOptionParser = Options
  <$> 
{-# LINE 262 "ccweb.org" #-}
      ((
{-# LINE 267 "ccweb.org" #-}
        (\vs -> case length vs of
                 0 -> Error
                 1 -> Warning
                 2 -> Info
                 3 -> Debug
                 _ -> Trace)
        <$>
        O.some
          ( O.flag' ()
            ( O.short 'v'
              <> O.long "verbose"
              <> O.help "Be verbose. Can be given multiple times for more verbosity."
            ))
{-# LINE 262 "ccweb.org" #-}
              ) <|> (
{-# LINE 284 "ccweb.org" #-}
                     O.flag Warning Quiet
                       ( O.short 'q'
                         <> O.long "quiet"
                         <> O.help "Only print things that were asked for."
                       )
{-# LINE 262 "ccweb.org" #-}
                        ))
{-# LINE 254 "ccweb.org" #-}
  <*> dryRunParser
  <*> 
{-# LINE 310 "ccweb.org" #-}
      O.switch
        ( O.short 'I'
        <> O.long "list-input-files"
        <> O.help "If given, list the files that have to be read in, one per line."
        )
{-# LINE 256 "ccweb.org" #-}
  <*> 
{-# LINE 301 "ccweb.org" #-}
      O.switch
        ( O.short 'O'
        <> O.long "list-output-files"
        <> O.help "If given, list the files that would be written out, one per line."
        )
{-# LINE 257 "ccweb.org" #-}
  <*> (
{-# LINE 293 "ccweb.org" #-}
       O.argument O.str
         ( O.metavar "FILE"
         <> O.help "The name of the input file."
         )
{-# LINE 257 "ccweb.org" #-}
          )
{-# LINE 319 "ccweb.org" #-}
dryRunParser :: O.Parser Bool
dryRunParser = O.switch
  ( O.short 'n'
  <> O.long "dry-run"
  <> O.help "If given, do not write any files. Useful when debugging."
  )
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
{-# LINE 38 "org/tangle.org" #-}
             (map (\(fp,b) -> (case fp of { ('~':xs) -> home ++ xs; _ -> fp }, b))
                  :: [(FilePath, [SourceBlock])] -> [(FilePath, [SourceBlock])])
               . (map (\(f,ss) -> (f, map (fromJust . sectionSourceBlock) ss))
                  :: [(FilePath, [Section])] -> [(FilePath, [SourceBlock])])
               . (mapMaybe (\(i,ss) -> case i of; (FileBlock f) -> Just (f,ss); _ -> Nothing)
                  :: DocumentPartition -> [(FilePath, [Section])])
               . (
{-# LINE 125 "org/doc.org" #-}
                  (map (\bs -> (fst (head bs), map snd bs))
                       :: [[(SourceBlockId, Section)]] -> DocumentPartition)
                    . (groupWith fst
                       :: [(SourceBlockId, Section)] -> [[(SourceBlockId, Section)]])
                    . (sortWith fst
                       :: [(SourceBlockId, Section)] -> [(SourceBlockId, Section)])
                    . (mapMaybe (\s -> sectionSourceBlock s >>= sourceBlockId >>= Just . (,s))
                       :: [Section] -> [(SourceBlockId, Section)])
                    . orgSections
{-# LINE 45 "org/tangle.org" #-}
                  :: Document -> DocumentPartition)
               $ doc
{-# LINE 20 "org/tangle.org" #-}
                     :: [(FilePath, [SourceBlock])]
{-# LINE 55 "org/tangle.org" #-}
  when (listOutputFiles opts) $ mapM_ putStrLn (map fst outs)
  when (listInputFiles opts)  $ mapM_ putStrLn (
    nub . sort .
    map (\(OrgLine p _) -> P.sourceName p) $ (toList ingested)
    )
{-# LINE 22 "org/tangle.org" #-}
  logM (logLevel opts) Debug $ PP.render (pretty outs)

{-# LINE 24 "org/tangle.org" #-}
  mapM_ (tangleFile opts doc) outs
  where
    optParser = O.info (userOptionParser <**> O.helper)
      ( O.fullDesc
      <> O.progDesc "Tangle the input FILE"
      <> O.header "cctangle - A literate programming tangler" )
{-# LINE 69 "org/tangle.org" #-}
data Line = CodeText String | LinePragma String P.SourcePos
{-# LINE 77 "org/tangle.org" #-}
instance Show Line where
  show (CodeText str) = str ++ "\n"
{-# LINE 87 "org/tangle.org" #-}
  show (LinePragma "haskell" pos) = unwords
    [ "{-# LINE", show (P.sourceLine pos)
    , "\"" ++ P.sourceName pos ++ "\""
    , "#-}\n" ]
{-# LINE 97 "org/tangle.org" #-}
  show (LinePragma "c" pos) = unwords
    [ "#line", show (P.sourceLine pos)
    , "\"" ++ P.sourceName pos ++ "\"\n"
    ]
  show (LinePragma "c++" pos) = show (LinePragma "c" pos)
{-# LINE 80 "org/tangle.org" #-}
  show (LinePragma _ _) = []
{-# LINE 129 "org/tangle.org" #-}
type Indent = String
{-# LINE 140 "org/tangle.org" #-}
data TangleState = TangleState
  { document :: Document
  , indent :: Indent
  , blocks :: Stack SourceBlock
  , pragmaLanguage :: String
  }
{-# LINE 153 "org/tangle.org" #-}
type Tangler a = State TangleState a

{-# LINE 155 "org/tangle.org" #-}
getTopBlock :: Tangler SourceBlock
getTopBlock = top . blocks <$> get

{-# LINE 158 "org/tangle.org" #-}
getIndent :: Tangler Indent
getIndent = indent <$> get

{-# LINE 161 "org/tangle.org" #-}
setIndentFrom :: String -> Tangler ()
setIndentFrom str = modify
  (\s -> s{ indent = replicate (length str) ' ' })

{-# LINE 165 "org/tangle.org" #-}
addIndentFrom :: String -> Tangler ()
addIndentFrom str = modify
  (\s -> s{ indent = indent s ++ replicate (length str) ' ' })
{-# LINE 176 "org/tangle.org" #-}
tangleSourceBlock :: SourceBlock -> Tangler [Line]
tangleSourceBlock block = do
  modify (\s -> s{ blocks = push block (blocks s) })
  ls <- concat <$> mapM tangleCodeLine (blockLines block)
  modify (\s -> s{ blocks = pop (blocks s) })
  maybeAddLinePragma ls (blockLocation block) block
{-# LINE 186 "org/tangle.org" #-}
maybeAddLinePragma :: [Line] -> P.SourcePos -> SourceBlock -> Tangler [Line]
maybeAddLinePragma ls pos block = do
  lang <- pragmaLanguage <$> get
  let pragma = LinePragma lang pos
  return $ case (headerArg ":comments" block, show pragma) of
    (Just (YesNo True), (_:_)) -> pragma : ls
    _ -> ls
{-# LINE 201 "org/tangle.org" #-}
tangleCodeLine :: CodeLine -> Tangler [Line]
tangleCodeLine (CodeLine _ []) = (:[]) . CodeText <$> getIndent
{-# LINE 230 "org/tangle.org" #-}
tangleCodeLine (CodeLine _ elements) =
  do
    initialIndent <- getIndent
    ls <- removeLeadingEmptyLine . 
{-# LINE 223 "org/tangle.org" #-}
                                   reverse <$> foldlM
                                     (
{-# LINE 259 "org/tangle.org" #-}
                                      \acc element -> do
                                        let (accPragmas, accRest) = breakCodeText acc
                                        case (accPragmas, accRest, element) of
{-# LINE 272 "org/tangle.org" #-}
                                          ([], (CodeText l:ls), Literal _ s) -> do
                                            addIndentFrom s
                                            return $ CodeText (l ++ s) : ls
{-# LINE 284 "org/tangle.org" #-}
                                          (_, _, Literal pos s) -> do
                                            i <- getIndent
                                            addIndentFrom s
                                            getTopBlock >>= maybeAddLinePragma (CodeText (i ++ s) : acc) pos
{-# LINE 303 "org/tangle.org" #-}
                                          (_, (CodeText l:ls), SectionReference pos name) -> do
                                            refLines <- 
{-# LINE 321 "org/tangle.org" #-}
                                                        do
                                                          bs <- 
{-# LINE 329 "org/tangle.org" #-}
                                                                filter (\b -> case (blockName b) of
                                                                               Nothing -> False
                                                                               Just t -> t == name)
                                                                  . mapMaybe sectionSourceBlock
                                                                  . orgSections
                                                                  . document
                                                                  <$> get
{-# LINE 322 "org/tangle.org" #-}
                                                                          :: Tangler [SourceBlock]
                                                          when (null bs) (error $ "source block not defined anywhere: " ++ show name)
                                                          concat <$> mapM tangleSourceBlock bs :: Tangler [Line]
{-# LINE 305 "org/tangle.org" #-}
                                            let (refPragmas, (CodeText first : rest)) = breakCodeText refLines
                                            acc' <- case (refPragmas, accPragmas) of
                                              ([], []) -> do
                                                unindented <- 
{-# LINE 340 "org/tangle.org" #-}
                                                              (\str -> do
                                                                 i <- getIndent
                                                                 return $ if isPrefixOf i str; then drop (length i) str; else str
                                                              )
{-# LINE 308 "org/tangle.org" #-}
                                                                first
                                                addIndentFrom unindented
                                                return $ reverse rest ++ (CodeText(l ++ unindented) : ls)
                                              _ -> do
                                                let reverseRefLines = reverse refLines
                                                    CodeText lst = head . snd $ breakCodeText reverseRefLines
                                                setIndentFrom lst
                                                return $ reverseRefLines ++ acc
                                            getTopBlock >>= maybeAddLinePragma acc' pos
{-# LINE 263 "org/tangle.org" #-}
                                          _ -> error $ "unreachable element tangling case"
{-# LINE 224 "org/tangle.org" #-}
                                                                                          )
                                     [CodeText initialIndent] elements
{-# LINE 234 "org/tangle.org" #-}
    modify (\s -> s{ indent = initialIndent })
    getTopBlock >>= maybeAddLinePragma ls (getPosition $ head elements)
      where
        removeLeadingEmptyLine :: [Line] -> [Line]
        removeLeadingEmptyLine a@(CodeText l:p@(LinePragma _ _):ls)
          | all isSpace l = p:ls
          | otherwise = a
        removeLeadingEmptyLine a = a
{-# LINE 249 "org/tangle.org" #-}
breakCodeText :: [Line] -> ([Line],[Line])
breakCodeText = break (\case { (CodeText _) -> True; _ -> False })
{-# LINE 351 "org/tangle.org" #-}
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
{-# LINE 111 "org/tangle.org" #-}
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
{-# LINE 364 "org/tangle.org" #-}
                                           ls
  logM (logLevel opts) Info $ "Writing the output file (" ++ fp ++ ")"
  if dryRun opts
    then logM (logLevel opts) Info contents
    else do
      let dir = F.takeDirectory fp
      when (mkDir && dir /= ".") $ D.createDirectoryIfMissing True dir

{-# LINE 372 "org/tangle.org" #-}
      F.fileExist fp >>= (`when` F.removeLink fp)
      writeFile fp contents

{-# LINE 375 "org/tangle.org" #-}
      case headerArg ":tangle-mode" block of
        Nothing -> return ()
        Just expr -> do
          let mode = fileMode expr
          F.setFileMode fp mode
  where
    mkDir = case headerArg ":mkdirp" (head bs) of
              Just (YesNo x) -> x
              _ -> False
{-# LINE 5 "org/tangle.org" #-}
