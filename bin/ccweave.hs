{-# LINE 63 "ccweb.org" #-}
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
import Data.Maybe (fromJust, isJust, mapMaybe)
import qualified Data.Map as Map
import GHC.Exts (IsList(..), groupWith, sortWith)
import Network.HostName (getHostName)
{-# LINE 385 "org/tangle.org" #-}
import qualified System.Directory as D
import qualified System.Environment as Env
import qualified System.FilePath as F
import qualified System.Posix.Files as F
import qualified System.Posix.Types as F
{-# LINE 119 "ccweb.org" #-}
import qualified Text.PrettyPrint as PP
{-# LINE 215 "ccweb.org" #-}
import qualified Options.Applicative as O
{-# LINE 323 "ccweb.org" #-}
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
{-# LINE 784 "ccweb.org" #-}
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
  { sectionHeadline :: Maybe Headline
  , sectionProps :: Properties
  , sectionDerivedProperties :: Properties
  , sectionDocumentation :: [Text]
  , sectionSourceBlock :: Maybe SourceBlock
  }
{-# LINE 32 "org/doc.org" #-}
data Headline = Headline Int Text
{-# LINE 37 "org/doc.org" #-}
data Text = Text [TextElement] deriving (Eq, Ord, Show)

{-# LINE 39 "org/doc.org" #-}
data TextElement =
  Bold String
  | InlineCode String
  | Italics String
  | Plain String
  | StrikeThrough String
  | Verbatim String
  deriving (Eq, Ord, Show)
{-# LINE 51 "org/doc.org" #-}
data SourceBlock = SourceBlock
  { blockName :: Maybe Text
  , blockLanguage :: String
  , blockProperties :: Property
  , blockDerivedProperties :: Property
  , blockLocation :: P.SourcePos
  , blockLines :: [CodeLine]
  }
{-# LINE 63 "org/doc.org" #-}
data CodeElement = Literal P.SourcePos String
                 | SectionReference P.SourcePos Text

{-# LINE 66 "org/doc.org" #-}
data CodeLine = CodeLine P.SourcePos [CodeElement]
{-# LINE 71 "org/doc.org" #-}
type Property = Map.Map String SExpression
type Properties = Map.Map String Property
{-# LINE 77 "org/doc.org" #-}
data SExpression =
  Atom String
  | Series [SExpression]
  | YesNo Bool
  deriving (Eq, Ord)
{-# LINE 86 "org/doc.org" #-}
class HeaderArgs a where
  headerArgs :: a -> Property
  headerArg :: String -> a -> Maybe SExpression
  headerArg k a = Map.lookup k $ headerArgs a

{-# LINE 91 "org/doc.org" #-}
instance HeaderArgs Properties where
  headerArgs = Map.findWithDefault Map.empty "header-args"

{-# LINE 94 "org/doc.org" #-}
instance HeaderArgs SourceBlock where
  headerArgs = blockDerivedProperties
{-# LINE 100 "org/doc.org" #-}
data SourceBlockId = FileBlock FilePath | NamedBlock Text deriving Eq

{-# LINE 102 "org/doc.org" #-}
instance Ord SourceBlockId where
  (<=) (FileBlock p1) (FileBlock p2) = p1 <= p2
  (<=) (NamedBlock p1) (NamedBlock p2) = p1 <= p2
  (<=) (FileBlock _) (NamedBlock _) = False
  (<=) _ _ = True

{-# LINE 108 "org/doc.org" #-}
sourceBlockId :: SourceBlock -> Maybe SourceBlockId
sourceBlockId SourceBlock{ blockName = (Just name) } = Just $ NamedBlock name
sourceBlockId block = case headerArg ":tangle" block of
    Nothing -> Nothing
    Just (Atom f) -> Just $ FileBlock f
    Just (YesNo _) -> Nothing
    Just e -> error $ "unsupported tangle destination: " ++ show e
{-# LINE 129 "ccweb.org" #-}
class Pretty a where
  pretty :: a -> PP.Doc
  prettyList :: [a] -> PP.Doc
  prettyList = PP.brackets . PP.fcat . PP.punctuate PP.comma . map pretty

{-# LINE 134 "ccweb.org" #-}
instance Pretty PP.Doc where
  pretty = id

{-# LINE 137 "ccweb.org" #-}
instance Pretty Bool where
  pretty True = PP.text "True"
  pretty False = PP.text "False"

{-# LINE 141 "ccweb.org" #-}
instance Pretty Char where
  pretty = PP.char
  prettyList = PP.text

{-# LINE 145 "ccweb.org" #-}
instance Pretty Int where
  pretty = PP.int

{-# LINE 148 "ccweb.org" #-}
instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = PP.text "Nothing"
  pretty (Just a) = pretty a

{-# LINE 152 "ccweb.org" #-}
instance Pretty a => Pretty [a] where
  pretty = prettyList

{-# LINE 155 "ccweb.org" #-}
instance Pretty P.ParseError where
  pretty = PP.text . show
{-# LINE 161 "ccweb.org" #-}
instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
  pretty = PP.brackets
           . PP.fcat
           . PP.punctuate PP.comma
           . map (\(k,v) -> pretty k PP.<> PP.equals PP.<> pretty v)
           . Map.toList
{-# LINE 171 "ccweb.org" #-}
data PrettyStruct = PrettyStruct String [(String, PP.Doc)]

{-# LINE 173 "ccweb.org" #-}
instance Pretty PrettyStruct where
  pretty (PrettyStruct t ps) =
    let fields = map (\(n,p) -> PP.text n PP.<> PP.colon PP.<+> p) ps
    in PP.text t PP.<> PP.braces (PP.fcat $ PP.punctuate PP.comma fields)
{-# LINE 181 "ccweb.org" #-}
instance Pretty P.SourcePos where
  pretty p = pretty (P.sourceName p)
             PP.<> PP.colon PP.<> pretty (P.sourceLine p)
             PP.<> PP.colon PP.<> pretty (P.sourceColumn p)
{-# LINE 424 "ccweb.org" #-}
instance Pretty OrgLine where
  pretty (OrgLine p l) =
    pretty p
    PP.<> PP.colon
    PP.<> PP.text (takeWhile (/= '\NUL') l)
{-# LINE 474 "ccweb.org" #-}
instance Pretty OrgFile where
  pretty (OrgFile ls) = pretty ls
{-# LINE 588 "ccweb.org" #-}
instance Pretty TextElement where
  pretty (Bold a) = PP.text "Bold:" PP.<+> pretty a
  pretty (InlineCode a) = PP.text "InlineCode:" PP.<+> pretty a
  pretty (Italics a) = PP.text "Italics:" PP.<+> pretty a
  pretty (Plain a) = pretty a
  pretty (StrikeThrough a) = PP.text "StrikeThrough:" PP.<+> pretty a
  pretty (Verbatim a) = PP.text "Verbatim:" PP.<+> pretty a
{-# LINE 616 "ccweb.org" #-}
instance Pretty Text where
  pretty (Text xs) = PP.text "Text:" PP.<+> PP.hcat (map pretty xs)
{-# LINE 632 "ccweb.org" #-}
instance Pretty CodeElement where
  pretty (Literal p s) = PP.parens $ pretty p PP.<> PP.colon PP.<> pretty s
  pretty (SectionReference p t) = pretty p PP.<> PP.colon PP.<> PP.char '«' PP.<> pretty t PP.<> PP.char '»'
{-# LINE 673 "ccweb.org" #-}
instance Pretty CodeLine where
  pretty (CodeLine p xs) = pretty p PP.<> PP.colon PP.<> pretty xs
{-# LINE 693 "ccweb.org" #-}
instance Show SExpression where
  show (Atom x) = x
  show (Series xs) = "(" ++ intercalate " " (map show xs) ++ ")"
  show (YesNo x) = show x

{-# LINE 698 "ccweb.org" #-}
instance Pretty SExpression where
  pretty (Atom x) = PP.text x
  pretty (Series xs) = PP.parens $ PP.fsep (map pretty xs)
  pretty (YesNo x) = PP.text (show x)
{-# LINE 859 "ccweb.org" #-}
instance Pretty Headline where
  pretty (Headline l t) =
    PP.text "Headline"
    PP.<> PP.braces (pretty l)
    PP.<+> pretty t
{-# LINE 890 "ccweb.org" #-}
instance Pretty SourceBlock where
  pretty s = pretty $ PrettyStruct "SourceBlock"
               [ ("name", pretty (blockName s))
               , ("language", pretty (blockLanguage s))
               , ("properties", pretty (blockProperties s))
               , ("derived properties", pretty (blockDerivedProperties s))
               , ("location", pretty (blockLocation s))
               , ("lines", pretty (blockLines s))
               ]
{-# LINE 975 "ccweb.org" #-}
instance Pretty Section where
  pretty s = pretty $ PrettyStruct "Section"
               [ ("headline", pretty (sectionHeadline s))
               , ("properties", pretty (sectionProps s))
               , ("derived properties", pretty (sectionDerivedProperties s))
               , ("text", pretty (sectionDocumentation s))
               , ("code", pretty (sectionSourceBlock s))
               ]
{-# LINE 1089 "ccweb.org" #-}
instance Pretty Document where
  pretty d = pretty $ PrettyStruct "Document"
               [ ("keywords", pretty (orgKeywords d))
               , ("sections", pretty (orgSections d))
               ]
{-# LINE 191 "ccweb.org" #-}
data LogLevel = Quiet | Error | Warning | Info | Debug deriving Show

{-# LINE 193 "ccweb.org" #-}
instance Pretty LogLevel where
  pretty = PP.text . show

{-# LINE 196 "ccweb.org" #-}
logM :: Pretty a => LogLevel -> LogLevel -> a -> IO ()
logM _ Quiet a = logM' a
logM Quiet _ _ = return ()
logM _ Error a = logM' a
logM Error _ _ = return ()
logM _ Warning a = logM' a
logM Warning _ _ = return ()
logM _ Info a = logM' a
logM Info _ _ = return ()
logM _ Debug a = logM' a

{-# LINE 207 "ccweb.org" #-}
logM' :: Pretty a => a -> IO ()
logM' = putStrLn . PP.render . pretty
{-# LINE 87 "ccweb.org" #-}
newtype Stack a = Stack [a]

{-# LINE 89 "ccweb.org" #-}
top :: Stack a -> a
top (Stack xs) = head xs

{-# LINE 92 "ccweb.org" #-}
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack $ x:xs

{-# LINE 95 "ccweb.org" #-}
pop :: Stack a -> Stack a
pop (Stack []) = error $ "popping empty stack"
pop (Stack (_:xs)) = Stack xs

{-# LINE 99 "ccweb.org" #-}
resize :: Int -> Stack a -> Stack a
resize i (Stack xs)
  | i == l = Stack xs
  | i < l = Stack $ reverse . take i . reverse $ xs
  | i > l && l > 0 = Stack $ replicate (i - l) (head xs) ++ xs
  | otherwise = error $ "resizing empty stack"
  where
    l = length xs
{-# LINE 334 "ccweb.org" #-}
type Parser = P.Parsec OrgFile ParserState

{-# LINE 336 "ccweb.org" #-}
class Parse a where
  parse :: Parser a
{-# LINE 342 "ccweb.org" #-}
data ParserState = ParserState
  { sectionNumber :: Int
  , propertyStack :: Stack Properties
  , hostName      :: String
  }

{-# LINE 348 "ccweb.org" #-}
initialParserState :: ParserState
initialParserState = ParserState
  { sectionNumber = 0
  , propertyStack = Stack []
  , hostName      = []
  }
{-# LINE 358 "ccweb.org" #-}
--parserTrace :: Pretty a => a -> Parser ()
--parserTrace l = do
--  (OrgFile ls) <- P.stateInput <$> P.getParserState
--  p <- P.getPosition
--  let label = pretty p PP.<> PP.colon PP.<> pretty l PP.<> PP.colon
--      state = pretty (take 3 ls)
--  Debug.trace (PP.render (PP.hang label 4 state)) $ return ()
{-# LINE 371 "ccweb.org" #-}
class Location a where
  newPosition :: P.SourcePos -> Char -> a -> P.SourcePos
  getPosition :: a -> P.SourcePos
  getPosition = error $ "getPosition not implemented"
{-# LINE 379 "ccweb.org" #-}
instance Location String where
  newPosition pos c _cs = P.updatePosChar pos c
{-# LINE 387 "ccweb.org" #-}
data OrgLine = OrgLine P.SourcePos String

{-# LINE 389 "ccweb.org" #-}
instance Location OrgLine where
  getPosition (OrgLine p _) = p
  newPosition _ _ (OrgLine p _) = p
{-# LINE 408 "ccweb.org" #-}
instance Monad m => P.Stream OrgLine m Char where
  -- The actual end of the stream
  uncons (
{-# LINE 403 "ccweb.org" #-}
          OrgLine _ ('\NUL':_)
{-# LINE 410 "ccweb.org" #-}
                              ) =
    return Nothing
  -- An empty string --- insert last newline
  uncons (OrgLine p []) =
    return $ Just ('\n', OrgLine (P.updatePosChar p '\n') "\NUL")
  -- Uncons a character
  uncons (OrgLine p (x:xs)) =
    return $ Just (x, OrgLine (P.updatePosChar p x) xs)
{-# LINE 436 "ccweb.org" #-}
newtype OrgFile = OrgFile [OrgLine]
{-# LINE 445 "ccweb.org" #-}
instance Monad m => P.Stream OrgFile m Char where
  uncons (OrgFile (
{-# LINE 403 "ccweb.org" #-}
                   OrgLine _ ('\NUL':_)
{-# LINE 446 "ccweb.org" #-}
                                       :[])) = return Nothing
  uncons (OrgFile (x:xs)) = P.uncons x >>= \case
    Nothing -> P.uncons (fromList xs :: OrgFile)
    Just (x',xs') -> return $ Just (x', OrgFile (xs':xs))
  uncons (OrgFile []) =
    error $ "(internal): uncons of empty OrgFile instance"
{-# LINE 456 "ccweb.org" #-}
instance Location OrgFile where
  getPosition (OrgFile (x:_)) = getPosition x
  getPosition _ = error $ "(internal) getPosition of empty OrgFile"

{-# LINE 460 "ccweb.org" #-}
  newPosition pos c (OrgFile (x:_)) = newPosition pos c x
  newPosition _ _ (OrgFile []) = error $ "(internal): location of empty OrgFile"
{-# LINE 466 "ccweb.org" #-}
instance IsList OrgFile where
  type Item OrgFile = OrgLine
  fromList xs = OrgFile xs
  toList (OrgFile xs) = xs
{-# LINE 486 "ccweb.org" #-}
satisfy :: (Location s, P.Stream s m Char) => (Char -> Bool) -> P.ParsecT s u m Char
satisfy f = P.tokenPrim
            (\c -> show [c])
            (\pos c cs -> newPosition pos c cs)
            (\c -> if f c then Just c else Nothing)
{-# LINE 501 "ccweb.org" #-}
char :: (Location s, P.Stream s m Char) => Char -> P.ParsecT s u m Char
char c = satisfy (==c) P.<?> show [c]

{-# LINE 504 "ccweb.org" #-}
newline :: (Location s, P.Stream s m Char) => P.ParsecT s u m Char
newline = char '\n' P.<?> "lf new-line"

{-# LINE 507 "ccweb.org" #-}
crlf :: (Location s, P.Stream s m Char) => P.ParsecT s u m Char
crlf = char '\r' *> char '\n' P.<?> "crlf new-line"

{-# LINE 510 "ccweb.org" #-}
endOfLine :: (Location s, P.Stream s m Char) => P.ParsecT s u m Char
endOfLine = newline <|> crlf P.<?> "new-line"
{-# LINE 520 "ccweb.org" #-}
anyChar :: (Location s, P.Stream s m Char) => P.ParsecT s u m Char
anyChar = P.noneOf "\n\r"

{-# LINE 523 "ccweb.org" #-}
space :: P.Stream s m Char => P.ParsecT s u m Char
space = P.satisfy (\c -> isSpace c && notElem c "\n\r")

{-# LINE 526 "ccweb.org" #-}
spaces :: P.Stream s m Char => P.ParsecT s u m String
spaces = P.many space

{-# LINE 529 "ccweb.org" #-}
spaces1 :: P.Stream s m Char => P.ParsecT s u m String
spaces1 = P.many1 space
{-# LINE 535 "ccweb.org" #-}
symbolChar :: P.Stream s m Char => P.ParsecT s u m Char
symbolChar = P.satisfy (\c -> isPrint c && not (isSpace c) && notElem c "()=")

{-# LINE 538 "ccweb.org" #-}
symbol :: P.Stream s m Char => P.ParsecT s u m String
symbol = P.many1 symbolChar
{-# LINE 544 "ccweb.org" #-}
line :: Parser a -> Parser a
line p = p <* endOfLine
{-# LINE 550 "ccweb.org" #-}
fromEither :: Pretty a => Either a b -> b
fromEither = either (error . PP.render . pretty) id

{-# LINE 553 "ccweb.org" #-}
ingest :: FilePath -> IO [OrgLine]
ingest fp = do
  ls <- fromEither
       <$> P.parse (P.many1 $ 
{-# LINE 572 "ccweb.org" #-}
                              liftA2 OrgLine P.getPosition (P.manyTill anyChar endOfLine)
{-# LINE 556 "ccweb.org" #-}
                                                                                         ) fp
       <$> readFile fp
  foldrM scan [] ls
  where
    scan l@(OrgLine p s) acc =
      case fromEither
           $ P.parse
           (P.setPosition p *> 
{-# LINE 577 "ccweb.org" #-}
                               P.optionMaybe (
                                 P.try (P.spaces *> P.string "#+INCLUDE:" *> P.spaces)
                                 *> P.char '"'
                                 *> P.manyTill anyChar (P.char '"')
                               )
{-# LINE 563 "ccweb.org" #-}
                                              )
           (P.sourceName p) s
      of
        Nothing -> return (l : acc)
        Just fp' -> (++ acc) <$> ingest fp'
{-# LINE 599 "ccweb.org" #-}
instance Parse TextElement where
  parse = literalOr (return . Plain) (
{-# LINE 605 "ccweb.org" #-}
                                      P.try (Bold <$> enclosed '*')
                                      <|> P.try (InlineCode <$> enclosed '~')
                                      <|> P.try (Italics <$> enclosed '/')
                                      <|> P.try (StrikeThrough <$> enclosed '+')
                                      <|> P.try (Verbatim <$> enclosed '=')
{-# LINE 600 "ccweb.org" #-}
                                                                   )
{-# LINE 622 "ccweb.org" #-}
instance Parse Text where
  parse = do
    --parserTrace "Text: parse"
    Text <$> line (P.many1 parse :: Parser [TextElement])
{-# LINE 639 "ccweb.org" #-}
instance Location CodeElement where
  getPosition (Literal p _) = p
  getPosition (SectionReference p _) = p

{-# LINE 643 "ccweb.org" #-}
  newPosition _ _ _ = error $ "(internal) newPosition of CodeElement"
{-# LINE 648 "ccweb.org" #-}
instance Parse CodeElement where
  parse = literalOr
              (liftA2 Literal P.getPosition . return)
              (P.try $ 
{-# LINE 656 "ccweb.org" #-}
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
{-# LINE 651 "ccweb.org" #-}
                         )
{-# LINE 679 "ccweb.org" #-}
instance Parse CodeLine where
  parse = line (liftA2 CodeLine
                     P.getPosition
                     (P.many parse :: Parser [CodeElement]))
{-# LINE 706 "ccweb.org" #-}
instance Parse SExpression where
  parse =
    (
{-# LINE 717 "ccweb.org" #-}
     Series <$> ( P.try (P.char '(')
                  *> spaces
                  *> (P.sepEndBy parse spaces1 <* P.char ')')
                )
{-# LINE 708 "ccweb.org" #-}
                                    )
    <|>
    P.try (
{-# LINE 725 "ccweb.org" #-}
           YesNo <$> (
             (P.string "no" *> P.lookAhead P.space *> return False)
             <|>
             (P.string "yes" *> P.lookAhead P.space *> return True)
             )
{-# LINE 710 "ccweb.org" #-}
                      )
    <|>
    (
{-# LINE 734 "ccweb.org" #-}
     Atom <$> (enclosed '"' <|> symbol)
{-# LINE 712 "ccweb.org" #-}
                                       )
{-# LINE 749 "ccweb.org" #-}
class Eval a where
  eval :: ParserState -> a -> a

{-# LINE 752 "ccweb.org" #-}
instance Eval SExpression where
  eval _ (Series [Atom "eq", Atom "system-type", Atom ('\'':t)]) =
    YesNo $ case (Sys.os, t) of
              ("linux", "gnu/linux") -> True
              ("mingw32", "windows-nt") -> True
              ("darwin", "darwin") -> True
              _ -> False

{-# LINE 760 "ccweb.org" #-}
  eval s (Series [Atom "unless", expr, result]) =
    case eval s expr of
      YesNo False -> eval s result
      YesNo True  -> YesNo False
      expr' -> expr'

{-# LINE 766 "ccweb.org" #-}
  eval s (Series [Atom "when", expr, result]) =
    case eval s expr of
      YesNo True -> eval s result
      expr' -> expr'

{-# LINE 771 "ccweb.org" #-}
  eval s (Series [Atom "system-name"]) =
    Atom $ hostName s 

{-# LINE 774 "ccweb.org" #-}
  eval s (Series [Atom "string-suffix-p", suffix, expr]) =
    case (eval s suffix, eval s expr) of
      (Atom s', Atom str) -> YesNo $ isSuffixOf s' str
      (e1, e2) -> Series [Atom "string-suffix-p", e1, e2]

{-# LINE 779 "ccweb.org" #-}
  eval _ x = x
{-# LINE 793 "ccweb.org" #-}
instance Eval Property where; eval s = Map.map (eval s)
instance Eval Properties where; eval s = Map.map (eval s)
{-# LINE 799 "ccweb.org" #-}
instance Parse Properties where
  parse = (
{-# LINE 806 "ccweb.org" #-}
           do
             _ <- P.try (P.string "#+PROPERTY:") *> spaces
             p <- symbol <* spaces1
             kvs <- 
{-# LINE 815 "ccweb.org" #-}
                    line (Map.fromList
                          <$> P.sepEndBy
                           (
{-# LINE 823 "ccweb.org" #-}
                            do
                              k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                              v <- parse :: Parser SExpression
                              return (k, v)
{-# LINE 817 "ccweb.org" #-}
                              )
                           spaces1)
{-# LINE 810 "ccweb.org" #-}
             return $ Map.singleton p kvs
{-# LINE 800 "ccweb.org" #-}
             )
              <|> (
{-# LINE 831 "ccweb.org" #-}
                   do
                     _ <- P.try (spaces *> P.string ":PROPERTIES:" *> spaces *> endOfLine)
                     Map.fromList <$> P.manyTill
                       (
{-# LINE 840 "ccweb.org" #-}
                        do
                          p <- spaces *> (enclosed ':' <* spaces)
                          kvs <- 
{-# LINE 815 "ccweb.org" #-}
                                 line (Map.fromList
                                       <$> P.sepEndBy
                                        (
{-# LINE 823 "ccweb.org" #-}
                                         do
                                           k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                                           v <- parse :: Parser SExpression
                                           return (k, v)
{-# LINE 817 "ccweb.org" #-}
                                           )
                                        spaces1)
{-# LINE 843 "ccweb.org" #-}
                          return (p, kvs)
{-# LINE 834 "ccweb.org" #-}
                          )
                       (P.try (spaces *> P.string ":END:" *> spaces *> endOfLine))
{-# LINE 801 "ccweb.org" #-}
                     )
{-# LINE 848 "ccweb.org" #-}
fileMode :: SExpression -> F.FileMode
fileMode (Series (Atom "identity" : Atom ('#':'o':a:g:u:[]) : [])) =
  fromIntegral $ 8 * ((8 * oct a) + oct g) + oct u
  where oct c = ord c - ord '0'
fileMode _ = 420
{-# LINE 868 "ccweb.org" #-}
instance Parse Headline where
  parse = do
    s <- P.try (char '*') *> P.many (char '*')
    _ <- spaces1
    t <- parse :: Parser Text
    return $ Headline (length s) t
{-# LINE 878 "ccweb.org" #-}
headlineLevel :: Headline -> Int
headlineLevel (Headline x _) = x
{-# LINE 903 "ccweb.org" #-}
instance Parse SourceBlock where
  parse = do
    --parserTrace "SourceBlock: trying to find a code block here"
    n  <- P.optionMaybe (
{-# LINE 931 "ccweb.org" #-}
                         P.try (spaces *> P.string "#+NAME:")
                         *> spaces
                         *> parse :: Parser Text
{-# LINE 906 "ccweb.org" #-}
                                                             )
    i  <- P.try (spaces <* (P.string "#+BEGIN_SRC" *> spaces1))
    --parserTrace "SourceBlock: found a code block, parsing language"
    l  <- symbol <* spaces
    --parserTrace "SourceBlock: found a code block, parsing properties"
    ps <- 
{-# LINE 815 "ccweb.org" #-}
          line (Map.fromList
                <$> P.sepEndBy
                 (
{-# LINE 823 "ccweb.org" #-}
                  do
                    k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                    v <- parse :: Parser SExpression
                    return (k, v)
{-# LINE 817 "ccweb.org" #-}
                    )
                 spaces1)
{-# LINE 912 "ccweb.org" #-}
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
{-# LINE 943 "ccweb.org" #-}
emptyLine :: (Location s, P.Stream s m Char) => P.ParsecT s u m ()
emptyLine = void $ spaces *> endOfLine

{-# LINE 946 "ccweb.org" #-}
skipEmptyLines :: (Location s, P.Stream s m Char) => P.ParsecT s u m ()
skipEmptyLines = P.skipMany (P.try emptyLine)
{-# LINE 952 "ccweb.org" #-}
literalOr :: (Location s, P.Stream s m Char) => (String -> P.ParsecT s u m a) -> P.ParsecT s u m a -> P.ParsecT s u m a
literalOr f p = scan "" where
  scan [] = p <|> (anyChar >>= scan . (:""))
  scan acc =
    ((P.lookAhead p) *> (f $ reverse acc))
    <|> (anyChar >>= scan . (:acc))
    <|> (f $ reverse acc)
{-# LINE 967 "ccweb.org" #-}
enclosed :: (Location s, P.Stream s m Char) => Char -> P.ParsecT s u m String
enclosed d = P.try (P.char d) *> P.manyTill anyChar (P.char d)
{-# LINE 987 "ccweb.org" #-}
instance HeaderArgs Section where
  headerArgs = headerArgs . sectionDerivedProperties
{-# LINE 993 "ccweb.org" #-}
instance Parse Section where
  parse = do
    skipEmptyLines
    h <- P.optionMaybe parse :: Parser (Maybe Headline)
    --parserTrace $ PP.text "Section: headline=" PP.<> pretty h
    skipEmptyLines
    ps <- parse <|> return Map.empty :: Parser Properties
    --parserTrace $ PP.text "Section: properties=" PP.<> pretty ps
    when (isJust h) $ do
      P.updateState (\s ->
        s{ propertyStack =
             push (Map.union ps $ top $ propertyStack s)
             . resize (headlineLevel $ fromJust h)
             $ propertyStack s
         })
    P.updateState (\s -> s{ sectionNumber = 1 + (sectionNumber s) })
    ps' <- top . propertyStack <$> P.getState
    --parserTrace $ PP.text "Section: derived properties=" PP.<> pretty ps'
    skipEmptyLines
    --parserTrace "Section: about to scan for text and code"
    (ts, c) <- scan
    --parserTrace "Section: section parsed"
    state <- P.getState
    return Section
      { sectionHeadline = h
      , sectionProps = ps
      , sectionDerivedProperties = eval state ps'
      , sectionDocumentation = ts
      , sectionSourceBlock = c
      }
      where
        scan :: Parser ([Text], Maybe SourceBlock)
        scan = 
{-# LINE 1030 "ccweb.org" #-}
               do{ c <- parse :: Parser SourceBlock; return ([], Just c) }
               <|>
                 (do
                     P.skipMany1 (P.try emptyLine)
                     c <- P.optionMaybe parse :: Parser (Maybe SourceBlock)
                     return ([], c)
                 )
               <|>
                 (do
                     t <- parse :: Parser Text
                     s <- scan
                     return ( t:(fst s), snd s )
                 )
{-# LINE 1049 "ccweb.org" #-}
instance Pretty Keyword where
  pretty (AuthorKeyword a) = PP.text "Author:" PP.<+> pretty a
  pretty (PropertyKeyword a) = PP.text "Property:" PP.<+> pretty a
  pretty (TitleKeyword a) = PP.text "Title:" PP.<+> pretty a
  pretty (OtherKeyword a) = PP.text "Other:" PP.<+> pretty a
{-# LINE 1058 "ccweb.org" #-}
instance Parse Keyword where
  parse = propertyKeyword <|> titleKeyword <|> authorKeyword <|> otherKeyword
    where
      authorKeyword = do
        keywordStart "#+AUTHOR:"
        AuthorKeyword <$> parse
      propertyKeyword = do
        PropertyKeyword <$> 
{-# LINE 806 "ccweb.org" #-}
                            do
                              _ <- P.try (P.string "#+PROPERTY:") *> spaces
                              p <- symbol <* spaces1
                              kvs <- 
{-# LINE 815 "ccweb.org" #-}
                                     line (Map.fromList
                                           <$> P.sepEndBy
                                            (
{-# LINE 823 "ccweb.org" #-}
                                             do
                                               k <- symbol <* (spaces1 <|> (spaces *> P.char '=' *> spaces))
                                               v <- parse :: Parser SExpression
                                               return (k, v)
{-# LINE 817 "ccweb.org" #-}
                                               )
                                            spaces1)
{-# LINE 810 "ccweb.org" #-}
                              return $ Map.singleton p kvs
{-# LINE 1066 "ccweb.org" #-}
      titleKeyword = do
        keywordStart "#+TITLE:"
        TitleKeyword <$> parse
      otherKeyword = do
        _ <- P.try $ P.string "#+"
        t <- P.many anyChar
        _ <- endOfLine
        return $ OtherKeyword t
      keywordStart h = void $ P.try (P.string h *> spaces)
{-# LINE 1079 "ccweb.org" #-}
headerProperties :: [Keyword] -> Properties
headerProperties = foldl acc mempty where
    acc a (PropertyKeyword p) = Map.union p a
    acc a _ = a
{-# LINE 1098 "ccweb.org" #-}
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
{-# LINE 1115 "ccweb.org" #-}
readOrgFile :: FilePath -> IO (OrgFile, Document)
readOrgFile fp = do
  ingested <- fromList <$> ingest fp :: IO OrgFile
  h <- getHostName
  let doc = fromEither $ P.runParser
        (parse :: Parser Document)
        initialParserState{ hostName = h }
        fp
        ingested
  return (ingested, doc)
{-# LINE 225 "ccweb.org" #-}
data Options = Options
  { logLevel :: LogLevel
  , dryRun :: Bool
  , listInputFiles :: Bool
  , listOutputFiles :: Bool
  , inputFile :: String
  }

{-# LINE 233 "ccweb.org" #-}
instance Pretty Options where
  pretty o = pretty $ PrettyStruct "Options"
    [ ("log level", pretty $ logLevel o)
    , ("dry-run", pretty $ dryRun o)
    , ("list-input-files", pretty $ listInputFiles o)
    , ("list-output-files", pretty $ listOutputFiles o)
    , ("input file", pretty $ inputFile o)
    ]

{-# LINE 242 "ccweb.org" #-}
userOptionParser :: O.Parser Options
userOptionParser = Options
  <$> 
{-# LINE 253 "ccweb.org" #-}
      ((
{-# LINE 258 "ccweb.org" #-}
        (\vs -> case length vs of
                 0 -> Error
                 1 -> Warning
                 2 -> Info
                 _ -> Debug)
        <$>
        O.some
          ( O.flag' ()
            ( O.short 'v'
              <> O.long "verbose"
              <> O.help "Be verbose. Can be given multiple times for more verbosity."
            ))
{-# LINE 253 "ccweb.org" #-}
                                 ) <|> (
{-# LINE 274 "ccweb.org" #-}
                                        O.flag Warning Quiet
                                          ( O.short 'q'
                                            <> O.long "quiet"
                                            <> O.help "Only print things that were asked for."
                                          )
{-# LINE 253 "ccweb.org" #-}
                                                            ))
{-# LINE 245 "ccweb.org" #-}
  <*> dryRunParser
  <*> 
{-# LINE 300 "ccweb.org" #-}
      O.switch
        ( O.short 'I'
        <> O.long "list-input-files"
        <> O.help "If given, list the files that have to be read in, one per line."
        )
{-# LINE 247 "ccweb.org" #-}
  <*> 
{-# LINE 291 "ccweb.org" #-}
      O.switch
        ( O.short 'O'
        <> O.long "list-output-files"
        <> O.help "If given, list the files that would be written out, one per line."
        )
{-# LINE 248 "ccweb.org" #-}
  <*> (
{-# LINE 283 "ccweb.org" #-}
       O.argument O.str
         ( O.metavar "FILE"
         <> O.help "The name of the input file."
         )
{-# LINE 248 "ccweb.org" #-}
                       )
{-# LINE 309 "ccweb.org" #-}
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
  logM (logLevel opts) Debug "* Command line options:"
  logM (logLevel opts) Debug (PP.render (pretty opts))
  --let doc = parseDocument state (inputFile opts)
  --logM (logLevel opts) Debug $ PP.render (pretty doc)
  --putStrLn (weave doc)
  where
    optParser = O.info (userOptionParser <**> O.helper)
      ( O.fullDesc
      <> O.progDesc "Weave the input FILE"
      <> O.header "ccweave - A literate programming weaver" )

{-# LINE 25 "org/weave.org" #-}
--class Weave a where
--  weave :: a -> String
{-# LINE 31 "org/weave.org" #-}
--instance Weave Char where
--  weave '&' = "{\\AM}"
--  weave '\\' = "{\\BS}"
--  weave '{' = "{\\LB}"
--  weave '}' = "{\\RB}"
--  weave '~' = "{\\TL}"
--  weave '_' = "{\\UL}"
--  weave '^' = "{\\CF}"
--  weave '#' = "{\\#}"
--  weave '$' = "{\\$}"
--  weave '%' = "{\\%}"
--  weave c = [c]
{-# LINE 47 "org/weave.org" #-}
--instance Weave TextElement where
--  weave (Plain s) = s
--  weave (Bold s) = concat ["{\\bf ", s, "}"]
--  weave (Italics s) = concat ["{\\it ", s, "\\/}"]
--  weave (InlineCode s) = "\\hbox{\\tenex " ++ concatMap weave s ++ "}"
--  weave (Verbatim s) = "\\hbox{\\tenex " ++ concatMap weave s ++ "}"
{-# LINE 57 "org/weave.org" #-}
--instance Weave Text where
--  weave (Text ts) = concatMap weave ts
{-# LINE 66 "org/weave.org" #-}
--instance Weave Section where
--  weave s = unlines (header : text) ++ code ++ "\\fi\n"
--    where
--      text = map weave (sectionText s)
--      header = concat $
--        case groupHeader s of
--          Nothing -> [ "\\M{", show (sectionNumber s), "}"]
--          Just h ->
--            [ "\\N{", show (sectionDepth h), "}"
--            , "{", show (sectionNumber s), "}"
--            , weave (sectionHeaderTitle h), "."
--            ]
--      code = concat $
--        case sectionCode s of
--          Nothing -> []
--          Just _ -> [ "\\X", ":", "\\X", "${}", "\\equiv{}$\n" ]
{-# LINE 86 "org/weave.org" #-}
--instance Weave Document where
--  weave Document{documentHeaders = []} = ""
--  weave doc@Document{documentHeaders = _} =
--    unlines [ "\\input cwebmac"
--            , concatMap weave (documentSections doc)
--            , "\\inx"
--            , "\\fin"
--            , "\\con"
--            ]
{-# LINE 20 "org/weave.org" #-}