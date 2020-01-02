{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module QuizParser
  ( CMarkOption
  , optSourcePos, optNormalize, optHardBreaks, optSmart, optSafe, optUnsafe
  , standardOptions
  , parseQuiz, parseQuizWith
  , toHtml, toHtmlWith
  ) where
import CMark
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.List (foldl')
import Data.Text (Text, pack, unpack, strip)
import qualified Data.Text as Text
import Model.Quiz

parseQuiz :: Text -> Maybe Quiz
parseQuiz = parseQuizWith standardOptions

parseQuizWith :: [CMarkOption] -> Text -> Maybe Quiz
parseQuizWith opts md = runP nodes $ do
    name <- parseName
    desc <- parseDesc
    qs <- parseQuestions
    return $ Quiz
      { name = name
      , description = desc
      , questions = qs
      }
  where
    Node _ t nodes = commonmarkToNode opts md

toHtml :: Text -> Text
toHtml = toHtmlWith standardOptions

toHtmlWith :: [CMarkOption] -> Text -> Text
toHtmlWith = commonmarkToHtml

standardOptions :: [CMarkOption]
standardOptions =
  [ optNormalize
  , optSmart
  , optSafe
  ]

newtype P a = P { unP :: StateT [Node] Maybe a }
  deriving (Functor, Applicative, Monad, Alternative)

runP :: [Node] -> P a -> Maybe a
runP ns (P m) =
  case runStateT m ns of
    Just (x, []) -> Just x
    Just _       -> Nothing
    Nothing      -> Nothing

nest :: [Node] -> P a -> P a
nest ns m = do
  old_ns <- P get
  P (put ns)
  x <- m
  P (put old_ns)
  return x

die :: Text -> P a
die _ = P $ lift $ Nothing

typeOf :: Node -> NodeType
typeOf (Node _ t _) = t

hasType :: Node -> NodeType -> Bool
hasType n t = typeOf n == t

isHeading :: Node -> Bool
isHeading n = case typeOf n of
  HEADING _ -> True
  _         -> False

isList :: Node -> Bool
isList n = case typeOf n of
  LIST _ -> True
  _      -> False

peek :: P Node
peek = do
  nodes <- P get
  case nodes of
    []    -> die "peek: eof"
    (n:_) -> return n

node :: P Node
node = do
  nodes <- P get
  case nodes of
    (n:ns) -> P (put ns) >> return n
    _      -> die "node: eof"

toMD :: [Node] -> P Text
toMD [n] = return $ strip $ nodeToCommonmark standardOptions Nothing n
toMD ns  = return $ strip $ nodeToCommonmark standardOptions Nothing $ Node Nothing DOCUMENT ns

nodesWhile :: (Node -> Bool) -> P [Node]
nodesWhile p = do
  nodes <- P get
  case break (not . p) nodes of
    (ns, rest) -> P (put rest) >> return ns

parseName :: P Text
parseName = do
  n@(Node _ t ns) <- node
  unless (isHeading n) $ do
    die "Quiz must start with a heading."
  toMD ns

parseDesc :: P Text
parseDesc = nodesWhile (not . isHeading) >>= toMD

many1 :: (Monad f, Alternative f) => f a -> f [a]
many1 p = do
  xs <- many p
  if null xs
    then empty
    else return xs

parseQuestions :: P [Question]
parseQuestions = many1 parseQuestion

parseQuestion :: P Question
parseQuestion = do
  hd <- parseQuestionHead
  q <- parseQuestionText
  as <- parseAlts
  return $ Question
    { title = hd
    , question = q
    , alts = as
    }

parseQuestionHead :: P Text
parseQuestionHead = do
  n@(Node _ t ns) <- node
  unless (isHeading n) $ do
    die "All questions must start with a heading."
  toMD ns

parseQuestionText :: P Text
parseQuestionText = nodesWhile (not . isList) >>= toMD

parseAlts :: P [Alt]
parseAlts = do
  n@(Node _ t ns) <- node
  unless (isList n) $ do
    die "Question does not have any alternatives."
  nest ns $ many parseAlt

parseAlt :: P Alt
parseAlt = do
    n@(Node _ t ns) <- node
    unless (t == ITEM) $ do
      die "All alternatives must be list items."
    alt <- toMD ns
    return $ Alt
      { alternative = stripMarkers alt
      , correct = isCorrect (Text.toLower alt)
      }
  where
    correctPrefixes = ["\\!", "->", "(correct)"]
    correctSuffixes = ["<-", "(correct)"]
    correctBrackets = [("\\[", "\\]"), ("<", ">"), (">", "<"), ("(", ")"), ("{", "}")]
    isSurroundedBy s (pre, post) =
      pre `Text.isPrefixOf` s && post `Text.isSuffixOf` s
    stripSurrounding (pre, suf) s
      | s `isSurroundedBy` (pre, suf) = tryStripPrefix pre $ tryStripSuffix suf s
      | otherwise                     = s
    tryStripPrefix pre s = maybe s id (Text.stripPrefix pre s)
    tryStripSuffix suf s = maybe s id (Text.stripSuffix suf s)
    isCorrect s = or
      [ any (`Text.isPrefixOf` s) correctPrefixes
      , any (`Text.isSuffixOf` s) correctSuffixes
      , any (s `isSurroundedBy`) correctBrackets
      ]
    stripBackslash = tryStripPrefix "\\" . tryStripSuffix "\\"
    stripMarkers = strip . stripBackslash . foldl' (.) id
      [ flip (foldl' (flip stripSurrounding)) correctBrackets
      , flip (foldl' (flip tryStripSuffix)) correctSuffixes
      , flip (foldl' (flip tryStripPrefix)) correctPrefixes
      ]
