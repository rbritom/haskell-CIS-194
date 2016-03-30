{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 02
--
----------------------------------------------------------------------

module LogAnalysis where

import Log

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"
-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format"

parseMessage :: String -> LogMessage
parseMessage a  = parseMessage' (words a)
  where parseMessage' ("E":y:x:xs) = LogMessage (Error $ parseToInt y) (parseToInt x) (unwords xs)
        parseMessage' ("I":x:xs)   = LogMessage Info (parseToInt x) (unwords xs)
        parseMessage' ("W":x:xs)   = LogMessage Warning (parseToInt x) (unwords xs)
        parseMessage' (_:xs)       = Unknown (unwords xs)
        parseMessage' []           = Unknown ""

parseToInt :: String -> Int
parseToInt a = read a :: Int

parse :: String -> [LogMessage]
parse a = map parseMessage (lines a)

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>>
--

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage Leaf  = Node Leaf logMessage Leaf
insert logMessage@(LogMessage _ time _) (Node prevT current@(LogMessage _ currentTime _) nextT)
  | time < currentTime = Node (insert logMessage prevT) current nextT
  | time > currentTime = Node prevT current (insert logMessage nextT)
insert _ tree = tree

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>>
--

build :: [LogMessage] -> MessageTree
build []     = Leaf
build [x]    = insert x Leaf
build (x:xs) = insert x (build xs)

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

-- |
--
-- >>>
--

inOrder :: MessageTree -> [LogMessage]
inOrder t = (flattenTree t) []
  where flattenTree (Node left logMessage rigth) l = (flattenTree left (logMessage:(flattenTree rigth l)))
        flattenTree Leaf l = l

----

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

-- |
--
-- >>>
--

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong l = let severe (LogMessage (Error x) _ _) = (x > 50)
                      severe (LogMessage _ _ _) = False
                      severe (Unknown _) = False
                      extract (LogMessage _ _ message) = message
                      extract (Unknown message) = message
                  in map extract (filter severe l)

----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

whoDidIt :: String
whoDidIt = undefined
