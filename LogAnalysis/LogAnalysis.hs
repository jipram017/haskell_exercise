--{-# OPTIONS_GHC -Wall #-}

module Main where

import Log 

createInfoMessage :: [String] -> LogMessage
createInfoMessage (timestamp:content) = LogMessage Info (read timestamp) (unwords content)
createInfoMessage msg = error ("Invalid Info message: " ++ unwords msg)

createWarningMessage :: [String] -> LogMessage
createWarningMessage (timestamp:content) = LogMessage Warning (read timestamp) (unwords content)
createWarningMessage msg = error ("Invalid Warning message: " ++ unwords msg)

createErrorMessage :: [String] -> LogMessage
createErrorMessage (severity:timestamp:content) = LogMessage (Error (read severity)) (read timestamp) (unwords content)
createErrorMessage msg = error ("Invalid Error message: " ++ unwords msg)

createUnknownMessage :: [String] -> LogMessage
createUnknownMessage message = Unknown (unwords message)

-- Parse an individual message from the log file
parseMessage :: String -> LogMessage
parseMessage ('I':message) = createInfoMessage    $ words $ message
parseMessage ('W':message) = createWarningMessage $ words $ message
parseMessage ('E':message) = createErrorMessage   $ words $ message
parseMessage message       = createUnknownMessage $ words $ message

parseMessageTest :: Bool
parseMessageTest = and
  [
    LogMessage Info 29 "la la la" == parseMessage "I 29 la la la",
    LogMessage Warning 19 "le le le" == parseMessage "W 19 le le le",
    LogMessage (Error 2) 562 "help help" == parseMessage "E 2 562 help help",
    Unknown "Not in the right format" == parseMessage "Not in the right format"
  ]

-- Parse a whole log file
parse :: String -> [LogMessage]
parse file = parseMessage <$> lines file

-- Insert a new LogMessage into an existing MessageTree
insert::LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ ts1 _) (Node left msg2@(LogMessage _ ts2 _) right) 
    | ts1 < ts2 = Node (insert msg1 left) msg2 right
    | otherwise = Node left msg2 (insert msg1 right)

insertTest::Bool
insertTest = and
  [
      Node Leaf info Leaf == insert info Leaf,
      Node Leaf info (Node Leaf warning Leaf) == insert warning infoTree,
      Node (Node Leaf info Leaf) warning Leaf == insert info warningTree
  ]
  where
      info = LogMessage Info 30 "It does not matter"
      infoTree = Node Leaf info Leaf
      warning = LogMessage Warning 50 "It still does not matter"
      warningTree = Node Leaf warning Leaf

-- Build a complete MessageTree from a list of LogMessage
build::[LogMessage] -> MessageTree
build = foldr insert Leaf

buildTest::Bool
buildTest = and
  [
      Node Leaf info Leaf == build[info],
      Node (Node Leaf info Leaf) warning Leaf == build[info, warning],
      Node Leaf info (Node Leaf warning Leaf) == build[warning, info, unknown]
  ]
  where
      info = LogMessage Info 10 "It does not matter"
      warning = LogMessage Warning 20 "It still does not matter"
      unknown = Unknown "It does not matter"

-- Traverse a MessageTree in order
-- 1) Traverse left subtree
-- 2) Visit root
-- 3) Traverse right subtree
inOrder::MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = (inOrder left) ++ [root] ++ (inOrder right)

inOrderTest::Bool
inOrderTest = and
  [
      [info] == inOrder (Node Leaf info Leaf),
      [info, warning] == inOrder (Node Leaf info (Node Leaf warning Leaf))
  ]
  where
      info = LogMessage Info 10 "It does not matter"
      warning = LogMessage Warning 20 "It still does not matter"

-- Get content of LogMessage with Error severity >= 50 sorted by timestamp
isRelevant::LogMessage -> Bool
isRelevant (LogMessage(Error severity)_ _) = severity >= 50
isRelevant _                               = False

getContent::LogMessage -> String
getContent(LogMessage _ _  content) = content
getContent(Unknown content)         = content

whatWentWrong::[LogMessage] -> [String]
whatWentWrong xs = getContent <$> filter isRelevant (inOrder. build $ xs)

whatWentWrongTest::Bool
whatWentWrongTest = and
  [
      [] == whatWentWrong [info, warning, error1],
      ["Second error"] == whatWentWrong [info, warning, error2],
      ["Second error", "Third error"] == whatWentWrong [info, warning, error2, error3]
  ]
  where
      info = LogMessage Info 10 "This is normal condition"
      warning = LogMessage Warning 20 "This is still normal"
      error1 = LogMessage (Error 20) 1 "First error"
      error2 = LogMessage (Error 50) 2 "Second error"
      error3 = LogMessage (Error 60) 3 "Third error" 

main = do
    print parseMessageTest
    print insertTest
    print buildTest
    print inOrderTest
    print whatWentWrongTest
