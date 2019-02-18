{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Log

-- Exercise 1
-- Parse a single message
-- This seems to be the solution that most people have implemented, however it does not account for if the log is not correclty formatted (i.e. ts is not an Int)
-- However, it seems beyond the scope of the course at this point to use something like `readMaybe`, so I will implement like this for now
parseMessage :: String -> LogMessage
parseMessage logMessage =
    case words logMessage of
        ("I":ts:msg) ->
            LogMessage Info (read ts) (unwords msg)

        ("W":ts:msg) ->
            LogMessage Warning (read ts) (unwords msg)

        ("E":severity:ts:msg) ->
            LogMessage (Error (read severity)) (read ts) (unwords msg)
        _ ->
            Unknown logMessage


-- Parse a log file
parse :: String -> [LogMessage]
parse logFile = map parseMessage (lines logFile)


-- Exercise 2
-- Left is less, right is more
insert :: LogMessage -> MessageTree -> MessageTree
insert logMessage@LogMessage{} Leaf = Node Leaf logMessage Leaf   -- suggested by hlint
-- insert logMessage@(LogMessage _ _ _) Leaf = Node Leaf logMessage Leaf
insert logMessage@(LogMessage _ newTs _) (Node leftSubTree rootMessage@(LogMessage _ originalTs _) rightSubTree) =
    if newTs < originalTs then
        Node (insert logMessage leftSubTree) rootMessage rightSubTree
    else
        Node leftSubTree rootMessage (insert logMessage rightSubTree)
insert _ root = root


-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftSubTree logMessage rightSubTree) = inOrder leftSubTree ++[logMessage] ++ inOrder rightSubTree


-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = foldr
        (\logMessage msgList ->
            case logMessage of
                LogMessage (Error severity) _ msg ->
                    if severity >= 50 then
                        msg:msgList

                    else
                        msgList

                _ ->
                    msgList
        )
        [] . (inOrder . build )
