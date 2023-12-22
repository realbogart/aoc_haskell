{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -Wno-redundant-bang-patterns #-}

module Y2023.D8 where

import AoC
import Data.HashMap.Strict qualified as HMS

default (Int, Text)

partOneTests = [("RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)", 2), ("LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)", 6)]

partTwoTests = [("LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)", 6)]

type Instructions = [Char]
type NodeID = [Char]

data Node = Node
  { source :: NodeID
  , left :: NodeID
  , right :: NodeID
  } deriving (Show)

source node = node.source

data NodeNetwork = NodeNetwork
  { instructions :: Instructions
  , nodes :: [Node]
  } deriving (Show)

type NodeConnections = HMS.HashMap NodeID (NodeID, NodeID)

parseInput :: Parser NodeNetwork
parseInput = do
  instructions <- parseInstructions
  NodeNetwork instructions <$> parseNodes
  where parseInstructions :: Parser Instructions
        parseInstructions = some (char 'L' <|> char 'R') <* space
        parseNodes = parseLineSeparated parseNode
        parseNode :: Parser Node
        parseNode = do
          source_id <- parseNodeID
          _ <- space <* char '=' <* space <* char '('
          left_id <- parseNodeID
          _ <- char ',' <* space
          right_id <- parseNodeID <* char ')'
          return $ Node source_id left_id right_id
        parseNodeID = count 3 (letterChar <|> digitChar)

getNodeConnections :: NodeNetwork -> NodeConnections
getNodeConnections nn = HMS.fromList (map (\node -> (node.source, (node.left, node.right))) nn.nodes)

step :: NodeConnections -> Char -> NodeID -> NodeID
step ncs dir nid = if dir == 'L' then l else r
  where (l, r) = ncs HMS.! nid

countSteps :: NodeConnections -> Instructions -> NodeID -> NodeID -> Int -> Int
countSteps ncs is from to acc | next_node == to = acc + 1
                              | otherwise = countSteps ncs (tail is) next_node to (acc + 1)
  where next_instruction = head is
        next_node = step ncs next_instruction from 

countSteps2 :: NodeConnections -> Instructions -> [NodeID] -> Int -> Int
countSteps2 ncs is from acc | done = next_acc
                            | otherwise = trace (show is_done) $ countSteps2 ncs future_instructions next_nodes next_acc
  where next_instruction = head is
        future_instructions = tail is
        next_nodes = map (step ncs next_instruction) from
        next_acc = acc + 1
        -- num_done = length $ filter endNode next_nodes
        is_done = map endNode next_nodes
        done = all endNode next_nodes
        endNode [_,_,'Z'] = True
        endNode _ = False

partOne :: NodeNetwork -> Int
partOne nn = countSteps ncs instructions "AAA" "ZZZ" 0
  where ncs = getNodeConnections nn
        instructions = cycle nn.instructions

partTwo :: NodeNetwork -> Int
partTwo nn = countSteps2 ncs instructions start_nodes 0
  where ncs = getNodeConnections nn
        instructions = cycle nn.instructions
        start_nodes = (filter start_node . map source) nn.nodes
        start_node [_,_,'A'] = True
        start_node _ = False
