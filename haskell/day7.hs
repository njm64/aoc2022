module Day7 where

import Data.List
import qualified Aoc

type Command = [String]
type Name = String
type Size = Int

data Node = 
    File Name Size | 
    Directory Name [Node]
    deriving (Show)

parse :: [String] -> [[String]]
parse = map words

-- Directory state is represented as zipper (a stack of nodes, 
-- with the current node at the top of the stack)
type DirState = [Node]

-- To change to a directory, we remove that directory from the node
-- at the head of the stack, and then push it onto the stack as the
-- new head.
changeDir :: DirState -> String -> DirState
changeDir ((Directory dirName dirNodes):rest) name =
    case findNode dirNodes name of
      Just n -> n : Directory dirName (delNode dirNodes name):rest
      Nothing -> error "Directory not found"

-- To change to a parent directory, we pop the current directory from
-- the stack (so the parent will be the new head), and re-add the
-- directory we just popped to the parent's list of children.
changeToParent :: DirState -> DirState
changeToParent [n] = [n]
changeToParent (n:(Directory name ns):rest) = Directory name (n:ns):rest
changeToParent _ = error "Invalid state"

-- To change to the root directory, we just keep changing to the parent
-- until there's only one node left in the stack.
changeToRoot :: DirState -> DirState
changeToRoot [d] = [d]
changeToRoot d = changeToRoot $ changeToParent d

addNode :: DirState -> Node -> DirState
addNode ((Directory dirName dirNodes):rest) node =
    case findNode dirNodes (nodeName node) of
      Just _ -> (Directory dirName dirNodes):rest
      Nothing -> (Directory dirName (node:dirNodes)):rest

nodeName :: Node -> String
nodeName (File name _) = name
nodeName (Directory name _) = name

-- Find a node with the given name
findNode :: [Node] -> String -> Maybe Node
findNode ns name = find (\n -> nodeName n == name) ns

-- Remove any nodes with the given name
delNode :: [Node] -> String -> [Node]
delNode ns name = filter (\n -> nodeName n /= name) ns

nodeSize :: Node -> Int
nodeSize (File _ size) = size
nodeSize (Directory _ nodes) = sum $ map nodeSize nodes

allDirs :: Node -> [Node]
allDirs n@(File _ _) = []
allDirs n@(Directory _ ns) = n : concatMap allDirs ns

update :: DirState -> Command -> DirState
update s cmd = 
    case cmd of
      ["$", "ls"] -> s
      ["$", "cd", "/"] -> changeToRoot s
      ["$", "cd", ".."] -> changeToParent s
      ["$", "cd", dir] -> changeDir s dir
      ["dir", name] -> addNode s (Directory name [])
      [size, name] -> addNode s (File name (read size))
      _ -> error "Unknown command"

makefs :: [Command] -> Node
makefs cmds =
    let s = [Directory "" []] in
    head . changeToRoot $ foldl' update s cmds

part1 cmds = 
    let sizes = map nodeSize . allDirs $ makefs cmds
    in sum . filter (< 100000) $ sizes

part2 cmds = 
    let fs = makefs cmds in
    let freeSpace = 70000000 - nodeSize fs in
    let requiredSpace = 30000000 - freeSpace in
    let sizes = map nodeSize (allDirs fs) in
    minimum $ filter (>= requiredSpace) sizes

run = Aoc.run 7 parse part1 part2


