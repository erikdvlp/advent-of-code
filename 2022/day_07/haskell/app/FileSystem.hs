module FileSystem where

type FileName = String
type FileSize = Int
type ChildIndex = Int
type Path = [ChildIndex]
type Children = [FileNode]

data FileNode = File { fileName :: FileName, fileSize :: FileSize }
    | Directory { fileName :: FileName, fileSize :: FileSize, children :: Children }

-- Attaches a file node to another file node as a new child.
attachChild :: FileNode -> FileNode -> FileNode
attachChild parent child = Directory (fileName parent) (fileSize parent + fileSize child) (children parent ++ [child])

-- Replaces a file node's child at an index with a file node.
replaceChildAtIndex :: FileNode -> ChildIndex -> FileNode -> FileNode
replaceChildAtIndex parent index newChild = Directory (fileName parent) newSize (replaceAtIndex (children parent) index newChild)
    where
        newSize = fileSize parent - fileSize (children parent !! index) + fileSize newChild
        replaceAtIndex elems index newElem = take index elems ++ [newElem] ++ drop (index+1) elems

-- Traverses a file node along a path and adds a file node as a new child.
addChildRecur :: FileNode -> Path -> FileNode -> FileNode
addChildRecur parent [] child = attachChild parent child
addChildRecur parent (x:xs) child = replaceChildAtIndex parent x $ addChildRecur (children parent !! x) xs child

-- Finds a child's position in a set of children by a name.
findChildPositionByName :: Children -> FileName -> ChildIndex -> ChildIndex
findChildPositionByName (x:xs) childName position
    | fileName x == childName = position
    | otherwise = findChildPositionByName xs childName (position + 1)

-- Steps one level up in a path by removing the last step.
stepUp :: Path -> Path
stepUp = init

-- Traverses a file node along a path and steps one level down in a path by adding the next step.
stepDown :: Path -> FileNode -> FileName -> Path
stepDown path root childName = path ++ [findChildPositionByName (children parent) childName 0]
    where
        parent = getFileNodeAtPath path root
        getFileNodeAtPath xs root = foldl (\ root x -> children root !! x) root xs

-- Traverses a file node using DFS and sums its directory sizes that are <= 100K for part 1 of the problem.
sumDirsBelow100K :: FileSize -> FileNode -> FileSize
sumDirsBelow100K sizeSum (Directory _ _ []) = sizeSum
sumDirsBelow100K sizeSum (Directory _ dirSize children) = sizeSum + sizeBelow100K + sum (map (sumDirsBelow100K sizeSum) children)
    where
        sizeBelow100K = if dirSize <= 100000 then dirSize else 0
sumDirsBelow100K sizeSum _ = sizeSum

-- Traverses a file node using DFS and gets the smallest directory whose deletion would free enough space for part 2 of the problem.
dirToDeleteSize :: FileSize -> FileSize -> FileNode -> FileSize
dirToDeleteSize spaceNeeded bestDirSize (Directory _ _ []) = bestDirSize
dirToDeleteSize spaceNeeded bestDirSize (Directory _ dirSize children) = minimum (map (dirToDeleteSize spaceNeeded newBestDirSize) children)
    where
        newBestDirSize = if dirSize >= spaceNeeded && dirSize < bestDirSize then dirSize else bestDirSize
dirToDeleteSize _ bestDirSize _ = bestDirSize
