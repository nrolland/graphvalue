import Protolude
import AG
import Dag.AG
import Dag.Render
import System.FilePath  ((</>))
import System.Directory (getTemporaryDirectory)
import System.Process   (system)
import System.IO.Unsafe (unsafePerformIO )
import           Control.Applicative
import           Control.Monad
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Traversable    (Traversable (..))
import           System.IO.Unsafe

import           Data.Foldable       hiding (fold)

import           AG
import           Dag.AG

import           Dag.Render

import           Data.Constraint
import Protolude hiding ((&), fold)

instance ShowConstr IntTreeF
  where
    showConstr (Leaf i)     = "Leaf " ++ show i
    showConstr (Node _ _)   = "Node "


data IntTreeF a = Leaf Int | Node a a
  deriving (Eq, Show)

iNode x y = In (Node x y)
iLeaf i = In (Leaf i)

instance Foldable IntTreeF where
    foldr _ z (Leaf _) = z
    foldr f z (Node x y) = x `f` (y `f` z)

instance Functor IntTreeF where
    fmap _ (Leaf i) = Leaf i
    fmap f (Node x y) = Node (f x) (f y)

instance Traversable IntTreeF where
    mapM _ (Leaf i) = return (Leaf i)
    mapM f (Node x y) = liftM2 Node (f x) (f y)

    traverse _ (Leaf i) = pure (Leaf i)
    traverse f (Node x y) = liftA2 Node (f x) (f y)


graph1 :: Tree IntTreeF
graph1 = iNode (iNode (iLeaf 2) a) b
           where a = iNode (iNode (iLeaf 2)(iLeaf 2)) (iLeaf 2)
                 b = a

-- graph1' = mkDag (Node (Node (Leaf 2) (Ref 0)) (Ref 0)) [(0, Node (Node (Leaf 2) (Leaf 2)) (Leaf 2))]

graph2 :: Tree IntTreeF
graph2 =  iNode (iNode (iLeaf 2) a) b
           where a = iNode (iNode (iLeaf 2) (iLeaf 2)) (iLeaf 2)
                 b = iNode (iNode (iLeaf 2) (iLeaf 2)) (iLeaf 2)


main :: IO ()
main = do putStrLn ("building assets" :: Text)
          renderDag2 (unsafePerformIO $ reifyDag graph1) "./static/graph1"
          renderDag2 (unsafePerformIO $ reifyDag graph2) "./static/graph2"
          writeFile  "./static/graph1.txt"  (show . unsafePerformIO $ reifyDag graph1) 


renderDag2 ::  (ShowConstr f, Traversable f) =>  Dag f -> Text -> IO ()
renderDag2 dag fn = do
   tmpd <- getTemporaryDirectory
   let tmpf = tmpd </> "523452345234"
   renderDag dag tmpf
   system $ "dot -Tsvg " ++ tmpf ++ " -o " ++ toS fn ++ ".svg"
   return ()

