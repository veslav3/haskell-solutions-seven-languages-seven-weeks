module Maze where
    import Data.List
    import Control.Monad

    data Node = Node { xcoord::Int,
        ycoord::Int,
        neighbours::[Node],
        is_finish::Bool,
        name::String
    } deriving (Show,Eq)

    n1 = Node{ xcoord=0, ycoord=0,
               neighbours=[n2], is_finish=False,
               name="n1" }
    n2 = Node{ xcoord=1, ycoord=0,
               neighbours=[n3], is_finish=False,
               name="n2" }
    n3 = Node{ xcoord=0, ycoord=1,
               neighbours=[n4], is_finish=False,
               name="n3" }
    n4 = Node{ xcoord=1, ycoord=1,
               neighbours=[], is_finish=True,
               name="n4" }
    n5 = Node{ xcoord=1, ycoord=2,
               neighbours=[n4,n3], is_finish=False,
               name="n5" }
    n6 = Node{ xcoord=2, ycoord=2,
               neighbours=[], is_finish=False,
               name="n6" }

    all_exits node acc
       | neighbours node == [] = [node]
       | otherwise =
           do n <- neighbours node;
                   let { nextLot = all_exits n acc };
                       (node:nextLot)

    solve start_node =
      let (pathNodes, finishNodes) = break (\ n -> is_finish n == True ) $ all_exits start_node []
      in
          if length finishNodes < 1 then
              []
          else
              pathNodes++[head finishNodes]
