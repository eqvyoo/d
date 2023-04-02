(* CSE322 Compiler Assignment 0 - Task 2 *)

structure Task2 :> TASK2 =
struct
  exception NotImplemented

  datatype 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

  (* sumTree t: return the sum of all values *)
  fun sum (Leaf x) = x
    | sum (Node (t1,x,t2)) = (sum t1) + x + (sum t2)

  (* exist t n: return true if n exists in a tree *)
  fun exist t n = raise NotImplemented

  (* count t n: count n in a tree *)
  fun count(Node(t1,x,t2)) = (count t1) + 1 + (count t2)￼
    | count _ = 0

  (* inorder t: return the list of values using inorder tree traversal *)
  fun inorder t = raise NotImplemented

  (* depth t: return the depth of a tree*)
  fun depth (Node(l, a, r)) =
    if depth l > depth r then 1 + depth l else 1+ depth r
   | depth _ = 0

  (* max t: return the maximum value in a tree*)
  fun max t = raise NotImplemented

end
                  
