type BinTree<'a> =
        |Leaf
        |Node of BinTree<'a> * 'a *  BinTree<'a>

        //Naive recursive implementation
let rec countBinTree t acc =
        match t with
        |Leaf -> acc
        |Node(b1, n, b2) -> 
                let acc' = countBinTree b1 acc
                let acc'' = acc' + 1
                countBinTree b2 acc'' 

        //EX 8.1 Implementation with continuation functions
let rec countBinTreeTail t a =
        match t with
        |Leaf -> a 0
        |Node(t1, n, t2) ->
                countBinTreeTail t1 (fun t1a -> countBinTreeTail t2 (fun t2a -> a (t1a+t2a+1)))

        //EX 8.2 Implementation with continuation function + accumulationj
let rec countBinTreeTail2 t i a =
        match t with
        |Leaf -> a i
        |Node(t1, n, t2) ->
                countBinTreeTail2 t1 (i+1) (fun res -> countBinTreeTail2 t2 res a)

        //EX 8.3
//This causes stack overflow with this call: bigListK 130000 i
let rec bigListK n k =
        if n=0 then k []
        else bigListK (n-1) (fun res -> 1::k(res));;
        //Problem:      The evalulation og the cons if waiting for the call of
        //              k(res) to return. since it will only the return when n=0
        //              the stack grows until then. On the other hand if the cons
        //              operation is passed along with k the function become tail
        //              recursive ass the evaluation of the cons operation is delayed
        //              through being passed along in a function.

bigListK 130000 id;;

let rec bigListKcorrected n k =
        if n=0 then k []
        else bigListKcorrected (n-1) (fun res -> k(1::res))

//EX 8.4
let rec leftTree n k =
        match n with
        |0 -> k Leaf
        |n -> leftTree (n-1) (fun res -> k (Node(res, n, Leaf)))

let rec rightTree n k =
        match n with
        |0 -> k Leaf
        |n -> rightTree (n-1) (fun res -> k (Node(res, n, Leaf)))

countBinTree(leftTree 3000000 id);
