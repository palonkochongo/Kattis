type Vertex = double * double

type Edge = {
        weight : double ;
        v : Vertex ;
        w : Vertex
}

type Adjecent = {
        v : Vertex
        a : Edge List 
}

type Graph =
        Adjecent List

type EdgePQ =
        |Leaf
        |Node of EdgePQ * Edge * EdgePQ
type PQStats = {
        pq : EdgePQ
        last : Vertex
}

let createEdge ((a,b):Vertex) ((c,d):Vertex) =
        let x = if a<c then c - a else a - c
        let y = if b<d then d - b else b - d
        let weight = sqrt( x*x + y*y )
        { 
                weight = weight
                v = (a,b)
                w = (c,d) 
        }

let rec createAdjecent lVert acc vert = 
        match lVert with
        |[] -> 
                 {
                        v = vert
                        a = acc
                }
        |x::xs -> 
                if x = vert then 
                        createAdjecent xs acc vert 
                else
                        let edge = createEdge x vert
                        createAdjecent xs (edge::acc) vert
        
let rec buildGraph l : Graph =
        List.map ( createAdjecent l [] ) l

let getNode pq =
        match pq with
        |Leaf -> None
        |Node(_, n, _) -> Some n

let rec addEdge pq e = 
        match pq with
        |Leaf -> 
                Node(Leaf, e, Leaf)
        |Node(Leaf, n, right) ->
                if e.weight < n.weight then
                        Node(addEdge Leaf n, e, right)
                elif e.weight < n.weight then
                        Node(addEdge Leaf e, n, right)
                else 
                        Node(Leaf, n, right)
        |Node(left, n, right) ->
                if e.weight < n.weight then
                        Node(left, e, addEdge right n)
                elif e.weight < n.weight then
                        Node(left, n, addEdge right e)
                else
                        Node(left, n, right)

let rec merge h1 h2 = 
        match h1, h2 with
        | Leaf, h | h, Leaf -> h
        | Node(l1, e1, r1), Node(l2, e2, r2) ->
                if e1.weight <= e2.weight then
                    Node(merge r1 h2, e1, l1)
                else
                    Node(merge r2 h1, e2, l2)


let rec buildMinEdgePQ ( g:Graph ) ( pq:EdgePQ ) =
        let rec helper e pq =
                match e with
                |[] -> pq
                |x::xs -> 
                        let pq' = addEdge pq x
                        helper xs pq'
        match g with
        |[] -> pq
        |x::xs -> 
                let pq' = helper x.a pq
                buildMinEdgePQ xs pq'

let rec exchange pq =
        let rec getLast pq n =
                match pq with
                |Leaf -> None
                |Node(left, n, right) ->
                        match getNode left, getNode right with
                        |Node(_, nl, _),Node(_, nr, _) ->
                                if nl < nr then
                                        Node(helper nl n,
                        
        let min = getNode pq

let remove = function

        |Leaf -> None
        |Node(left, n, right) ->
                match getNode left, getNode right with
                |Node(_, nl, _),Node(_, nr, _) ->
                        if nl < nr then

(*
                |Node(left, n, Leaf) ->
                        if x.weight < n.weight then
                                Node(addEdge left x, n, Leaf)
                        elif x.weight > n.weight then
                                Node(addEdge left n, x, Leaf)
                        else Node(left, n, Leaf)
                |Node(Leaf, n, right) ->
                        if x.weight < n.weight then
                                Node(Leaf, n, addEdge right x)
                        elif x.weight > n.weight then
                                Node(Leaf, x, addEdge right n)
                        else Node(left, n, Leaf)
*)
