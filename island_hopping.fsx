open System
open System.Text.RegularExpressions

type Vertex = double * double

type Edge = {
        weight : double ;
        fromV : int
        toV : int
}

type Adjecent = {
        adjEdges : Edge List 
        marked : bool
}

type Graph =
        Adjecent[]

type EdgePQ =
        |Leaf
        |Node of EdgePQ * Edge * EdgePQ
type PQStats = {
        pq : EdgePQ
        last : Vertex
}

let createEdge ((a,b), i) ((c,d), i') =
        let x = if a<c then c - a else a - c
        let y = if b<d then d - b else b - d
        let weight = sqrt( x*x + y*y )
        { 
                weight = weight
                fromV = i
                toV = i'
        }

let createAdjEdges lVert ((v, i) as vert ) =
        let adjEdges =
                lVert
                |> List.filter (fun (_, x ) -> x <> i)
                |> List.map (createEdge vert)
        { adjEdges = adjEdges ; marked = false }

let rec buildGraph l : Graph =
        Array.init ( List.length l ) (fun i -> createAdjEdges l l[i])


let getNode pq =
        match pq with
        |Leaf -> None
        |Node(_, n, _) -> Some n

let rec merge h1 h2 = 
        match h1, h2 with
        | Leaf, h | h, Leaf -> h
        | Node(l1, e1, r1), Node(l2, e2, r2) ->
                if e1.weight <= e2.weight then
                    Node(merge r1 h2, e1, l1)
                else
                    Node(merge r2 h1, e2, l2)

let addEdge1 (e:Edge) (pq:EdgePQ) =
        let node = Node(Leaf, e, Leaf)
        merge node pq


let addAdjecent ( a:Adjecent ) ( pq:EdgePQ ) =
        let rec helper al pq =
                match al with
                |[] -> pq
                |x::xs -> 
                        let pq' = addEdge1 x pq
                        helper xs pq'
        helper a.adjEdges pq

let remove = function
        |Leaf -> None
        |Node(left, min, right) ->
                let pq = merge left right
                Some (min, pq)

let buildMST ( g:Graph ) =
        let rec helper ( g:Graph ) pq res =
                match remove pq with
                |None -> res
                |Some(min, pq') -> 
                        if g[min.toV].marked = true then
                                helper g pq' res
                        else
                                g[min.toV] <- { g[min.toV] with adjEdges = List.filter (fun x -> not g[x.toV].marked) g[min.toV].adjEdges ; marked = true }
                                let pq'' = addAdjecent g[min.toV] pq'
                                helper g pq'' (min::res)
        let pq' = addAdjecent g[0] Leaf
        g[0] <- { g[0] with marked = true }
        helper g pq' []

let totalBridgeLength l =
        List.foldBack (fun x acc -> x.weight + acc) l ( double 0.0 )

let rec readInput lines =
        let rec helper l acc =
                match l with
                |0 -> acc
                |_ ->
                        helper (l-1) ( (Console.ReadLine(), acc.Length)  :: acc )
        List.rev ( helper lines [] )
 
let toNumber ( l:list<string * int> ) =
        let (s, i) = l.Head
        int s

let formatInput l =
        let formatLine ( s, i ) =
                let reg = Regex @"\G(-?\d+\.\d+)\s(-?\d+\.\d+)"
                let m = reg.Match s
                if m.Success then
                        let x = double m.Groups.[1].Value
                        let y = double m.Groups.[2].Value
                        Some ( (x, y), i )
                else None
        l |> List.choose formatLine


let rec loadGraph acc n =
        match n with
        |0 -> acc
        |_ ->
                let length =
                        1 
                        |> readInput 
                        |> toNumber
                        |> readInput
                        |> formatInput
                        |> buildGraph
                        |> buildMST
                        |> totalBridgeLength
                loadGraph ( length::acc ) (n-1)

let printResult l = 
        l |> List.iter (fun x -> printfn "%A" x) 

1 |> readInput |> toNumber |> loadGraph [] |> List.rev |> printResult

(*
let lines = [
        "0.0 0.0", 0 ;
        "0.0 1.0", 1 ;
        "1.0 0.0", 2 
]
let g = lines |> formatInput |> buildGraph
let mst = g |> buildMST
*)
