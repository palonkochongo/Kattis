open System
open System.Text.RegularExpressions

type Vertex = double * double

type Edge = {
        weight : double ;
        fromV : Vertex
        toV : Vertex
}

type Adjecent = {
        vert : Vertex
        adjEdges : Edge List 
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
                fromV = (a,b)
                toV = (c,d) 
        }

let createAdjEdges lVert vert =
        let adjEdges =
                lVert
                |> List.filter (fun x -> x <> vert)
                |> List.map (createEdge vert)
        { vert = vert ; adjEdges = adjEdges }

let rec buildGraph l : Graph =
        List.map ( createAdjEdges l ) l

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

let edgeEquals e1 e2 =
    (e1.fromV = e2.fromV && e1.toV = e2.toV) ||
    (e1.fromV = e2.toV && e1.toV = e2.fromV)

let buildMST ( g:Graph ) =
        let searchV g v =
                match ( List.tryFind (fun ( x:Adjecent ) -> x.vert = v) g ) with
                |Some adj ->
                        let newGraph = List.filter (fun x -> x.vert <> v) g
                        (adj, newGraph)
                |None ->
                        ({vert = v ; adjEdges = []}, g)
        let rec helper g pq res visited =
                match remove pq with
                |None -> res
                |Some(min, pq') -> 
                        if ( Set.contains min.toV visited ) then
                                helper g pq' res visited
                        else
                                let (minAdj, newGraph) = searchV g min.toV
                                let unique = { 
                                        vert = min.toV ; 
                                        adjEdges = 
                                                minAdj.adjEdges 
                                                |> List.filter (fun x -> not (  List.exists (edgeEquals x ) res )) 
                                }
                                let pq'' = addAdjecent unique pq'
                                helper newGraph pq'' (min::res) (Set.add min.toV visited)


        let start = g.Head
        let pq' = addAdjecent start Leaf
        helper g pq' [] ( Set.singleton start.vert )

let totalBridgeLength l =
        List.foldBack (fun x acc -> x.weight + acc) l ( double 0.0 )

let rec readInput lines =
        let rec helper l acc =
                match l with
                |0 -> acc
                |_ ->
                        helper (l-1) ( Console.ReadLine()  :: acc )
        helper lines []
 
let toNumber ( l:list<string> ) =
        int l.Head

let formatInput l =
        let formatLine l =
                let reg = Regex @"\G(-?\d+\.\d+)\s(-?\d+\.\d+)"
                let m = reg.Match l
                if m.Success then
                        let x = double m.Groups.[1].Value
                        let y = double m.Groups.[2].Value
                        Some (x, y)
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
