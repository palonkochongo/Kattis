type Heap<'a when 'a: equality> =
         | EmptyHp
         | HP of 'a * Heap<'a> * Heap<'a>

let ex3 = HP(1,HP(2,HP(3,EmptyHp,EmptyHp),HP(5,EmptyHp,EmptyHp)),HP(4,EmptyHp,EmptyHp))
//The heap is a polymorphic type since it can take many different type as long as equality can be determined. 'a represent a generic type.

let emptVal = EmptyHp

exception HeapError of string

let isEmpty aHeap = function
        |EmptyHp -> true
        |_ -> false
         
         //naive implementation - not tailrecusive!
let rec size h acc = 
        match h with
        |EmptyHp -> acc
        |HP(_, a, b) -> 
                let acc' = size a acc
                size b (acc' + 1)

         //Tail recursive version using a continuation function
let rec sizeTailRecursive h acc k = 
        match h with
        |EmptyHp -> k acc
        |HP(_, a, b)-> sizeTailRecursive a ( acc+1 ) (fun res -> sizeTailRecursive b res k)
         
let find = function
        |EmptyHp -> None
        |HP(x, _, _) -> Some x
        
let rec chkHeapProperty ( h : Heap<'a> ) : bool when 'a : comparison = 
        match h with
        |EmptyHp -> true
        |HP(x, a, b) ->
                let a' = match find a with Some c -> c |None -> x 
                let b' = match find b with Some c -> c |None -> x 
                if a' >= x && b' >= x then
                        let chkA = chkHeapProperty a
                        let chkB = chkHeapProperty b
                        ( chkA && chkB )
                else false

let rec map f h =
        match h with
        |EmptyHp -> EmptyHp
        |HP(x, a, b) -> HP(f x, map f a, map f b)

let rec mapTail f h id=
        match h with
        |EmptyHp -> id EmptyHp
        |HP(x, a, b) -> mapTail f a (fun resa -> 
                mapTail f b (fun resb -> 
                        id(HP(f x, resa, resb))
                        )
                )

let aFunc x =
        if x = 1 then
                x + 10
        else 
                x

//Question 3

let triNum x =
        ( x * (x + 1) ) / 2

let seq = Seq.initInfinite triNum

let  triNumC = Seq.cache seq

let rec filterOddIndex s =
        Seq.delay ( fun () -> 
                Seq.append 
                        ( Seq.singleton (Seq.item 0 s) )
                        ( filterOddIndex (Seq.skip 2 s) ) 
        )

Seq.item 5 ( filterOddIndex triNumC );;

let rec seqZip s1 s2 =
        Seq.delay ( fun () -> 
                Seq.append ( 
                        Seq.singleton (
                                Seq.item 0 s1,
                                Seq.item 0 s2 
                        ) 
                )
                        ( seqZip (Seq.skip 1 s1) (Seq.skip 1 s2))
                )


let zipped = seqZip seq seq;;
Seq.item 2 zipped;;
