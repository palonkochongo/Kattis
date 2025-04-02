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
        |EmptyHp -> failwith "heap is empty"
        |HP(x, _, _) -> x
        
let rec chkHeapProperty h  : Heap<'a> -> bool when 'a : comparison = function
        |EmptyHp -> true
        |HP(x, a, b) ->
                let a' = find a
                let b' = find b
                if a' > x && b' > x then
                        let chkA = chkHeapProperty a
                        let chkB = chkHeapProperty b
                        ( chkA && chkB )
                else false
