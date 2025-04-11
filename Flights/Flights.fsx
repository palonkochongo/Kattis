open System
open System.Text.RegularExpressions

type Info = { Time:int ; Dest:string }
type FlightSched =
        |Empty
        |Flight of FlightSched * Info * FlightSched
type Operation =
        |Destination of Info
        |Cancel of int
        |Reroute of Info
        |Delay of int * int

let inputToInfo (s:string) :Info =
        let re = Regex @"\G(\d\d):(\d\d):(\d\d)\s(\S*)$"
        let m1 = re.Match s
        if not m1.Success then
                failwith "invalid intput string!"
        let hour = int m1.Groups.[1].Value * 60 * 60
        let min = int m1.Groups.[2].Value * 60
        let sec = int m1.Groups.[3].Value
        let timeInSec = hour + min + sec
        { Time = timeInSec ; Dest = m1.Groups.[4].Value }

let prsOperation ( s:string ) : Operation =
        let re = Regex @"(\S*)\s(\d\d):(\d\d):(\d\d)\s?(\S*|\d+)?$"
        let m = re.Match s
        if not m.Success then
                failwith "invalid intput string!"
        let hour = int m.Groups.[2].Value * 60 * 60
        let min = int m.Groups.[3].Value * 60
        let sec = int m.Groups.[4].Value
        let timeInSec = hour + min + sec
        let opr = m.Groups.[1].Value
        match opr with
        |"destination" -> Destination {Time = timeInSec ; Dest = opr}
        |"cancel" -> Cancel timeInSec
        |"delay" ->
                let delay = int m.Groups.[5].Value
                printf "%d\n" hour
                printf "%d\n" min
                printf "%d\n" sec
                Delay (timeInSec, delay)
        |"reroute" -> Reroute {Time = timeInSec ; Dest = m.Groups.[5].Value}
        |_ -> failwith "not a valid operation!"


let rec buildTimeTable f i = 
        match f with
        |Empty -> Flight(Empty, i, Empty)
        |Flight(left, n, right) ->
                if i.Time < n.Time then
                        Flight( buildTimeTable left i, n, right )
                else
                        Flight( left , n, buildTimeTable right i )


let rec readAndBuild n f =
        match n with
        |0 -> f
        |_ -> 
                Console.ReadLine() |>
                inputToInfo |>
                buildTimeTable f |>
                readAndBuild (n-1)

let rec destination s f =
        match f with
        |Empty -> None
        |Flight(left, n, right) -> 
                if s = n.Time then
                        Some n
                elif s < n.Time then
                        destination s left
                else
                        destination s right

let rec reroute (s:Info) (f:FlightSched) =
        match f with
        |Empty -> Empty
        |Flight(left, n, right) -> 
                if s.Time = n.Time then
                        Flight(left, { n with Dest = s.Dest }, right)
                elif s.Time > n.Time then
                        Flight(reroute s left, n, right)
                else
                        Flight(left, n, reroute s right)

let rec delMin f =
        match f with
        |Empty -> Empty
        |Flight(left, n, right) ->
                match left with
                | Empty -> right  
                | _ -> delMin left

let rec cancel (s:int) (f:FlightSched) =
        match f with
        |Empty -> Empty
        |Flight(left, n, right) ->
                if s = n.Time then
                        match left, right with
                        |Empty, Empty -> Empty
                        |Empty,_ -> right
                        |_, Empty -> left
                        |_,_ ->
                                let rec findMin (f:FlightSched) =
                                        match f with
                                        |Empty -> failwith "This tree shouldn't be empty."
                                        |Flight(Empty , minNode, _) -> minNode
                                        |Flight(left, _, _) -> findMin left
                                let minRight = findMin right
                                let newRight = delMin right
                                Flight(left, minRight, newRight)
                elif s < n.Time then
                        cancel s left
                else 
                        cancel s right


let rec find s f =
        match f with
        |Empty -> failwith "not found!"
        |Flight(left, n, right) ->
                printf "Checking flight at time %d\n with time %d" n.Time s// Debugging 
                if s = n.Time then
                        n
                elif s < n.Time then
                        find s left
                else
                        find s right

let rec readAndOperate n f =
        let ot = Console.ReadLine()
        printf "%s\n" ot
        let o = prsOperation ot
        match n with
        |0 -> f
        |_ -> 
                match o with
                |Destination(x) ->
                        let res = destination x.Time f
                        match res with
                        |Some x -> printf "%s\n" x.Dest
                        |None -> printf "-\n"
                        readAndOperate (n-1) f
                |Cancel(x) ->
                        let newf = cancel x f
                        readAndOperate (n-1) newf
                |Reroute(x) ->
                        let newf = reroute x f 
                        readAndOperate (n-1) newf
                |Delay(at, by) ->
                        let flight = destination at f
                        match flight with
                        |Some x -> 
                                let newtime = {x with Time = at + by}
                                let f' = cancel at f
                                let newf = buildTimeTable f' newtime 
                                readAndOperate (n-1) newf
                        |None -> failwith "didnt find the flight you requested"


let rec printTable f = 
        match f with
        |Empty -> "Empty"
        |Flight(left, n, right) ->
                "(" + printTable left + ", Dest: " + n.Dest + " Time: " + string n.Time + ", " + printTable right + ")"

let line = Console.ReadLine()
let nums = line.Split([|' '|]) |> Array.map int

let sched = readAndBuild nums[0] Empty
printf "%s\n" ( printTable sched )
readAndOperate nums[1] sched

