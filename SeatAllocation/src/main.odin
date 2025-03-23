package exercise_dynamic_array

import pq "core:container/priority_queue"
import "core:fmt"
import "core:os"
import "core:strings"


main :: proc() {

    line, ok := os.read_entire_file("C:\\Users\\Elias Illeris Poggi\\Documents\\Programming\\Odin\\test\\text.txt")
    assert(ok)
    lines := strings.split(string(line), "\n")
    n, offSet, ok1 := fmt._parse_int(lines[0], 0)
    k, _, ok2 := fmt._parse_int(lines[0], offSet+1)
    assert(ok1)
    assert(ok2)

    partyList := make([dynamic]Party, 0, n)
    defer delete(partyList)

    queue : pq.Priority_Queue(^Party)
    queue_ptr := &queue
    pq.init(queue_ptr, less, pq.default_swap_proc(^Party), capacity = n)

    for i in 1..<len(lines)-1 {
        someInt, _, _ := fmt._parse_int(lines[i], 0)
        num := f64(someInt)
        party := Party {
            votes = num,
            quotient = num,
        }
        append(&partyList, party)
        pq.push(queue_ptr, &partyList[i-1])
    }

    for i in 1..= k {
        party : ^Party = pq.pop(queue_ptr)
        party.seats += 1
        party.quotient = party.votes/(1+party.seats)
        pq.push(queue_ptr, party)


    }

    for i in partyList {

        fmt.println(i.seats)
    }


//queue := Priority_Queue
//container.init(&queue)


}

less :: proc(a, b:^Party) -> bool
{
    if a.quotient > b.quotient
    {
        return true
    }
    return false
}

Party :: struct {
    votes : f64,
    seats : f64,
    quotient : f64,
}


