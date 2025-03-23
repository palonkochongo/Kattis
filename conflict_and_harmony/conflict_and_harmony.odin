package conflict_and_harmony

import "core:fmt"
import "core:os"
import "core:strings"
import "base:runtime"
import list "core:container/intrusive/list"

Edge :: struct {
	node: list.Node,
	isConflicting: bool,
	vert : ^Vertex,
}
Vertex :: struct {
	node : list.Node,
	isColoured : bool,
	isTouched : bool,
	edges : list.List,
}


addEdge :: proc (graph : ^[]Vertex, v, u, c : int) {
		edgeVU:= new(Edge)
		edgeVU^ = {
			isConflicting = c == 1 ? true : false,
			vert = &graph[u],
		}
		list.push_back(&graph[v].edges, &edgeVU.node)

		edgeUV:= new(Edge)
		edgeUV^ = {
			isConflicting = c == 1 ? true : false,
			vert = &graph[v],
		}
		list.push_back(&graph[u].edges, &edgeUV.node)
}

pushToQueue :: proc (pq: ^list.List, v : ^Vertex ) {
	iter := list.iterator_head(v.edges, Edge, "node")
	for i in list.iterate_next(&iter) {
		list.push_front(pq, &i.node)
	}
}

bfs :: proc (graph : ^[]Vertex) -> int {
	pq : list.List
	list.push_front(&pq, &graph[0].node)
	for !list.is_empty(&pq) {
		vNode := list.pop_back(&pq) //Pop a vertex
		v := runtime.container_of(vNode, Vertex, "node")
		iter := list.iterator_head(v.edges, Edge, "node") //Iterator for the egdes of V

		for i in list.iterate_next(&iter) {
			if !i.vert.isTouched {
				i.vert.isColoured = i.isConflicting == true ? !v.isColoured : v.isColoured
				i.vert.isTouched = true
				list.push_front(&pq, &i.vert.node)
			} else {
				harmony := v.isColoured == i.vert.isColoured ? true : false
				if harmony == i.isConflicting {return 0}
			}
		}

	}
	return 1
}

main :: proc () {
	line, ok := os.read_entire_file(os.stdin)
	assert(ok)
	lines := strings.split(string(line), "\n")
	n, offSet, ok1 := fmt._parse_int(lines[0], 0)
	m, _, ok2 := fmt._parse_int(lines[0], offSet+1)
	assert(ok1)
	assert(ok2)


	graph := make([]Vertex, n)
	defer delete(graph)

	for i in 0..<m {
		u, offSet, ok1 := fmt._parse_int(lines[i+1], 0)
		v, offSet2, ok2 := fmt._parse_int(lines[i+1], offSet+1)
		c, offSet3, ok3 := fmt._parse_int(lines[i+1], offSet2+1)

		addEdge(&graph, v, u, c)
	}
	
	graph[0].isTouched = true
	graph[0].isColoured = true

	result := bfs(&graph)

	fmt.println(result)

}
