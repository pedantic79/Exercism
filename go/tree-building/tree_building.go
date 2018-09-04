package tree

import (
	"fmt"
	"sort"
)

type Record struct {
	ID, Parent int
}

type Node struct {
	ID       int
	Children []*Node
}

type NodeSlice []*Node

func (n NodeSlice) Len() int {
	return len(n)
}

func (n NodeSlice) Less(i, j int) bool {
	return n[i].ID < n[j].ID
}

func (n NodeSlice) Swap(i, j int) {
	n[i], n[j] = n[j], n[i]
}

func Build(records []Record) (*Node, error) {
	l := len(records)
	if l == 0 {
		return nil, nil
	}

	nodes := make([]Node, l)
	seen := make([]bool, l)

	for _, r := range records {
		if r.ID >= l {
			return nil, fmt.Errorf("Record ID too high %d", r.ID)
		}
		if seen[r.ID] {
			return nil, fmt.Errorf("Record ID %d occurs multiple times", r.ID)
		}
		if r.ID == 0 && r.Parent != 0 {
			return nil, fmt.Errorf("Root node has non-0 parent %d", r.Parent)
		}
		if r.ID != 0 {
			if r.ID <= r.Parent {
				return nil, fmt.Errorf("Record ID %d has parent %d to self or later", r.ID, r.Parent)
			}
			nodes[r.Parent].Children = append(nodes[r.Parent].Children, &nodes[r.ID])
		}

		seen[r.ID] = true
		nodes[r.ID].ID = r.ID
	}

	for _, n := range nodes {
		sort.Sort(NodeSlice(n.Children))
	}

	return &nodes[0], nil
}
