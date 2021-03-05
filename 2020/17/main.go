package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
)

const (
	xSide = iota + 1
	ySide
	zSide
)

type field struct {
	xSize, ySize, zSize int
	body                [][][]bool
}

func (field *field) at(x, y, z int) bool {
	return field.body[x][y][z]
}

func (field *field) expand(dir int) {
	switch dir {
	case xSide:
		layer := make2dSlice(field.ySize, field.zSize)
		field.body = append(field.body, layer)
		field.xSize++
	case -xSide:
		layer := make2dSlice(field.ySize, field.zSize)
		field.body = append([][][]bool{layer}, field.body...)
		field.xSize++
	case ySide:
		for i := range field.body {
			row := make([]bool, field.zSize)
			field.body[i] = append(field.body[i], row)
		}
		field.ySize++
	case -ySide:
		for i := range field.body {
			row := make([]bool, field.zSize)
			field.body[i] = append([][]bool{row}, field.body[i]...)
		}
		field.ySize++
	case zSide:
		for i := range field.body {
			for j := range field.body[i] {
				field.body[i][j] = append(field.body[i][j], false)
			}
		}
		field.zSize++
	case -zSide:
		for i := range field.body {
			for j := range field.body[i] {
				field.body[i][j] = append([]bool{false}, field.body[i][j]...)
			}
		}
		field.zSize++
	}
}

func make2dSlice(rows, cols int) [][]bool {
	result := make([][]bool, rows)
	for i := range result {
		result[i] = make([]bool, cols)
	}
	return result
}

func print2dSlice(body [][]bool) {
	strs := make([][]rune, len(body))
	for i := range strs {
		strs[i] = make([]rune, 0)
	}
	for i, row := range body {
		for _, cell := range row {
			if cell {
				strs[i] = append(strs[i], '#')
			} else {
				strs[i] = append(strs[i], '.')
			}
		}
	}
	result := make([]string, len(body))
	for i := range strs {
		result[i] = string(strs[i])
	}
	fmt.Println(strings.Join(result, "\n"))
}

func (field *field) side(side int) [][]bool {
	var result [][]bool
	switch side {
	case xSide, -xSide:
		result = make2dSlice(field.ySize, field.zSize)
		if side == xSide {
			copy(result, field.body[field.xSize-1])
		} else {
			copy(result, field.body[0])
		}
	case ySide, -ySide:
		yLayerIdx := 0
		if side == ySide {
			yLayerIdx = field.ySize - 1
		}
		result = make2dSlice(field.xSize, field.zSize)
		for i := 0; i < field.xSize; i++ {
			for j := 0; j < field.zSize; j++ {
				result[i][j] = field.body[i][yLayerIdx][j]
			}
		}
	case zSide, -zSide:
		zLayerIdx := 0
		if side == zSide {
			zLayerIdx = field.zSize - 1
		}
		result = make2dSlice(field.xSize, field.ySize)
		for i := 0; i < field.xSize; i++ {
			for j := 0; j < field.ySize; j++ {
				result[i][j] = field.body[i][j][zLayerIdx]
			}
		}
	}
	return result
}

func hasCube(side [][]bool) bool {
	for _, row := range side {
		for _, x := range row {
			if x {
				return true
			}
		}
	}
	return false
}

func (field *field) neighbours(x, y, z int) (result int) {
	sx, sy, sz := field.xSize-1, field.ySize-1, field.zSize-1
	type predicate struct {
		cond  bool
		coord [3]int
	}
	p := func(cond bool, x, y, z int) predicate {
		return predicate{cond, [3]int{x, y, z}}
	}
	predicates := []predicate{
		p(x > 0, -1, 0, 0),
		p(x < sx, 1, 0, 0),
		p(y > 0, 0, -1, 0),
		p(y < sy, 0, 1, 0),
		p(z > 0, 0, 0, -1),
		p(z < sz, 0, 0, 1),

		p(x > 0 && y > 0, -1, -1, 0),
		p(x < sx && y < sy, 1, 1, 0),
		p(x > 0 && y < sy, -1, 1, 0),
		p(x < sx && y > 0, 1, -1, 0),
		p(x > 0 && z > 0, -1, 0, -1),
		p(x < sx && z < sz, 1, 0, 1),
		p(x > 0 && z < sz, -1, 0, 1),
		p(x < sx && z > 0, 1, 0, -1),
		p(y > 0 && z > 0, 0, -1, -1),
		p(y < sy && z < sz, 0, 1, 1),
		p(y > 0 && z < sz, 0, -1, 1),
		p(y < sy && z > 0, 0, 1, -1),

		p(x > 0 && y > 0 && z > 0, -1, -1, -1),
		p(x > 0 && y > 0 && z < sz, -1, -1, 1),
		p(x > 0 && y < sy && z > 0, -1, 1, -1),
		p(x > 0 && y < sy && z < sz, -1, 1, 1),
		p(x < sx && y > 0 && z > 0, 1, -1, -1),
		p(x < sx && y > 0 && z < sz, 1, -1, 1),
		p(x < sx && y < sy && z > 0, 1, 1, -1),
		p(x < sx && y < sy && z < sz, 1, 1, 1),
	}
	for _, predicate := range predicates {
		dx, dy, dz := predicate.coord[0], predicate.coord[1], predicate.coord[2]
		if predicate.cond && field.body[x+dx][y+dy][z+dz] {
			result++
		}
	}
	return
}

func (fd *field) pad() {
	for _, s := range []int{xSide, -xSide, ySide, -ySide, zSide, -zSide} {
		if hasCube(fd.side(s)) {
			fd.expand(s)
		}
	}
}

func (fd *field) copyBody() [][][]bool {
	result := make([][][]bool, fd.xSize)
	for i := range fd.body {
		layer := make2dSlice(fd.ySize, fd.zSize)
		for j := range fd.body[i] {
			for k := range fd.body[i][j] {
				layer[j][k] = fd.body[i][j][k]
			}
		}
		result[i] = layer
	}
	return result
}

func (fd *field) nextStep() {
	fd.pad()
	body := fd.copyBody()
	for i := range fd.body {
		for j := range fd.body[i] {
			for k, isAlive := range fd.body[i][j] {
				nNeighbours := fd.neighbours(i, j, k)
				if isAlive && (nNeighbours < 2 || nNeighbours > 3) {
					body[i][j][k] = false
				} else if !isAlive && nNeighbours == 3 {
					body[i][j][k] = true
				}
			}
		}
	}
	fd.body = body
}

func (fd *field) nAlive() (result int) {
	for _, layer := range fd.body {
		for _, row := range layer {
			for _, cell := range row {
				if cell {
					result++
				}
			}
		}
	}
	return
}

func parseInput(s string) field {
	lines := strings.Split(s, "\n")
	layer := make([][]bool, 0)
	for i, line := range lines {
		if len(line) == 0 {
			continue
		}
		layer = append(layer, make([]bool, len(line)))
		row := make([]bool, len(line))
		for j, c := range line {
			if c == '#' {
				row[j] = true
			} else {
				row[j] = false
			}
		}
		layer[i] = row
	}
	body := [][][]bool{layer}
	return field{
		body:  body,
		xSize: len(body),
		ySize: len(body[0]),
		zSize: len(body[0][0]),
	}
}

func main() {
	inputFname := os.Args[1]
	contents, _ := ioutil.ReadFile(inputFname)
	field := parseInput(string(contents))
	for i := 0; i < 6; i++ {
		field.nextStep()
	}
	fmt.Println("Alive cubes:", field.nAlive())
}
