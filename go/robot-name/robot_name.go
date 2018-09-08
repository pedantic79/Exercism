package robotname

import (
	"fmt"
	"math/rand"
	"time"
)

type Robot struct {
	name *string
}

var nameCache map[string]bool = map[string]bool{}

func init() {
	rand.Seed(time.Now().UnixNano())
}

func (rbt *Robot) Name() string {
	if rbt.name == nil {
		name := generateName()
		for nameCache[name] {
			name = generateName()
		}
		nameCache[name] = true
		rbt.name = &name
	}

	return *rbt.name
}

func generateName() string {
	n := rand.Intn(1000)
	b := 'A' + rand.Intn(26)
	a := 'A' + rand.Intn(26)
	return fmt.Sprintf("%c%c%03d", a, b, n)
}

func (rbt *Robot) Reset() {
	if rbt.name != nil {
		// nameCache[*rbt.name] = false
		rbt.name = nil
	}
}
