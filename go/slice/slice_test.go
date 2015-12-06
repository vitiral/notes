package slice

import "testing"
import "github.com/stretchr/testify/assert"
import "fmt"

func TestBasic(t *testing.T) {
	a := [10]int{1, 2, 3, 4, 5}
	b := [10]int{}
	copy(b[:], a[:])
	assert.Equal(t, a, b)
	b[9] = 8
	assert.NotEqual(t, a, b)
}

// Why does this not exist in the stdlib?
func Extend(self, value []int) []int {

	new := make([]int, len(self)+len(value))
	copy(new, self)
	copy(new[len(self):], value)
	return new
}

func TestSlicesOfArrays(t *testing.T) {
	a := [10]int{1, 2, 3, 4, 5}
	// They are slices of the same array
	s1 := a[3:7]
	s2 := a[3:7]
	s1[2] = 3
	assert.Equal(t, s1, s2)
	// if you append onto one, the other one doesn't get the LENGTH
	s1 = append(s1, 99)
	assert.NotEqual(t, s1, s2)
	// but they both update the same ARRAY
	s2 = append(s2, 102)
	assert.Equal(t, s1, s2)
	s1 = append(s1, 1000)
	s3 := a[3:9]
	assert.Equal(t, s1, s3)
	s2 = append(s2, 9999, 1, 2, 3)
	s1 = Extend(s1, []int{1, 2, 3})
	assert.NotEqual(t, s1, s2)
	s1[len(s1)-4] = 9999
	assert.Equal(t, s1, s2)

}

func TestSlices(t *testing.T) {
	s1 := make([]string, 3)
	s2 := make([]int, 3)
	a := [3]int{1, 2, 3}
	copy(s2, a[:])
	assert.Equal(t, s2, a[:])
	s1[2] = "hi    this is bob"
	s1[0] = "the beginning"
	fmt.Println("String:", s1)
}
