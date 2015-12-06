package switching

import "testing"

func BenchmarkMap(b *testing.B) {
	sum := 0
	m := make(map[int]int)
	m[0] = 0
	m[1] = 2
	m[2] = 3
	m[3] = 4
	m[4] = 6
	for n := 0; n < b.N; n++ {
		i := n % 10
		if val, ok := m[i]; ok {
			sum += val
		} else {
			sum -= 8
		}
	}
}

func BenchmarkCase(b *testing.B) {
	sum := 0
	for n := 0; n < b.N; n++ {
		i := n % 10

		switch i {
		case 0:
			sum += 0
		case 1:
			sum += 2
		case 2:
			sum += 3
		case 3:
			sum += 4
		case 4:
			sum += 6
		default:
			sum -= 8
		}
	}
}

func BenchmarkIf(b *testing.B) {
	sum := 0
	for n := 0; n < b.N; n++ {
		i := n % 10
		if i == 0 {
			sum += 0
		} else if i == 1 {
			sum += 2
		} else if i == 2 {
			sum += 3
		} else if i == 3 {
			sum += 4
		} else if i == 4 {
			sum += 6
		} else {
			sum -= 8
		}
	}
}

func TestSomething(b *testing.T) {
	x := 1+1 == 2
	if !x {
		b.Fail()
	}
}

/*
func main() {
	for i := 0; i < 10; i++ {
		switch i {
		case 1:
			fmt.Println("one")
		case 2:
			fmt.Println("two")
		case 3:
			fmt.Println("three")
		case 4, 5:
			fmt.Println("four or five")
		default:
			fmt.Println("More than 3")
		}
	}
}
*/
