/* The panic channel attempt */
package main

func gopan(e chan error) {
	if e != nil {
		panic(nil)
	}
}

func main() {
	e := make(chan error)
	go gopan(e)
}
