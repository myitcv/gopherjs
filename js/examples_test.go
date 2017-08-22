package js_test

import (
	"fmt"

	"github.com/gopherjs/gopherjs/js"
)

// ExampleObject_Object demonstrates
func ExampleObject_Object() {
	x := eval("5")
	fmt.Printf("%T\n", x)
	// Output: *js.Object
}

func ExampleObject_int() {
	x := eval("5").Int()
	fmt.Printf("%v: %T\n", x, x)
	// Output: 5: int
}

// eval is a convenience function for the Example* tests
func eval(s string) *js.Object {
	return js.Global.Call("eval", s)
}

// consolelog is a convenience function for the Example* tests
func consolelog(args ...interface{}) {
	js.Global.Get("console").Call("log", args...)
}
