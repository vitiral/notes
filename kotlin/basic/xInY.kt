
// Just like java
package com.learnxinyminutes.kotlin

// rust-like syntax
fun main(args: Array<String>) {
  // val=value (immutable). var=variable (mutable)
  val fooVal = 10
  var fooVar: Int = 10  // can specify type
  fooVar += fooVal

  // note: args doesn't print neatly. We'll fix later
  println("args=$args")
  println("fooVal=$fooVal  fooVar=$fooVar")

  val hello = "Hello"
  val name = "World"
  println("$hello $name!")
  println(hello + " " + name + "!")

  fun hello(name: String = "Bob"): String { return "Hello, $name!" }
  println(hello("foo"))
  println(hello())

  // A function which is a single expression can use '='
  fun odd(x: Int): Boolean = (x % 2 == 1)
  println("3 is odd: ${odd(3)}")

  // Accepts function and returns function
  fun notHigherOrder(f: ((Int) -> Boolean)): ((Int) -> Boolean) {
    return {n -> !f.invoke(n)} // closure
  }
  val notOdd = notHigherOrder(::odd)
  println("4 is not odd: ${notOdd(4)}")

  // lambda expression can be specified as arguments WITHOUT parens
  val notZero = notHigherOrder{n -> n == 0}

  println("1 is notZero: ${notZero(1)}")

  class ExampleClass(val x: Int) {
    fun memberFunction(y: Int): Int = (x + y)

    infix fun infixFunction(y: Int): Int {
      return x * y
    }
  }

  var ex = ExampleClass(5)
  println(ex) // print is bad
  println(ex.memberFunction(5))
  println(ex infixFunction 5)

  // Note: data values can be mutable(var) or immutable(val)
  data class Data(var x: Int) {
    infix fun infixFun(y: Int): Int {
      return x * y
    }
  }
  var d = Data(5)
  println(d) // print is bad
  println(d infixFun 5)

  // Map data
  val mapData = mapOf("a" to 1, "b" to 2)
  println(mapData)

  // Nice shortcut to access members
  with(d) {
    x += 5
  }
  println(d)

  val z = (1..9)
    .map {it * 3}
    .filter {it < 20}
    .groupBy {it % 2 == 0}
    .mapKeys {if (it.key) "even" else "odd"}
  println(z)

  // when: kotlin match
  val i = 21
  when {
    i < 7 -> println("first")
    else -> println("else")
  }

  // Both when and if return the result of the expression
  var result = when (i) {
      0, 21 -> if(0 == i) "0" else "21"
      in 1..20 -> "in the range 1 to 20"
      else -> "none of the above"
  }
  println(result)

  println("Give me the \$baby")

    open class Monster1
    class Goblin1 : Monster1();

    open class Monster2() {
        constructor(name: String): this() {
            println("Created monster 2 $name")
        }
    }
    class Goblin2 : Monster2 {
      constructor() : super("default") {
        println("Created goblin 2")
      }
    }

    val gob2 = Goblin2()

    class Gremlin { val age get() = 42 }

    val grem = Gremlin()
    println("Gremlin's are all age ${grem.age}")
}

