println("Hello, World!")

fun sum(a: Int, b:Int): Int {
  return a + b
}

println("1 + 2: ${sum(1, 2)}")


val a: Int = 2
val b = 2
// b = 3 error: val cannot be reassigned
println("a + b: ${a + b}")

var c = 7
c = 5
println("a + c: ${a + c}")

open class Shape

class Rectangle(var height: Double, var length: Double): Shape() {
  var perimeter = (height + length) * 2
}

val r = Rectangle(5.0, 2.0)
println("perimiter: ${r.perimeter}")
println("rectangle: $r")

if (7 in 1..10) println("7 in: 1..10")
if (10 in 1..10) println("10 in: 1..10")
if (10 !in 1 until 10) println("10 not in: 1 until 10")
print("Counting from 2 to 10: ")
for(i in 2..10) print("$i ")
println()

var fruits = listOf("banana", "avocado", "apple", "kiwifruit")
print("Fruits starting with a: ")
fruits
  .filter { it.startsWith("a") }
  .sortedBy { it }
  .map { it.uppercase() }
  .forEach { print("$it ")  }
println()

fun parseInt(s: String): Int? {
  try { return s.toInt() }
  catch (e: NumberFormatException) { return null; }
}

fun product(xStr: String, yStr: String): Int? {
  val x = parseInt(xStr)
  val y = parseInt(yStr)

  if (x == null || y == null) return null
  return x * y
}

println("Product 4 * 7: ${product("4", "7")}")
println("Product 4 * hello: ${product("4", "hello")}")

fun lazyBro(s: String): String {
  val p: String by lazy {
    println("Computing p...")
    s + " is a lazy bro"
  }
  if (s == "bro") return "bro no bro";
  return p + " bro";
}

println(lazyBro("bob"))
println(lazyBro("sue"))
println(lazyBro("bro"))


fun String.capitalBro(): String {
  return this.replace("bro", "Bro")
}

println("bro, bro should be bigger".capitalBro())
