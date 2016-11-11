// without ts
function add(a, b) {
    return a + b;
}
// console.log(add(1, '3')) // error: param string not assignable
var Pizza = (function () {
    function Pizza(toppings) {
        this.toppings = toppings;
        this.crust = 42;
    }
    return Pizza;
}());
var p = new Pizza(['']);
console.log("pizza: " + p);
