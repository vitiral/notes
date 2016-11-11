'use strict';

class Toppings {
    constructor() {
        this.toppings = 'test';
    }
    formatToppings(param) {
        console.log(param);
    }
    list() {
        return this.formatToppings(this.toppings);
    }
}

var topping = new Toppings();
topping.list();

var list = topping.list.bind(topping);

list();

// You can use arrow functions

let items = [1,2,3];
let incrementedItems = [];

items.forEach(function(x) {
    console.log(x);
    incrementedItems.push(x+1);
});

console.log(incrementedItems);

items.forEach((x) => {
    console.log(x);
    incrementedItems.push(x+2);
});

console.log(incrementedItems);

// or you can do this
incrementedItems = items.map((x) => x+1);

console.log(incrementedItems);

// note that arrow functions do not set a local copy of this, arguments, super or new.target


// you can use `backticks` to do special built-in string formatting (yay?)
var name = 'Sam';
var age = 42;

console.log('hello my name is ' + name + ' I am ' + age + ' years old');
console.log(`hello my name is ${name}
            I am ${age} years old`);  // spaces and newline are kept

// everything is scoped to a function (typically)


function test() {
    console.log(name);
    var i = 0;

    for (; i < 20; i+= 1) {
        var n = i * 2;
        let j = i * 3; // scoped to this block
        const k = 5; // const can't be assigned
    }
    console.log(n);
    // console.log(j); // not in scope because let was used
    // console.log(k); // k also can't be accessed

    const k = {};
    console.log(k);
    k.wow = 'wow'; // can assign to constant attributes
    console.log(k.wow);
}

test();

//console.log(i); // not in scope
//

function print(a, b, c, ...more) {
    console.log(more[0]);
    console.log(arguments[0]);
}

print(1, 2, 3, 4, 5);

// destructuring

let foo = ['one', 'two', 'three'];

let one = foo[0]
let two = foo[1]
let three = foo[2]

let [one_, two_, ...rest_foo] = foo;

console.log([one, two, three, one_, two_, rest_foo])

let myModule = {
    drawSquare: function drawSquare(length) {},
    drawCircle: function drawCircle(length) {},
    drawText: function drawText(length) {},
};

let {drawSquare, drawText} = myModule;

drawSquare(5);
drawText(5);

let jane = {firstName: 'Jane', lastName: 'Doe'};
let john = {firstName: 'John', lastName: 'Doe', middleName: 'Smith'};

function sayName({firstName, lastName, middleName = 'N/A'}) {
    console.log(`Hello ${lastName}, ${firstName} ${middleName}`)
}

sayName(jane);
sayName(john);
