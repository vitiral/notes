/*
https://learnxinyminutes.com/docs/typescript/

Share this page
Select theme: light dark
Learn X in Y minutes
Where X=TypeScript
Get the code: learntypescript.ts

TypeScript is a language that aims at easing development of large scale applications written in JavaScript. TypeScript adds common concepts such as classes, modules, interfaces, generics and (optional) static typing to JavaScript. It is a superset of JavaScript: all JavaScript code is valid TypeScript code so it can be added seamlessly to any project. The TypeScript compiler emits JavaScript.

This article will focus only on TypeScript extra syntax, as opposed to JavaScript.

To test TypeScript’s compiler, head to the Playground where you will be able to type code, have auto completion and directly see the emitted JavaScript.
*/
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
// There are 3 basic types in TypeScript
var isDone = false;
var lines = 42;
var anders = "Anders";
// But you can omit the type annotation if the variables are derived
// from explicit literals
var isDone2 = false;
var lines2 = 42;
var anders2 = "Anders";
// When it's impossible to know, there is the "Any" type
var notSure = 4;
notSure = "maybe a string instead";
notSure = false; // okay, definitely a boolean
// Use const keyword for constants
var numLivesForCat = 9;
// numLivesForCat = 1; // Error
// For collections, there are typed arrays and generic arrays
var list = [1, 2, 3];
// Alternatively, using the generic array type
var array = [1, 2, 3];
// For enumerations:
var Color;
(function (Color) {
    Color[Color["Red"] = 0] = "Red";
    Color[Color["Green"] = 1] = "Green";
    Color[Color["Blue"] = 2] = "Blue";
})(Color || (Color = {}));
;
var c = Color.Green;
// Lastly, "void" is used in the special case of a function returning nothing
function bigHorribleAlert() {
    alert("I'm a little annoying box!");
}
// Functions are first class citizens, support the lambda "fat arrow" syntax and
// use type inference
// The following are equivalent, the same signature will be inferred by the
// compiler, and same JavaScript will be emitted
var f1 = function (i) { return i * i; };
// Return type inferred
var f2 = function (i) { return i * i; };
// "Fat arrow" syntax
var f3 = function (i) { return i * i; };
// "Fat arrow" syntax with return type inferred
var f4 = function (i) { return i * i; };
// "Fat arrow" syntax with return type inferred, braceless means no return
// keyword needed
var f5 = function (i) { return i * i; };
// Object that implements the "Person" interface
// Can be treated as a Person since it has the name and move properties
var p = { name: "Bobby", move: function () { } };
// Objects that have the optional property:
var validPerson = { name: "Bobby", age: 42, move: function () { } };
// Only the parameters' types are important, names are not important.
var mySearch;
mySearch = function (src, sub) {
    return src.search(sub) != -1;
};
// Classes - members are public by default
var Point = /** @class */ (function () {
    // Constructor - the public/private keywords in this context will generate
    // the boiler plate code for the property and the initialization in the
    // constructor.
    // In this example, "y" will be defined just like "x" is, but with less code
    // Default values are also supported
    function Point(x, y) {
        if (y === void 0) { y = 0; }
        this.y = y;
        this.x = x;
        this.p = x * y;
    }
    // Functions
    Point.prototype.dist = function () { return Math.sqrt(this.x * this.x + this.y * this.y); };
    Point.sameSides = function (side) {
        return new Point(side, side);
    };
    // Static members
    Point.origin = new Point(0, 0);
    return Point;
}());
// Classes can be explicitly marked as implementing an interface.
// Any missing properties will then cause an error at compile-time.
var PointPerson = /** @class */ (function () {
    function PointPerson() {
    }
    PointPerson.prototype.move = function () { };
    return PointPerson;
}());
var p1 = new Point(10, 20);
var p2 = new Point(25); //y will be 0
var p3 = Point.sameSides(10);
// Inheritance
var Point3D = /** @class */ (function (_super) {
    __extends(Point3D, _super);
    function Point3D(x, y, z) {
        if (z === void 0) { z = 0; }
        var _this = _super.call(this, x, y) || this;
        _this.z = z;
        return _this;
    }
    // Overwrite
    Point3D.prototype.dist = function () {
        var d = _super.prototype.dist.call(this);
        return Math.sqrt(d * d + this.z * this.z);
    };
    return Point3D;
}(Point));
// Modules, "." can be used as separator for sub modules
var Geometry;
(function (Geometry) {
    var Square = /** @class */ (function () {
        function Square(sideLength) {
            if (sideLength === void 0) { sideLength = 0; }
            this.sideLength = sideLength;
        }
        Square.prototype.area = function () {
            return Math.pow(this.sideLength, 2);
        };
        return Square;
    }());
    Geometry.Square = Square;
})(Geometry || (Geometry = {}));
var s1 = new Geometry.Square(5);
// Local alias for referencing a module
var G = Geometry;
var s2 = new G.Square(10);
// Generics
// Classes
var Tuple = /** @class */ (function () {
    function Tuple(item1, item2) {
        this.item1 = item1;
        this.item2 = item2;
    }
    return Tuple;
}());
// And functions
var pairToTuple = function (p) {
    return new Tuple(p.item1, p.item2);
};
var tuple = pairToTuple({ item1: "hello", item2: "world" });
// Including references to a definition file:
/// <reference path="jquery.d.ts" />
// Template Strings (strings that use backticks)
// String Interpolation with Template Strings
var tyrone = 'Tyrone';
var greeting = "Hi " + tyrone + ", how are you?";
// Multiline Strings with Template Strings
var multiline = "This is an example\nof a multiline string";
var p4 = { name: "Tyrone", age: 42 };
// p4.age = 25; // Error, p4.age is read-only
var p5 = { name: "John", age: 60 };
var p6 = p5; // Ok, read-only alias for p5
// p6.age = 35; // Error, p6.age is read-only
p5.age = 45; // Ok, but also changes p6.age because of aliasing
/*
class Car {
  readonly make: string;
  readonly model: string;
  readonly year = 2018;

  constructor() {
    this.make = "Unknown Make"; // Assignment permitted in constructor
    this.model = "Unknown Model"; // Assignment permitted in constructor
  }
}

let numbers: Array<number> = [0, 1, 2, 3, 4];
let moreNumbers: ReadonlyArray<number> = numbers;
moreNumbers[5] = 5; // Error, elements are read-only
moreNumbers.push(5); // Error, no push method (because it mutates array)
moreNumbers.length = 3; // Error, length is read-only
numbers = moreNumbers; // Error, mutating methods are missing

// Tagged Union Types for modelling state that can be in one of many shapes
type State =
  | { type: "loading" }
  | { type: "success", value: number }
  | { type: "error", message: string };

declare const state: State;
if (state.type === "success") {
  console.log(state.value);
} else if (state.type === "error") {
  console.error(state.message);
}

// Iterators and Generators

// for..of statement
// iterate over the list of values on the object being iterated
let arrayOfAnyType = [1, "string", false];
for (const val of arrayOfAnyType) {
    console.log(val); // 1, "string", false
}

let list = [4, 5, 6];
for (const i of list) {
   console.log(i); // 4, 5, 6
}

// for..in statement
// iterate over the list of keys on the object being iterated
for (const i in list) {
   console.log(i); // 0, 1, 2
}

// Type Assertion

let foo = {} // Creating foo as an empty object
foo.bar = 123 // Error: property 'bar' does not exist on `{}`
foo.baz = 'hello world' // Error: property 'baz' does not exist on `{}`

// Because the inferred type of foo is `{}` (an object with 0 properties), you
// are not allowed to add bar and baz to it. However with type assertion,
// the following will pass:

interface Foo {
  bar: number;
  baz: string;
}

let foo = {} as Foo; // Type assertion here
foo.bar = 123;
foo.baz = 'hello world'

******/
/*
Further Reading
TypeScript Official website
TypeScript language specifications
Anders Hejlsberg - Introducing TypeScript on Channel 9
Source Code on GitHub
Definitely Typed - repository for type definitions
Got a suggestion? A correction, perhaps? Open an Issue on the Github Repo, or make a pull request yourself!

Originally contributed by Philippe Vlérick, and updated by 18 contributor(s).

Creative Commons License
© 2020 Philippe Vlérick
*/
