

// ##################################################
// classes

class Foo { foo: number; }
class Bar { bar: string; }

class Baz {
    constructor(foo: Foo, bar: Bar) { }
}


let baz = new Baz(new Foo(), new Bar())
console.log(baz)

class Person {
    name: string;
    nickName?: string;
}

// ##################################################
// interfaces

// interface to describe a function
interface Callback {
    (error: Error, data: any): void;
}

function callServer(callback: Callback) {
    callback(null, 'hi');
}

callServer((error, data) => console.log(data)); // 'hi'
// callServer('hi');  // tsc error

// ##################################################
// overloaded functions as interfaces

interface PrintOutput {
    (message: string): void;   // common case
    (message: string[]): void; // less common case
}

let printOut: PrintOutput = (message) => {
    if (Array.isArray(message)) {
        console.log(message.join(', '));
    } else {
        console.log(message);
    }
}

printOut('hello out');
printOut(['hi', 'bye']);

// ##################################################
// creating interfaces

interface Action {
    type: string;
    print(): number;
    error?: boolean;
}

// ##################################################
// implementing interfaces
class AnAction implements Action {
    type: string;
    print(): number {
        return 5;
    }
}


class NotAnAction {
    type: string;
    constructor() {
        this.type = 'Constructor function (class)';
    }
    print() {
        return 10;
    }
}

// create an Action variable
let a: Action = {
    type: 'literal',
    print: () => { return 5; },
    error: false, // optional
};

// they match the same "shape" so this is allowed
// (even though they don't explicitly define the same interface)
a = new NotAnAction();


// ##################################################
// type inference

let numbers = [2, 3, 5, 7, 11];
// numbers = ['fails compilation'] // tsc Type 'string[]' is not assignable to type 'number[]'.

let num_or_string: number[] | string[] = ['hello'];
num_or_string = [2, 3];



// ##################################################
// decorators

import { Component, Inject, Input } from '@angular/core';

// doesn't actually work
@Component({

})
class MyComponent {
    @Input() name: string;

    constructor(@Inject('myService') s) {
    }

}
