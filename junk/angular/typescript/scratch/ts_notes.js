// ##################################################
// classes
"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
var __param = (this && this.__param) || function (paramIndex, decorator) {
    return function (target, key) { decorator(target, key, paramIndex); }
};
var Foo = (function () {
    function Foo() {
    }
    return Foo;
}());
var Bar = (function () {
    function Bar() {
    }
    return Bar;
}());
var Baz = (function () {
    function Baz(foo, bar) {
    }
    return Baz;
}());
var baz = new Baz(new Foo(), new Bar());
console.log(baz);
var Person = (function () {
    function Person() {
    }
    return Person;
}());
function callServer(callback) {
    callback(null, 'hi');
}
callServer(function (error, data) { return console.log(data); }); // 'hi'
var printOut = function (message) {
    if (Array.isArray(message)) {
        console.log(message.join(', '));
    }
    else {
        console.log(message);
    }
};
printOut('hello out');
printOut(['hi', 'bye']);
// ##################################################
// implementing interfaces
var AnAction = (function () {
    function AnAction() {
    }
    AnAction.prototype.print = function () {
        return 5;
    };
    return AnAction;
}());
var NotAnAction = (function () {
    function NotAnAction() {
        this.type = 'Constructor function (class)';
    }
    NotAnAction.prototype.print = function () {
        return 10;
    };
    return NotAnAction;
}());
// create an Action variable
var a = {
    type: 'literal',
    print: function () { return 5; },
    error: false,
};
// they match the same "shape" so this is allowed
// (even though they don't explicitly define the same interface)
a = new NotAnAction();
// ##################################################
// type inference
var numbers = [2, 3, 5, 7, 11];
// numbers = ['fails compilation'] // tsc Type 'string[]' is not assignable to type 'number[]'.
var num_or_string = ['hello'];
num_or_string = [2, 3];
// ##################################################
// decorators
var core_1 = require('@angular/core');
// doesn't actually work
var MyComponent = (function () {
    function MyComponent(s) {
    }
    __decorate([
        core_1.Input(), 
        __metadata('design:type', String)
    ], MyComponent.prototype, "name", void 0);
    MyComponent = __decorate([
        core_1.Component({}),
        __param(0, core_1.Inject('myService')), 
        __metadata('design:paramtypes', [Object])
    ], MyComponent);
    return MyComponent;
}());
//# sourceMappingURL=ts_notes.js.map