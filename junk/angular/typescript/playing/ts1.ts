
// without ts
function add(a: number, b: number) {
    return a + b;
}

// console.log(add(1, '3')) // error: param string not assignable

class Pizza {
    toppings: string[];
    crust: any;  // you can use the type any as well
    constructor(toppings: string[]) {
        this.toppings = toppings;
        this.crust = 42;
    }
}

let p = new Pizza(['']);

console.log(`pizza: ${p}`);
