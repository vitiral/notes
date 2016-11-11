import {Component} from '@angular/core';

@Component({
	selector: 'hello',
	template: '<p>Hello {{name}}</p>' // <p> not necessary
})
export class Hello {
  name: string;

  constructor() {
    this.name = 'World';
  }
}
