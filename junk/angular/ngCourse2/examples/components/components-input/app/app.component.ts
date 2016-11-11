import {Component} from '@angular/core';

@Component({
	selector: 'app',
	template: `<div>
        <!--  [brackets] denote to use property of this.helloName -->
	    <hello [name]="helloName"></hello>
        <!--  no-brackets is interpreted as a literal -->
	    <hello name="Another World"></hello>
	  </div>`
})
export class App {
  helloName: string;

  constructor() {
    this.helloName = "World";
  }
}
