import { Component } from '@angular/core';

@Component({
    moduleId: module.id,
    selector: 'my-user-input',
    templateUrl: './my-user-input.cmt.html',
})
export class MyUserInputCmt {
    private heroes: string[] = ["Fellwind", "Darklost"];

    add(hero: string) {
        this.heroes.push(hero);
    }
}

@Component({
    selector: 'my-app',
    template: `
        <h1>User Input</h1>
        <my-user-input></my-user-input>
    `
})
export class AppCmt { }
