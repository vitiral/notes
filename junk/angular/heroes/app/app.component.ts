//! This is the root component that has the base-view for all other
//! components. It is referenced as the root by index.html <my-app>

import { Component } from '@angular/core';

@Component({
    // the moduleId is necessary if you have templateUrl. Not sure what
    // else it is used for
    moduleId: module.id,

    // the selector is what is used in html to denote where the component
    // should be used. In this case, <my-app>Loading...</my-app>
    selector: 'my-app',

    // the template or templateUrl
    template: `
        <h1>{{title}}</h1>
        <nav>
            <a routerLink="/dashboard" routerLinkActive="active">Dashboard</a>
            <a routerLink="/heroes" routerLinkActive="active">Heroes</a>
        </nav>
        <router-outlet></router-outlet>
    `,

    // style sheets to use
    styleUrls: [ '../html/app.component.css' ],
})
export class AppComponent {
    // variables can be accessed in the template. This variable
    // is accessed through {{title}}
    title = "Tour of Heroes";
}
