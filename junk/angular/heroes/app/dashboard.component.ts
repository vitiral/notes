import { OnInit, Component } from '@angular/core';
import { Router } from '@angular/router';

import { Hero } from './hero';
import { HeroService } from './hero.service';

@Component({
    moduleId: module.id,
    selector: 'my-dashboard',
    templateUrl: '../html/dashboard.component.html',
    styleUrls: [ '../html/dashboard.component.css' ],

    // specifying providers is necessary for service
    // dependency injection (but not Router... don't know
    // why...)
    providers: [ HeroService ],
})
export class DashboardComponent implements OnInit {
    heroes: Hero[] = [];

    // angular knows how to inject dependencies based on the arguments
    // here -- all seems very magical to me.
    constructor(
        private router: Router,
        private heroService: HeroService,
    ) { }

    // this is called when the component is initialized by the browser
    // In this case, it can make a call to a service to get the heroes
    ngOnInit(): void {
        this.heroService.getHeroes()
            .then(heroes => this.heroes = heroes.slice(1, 5));
    }

    // this is referenced in the template and is called when a hero
    // on the dashboard is clicked
    gotoDetail(hero: Hero): void {
        let link = ['/detail', hero.id];
        this.router.navigate(link)
    }
}
