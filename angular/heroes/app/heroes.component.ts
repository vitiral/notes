
import { OnInit, Component } from '@angular/core';
import { Router } from '@angular/router';

import { Hero } from './hero';
import { HeroDetailComponent } from './hero-detail.component';
import { HeroService } from './hero.service';

@Component({
    moduleId: module.id,
    selector: 'my-heroes',
    templateUrl: '../html/heroes.component.html',
    styleUrls: [ '../html/heroes.component.css' ],
    providers: [HeroService],
})
export class HeroesComponent implements OnInit {
    title = "Tour of Heroes";
    heroes: Hero[];
    selectedHero: Hero;

    constructor(
        private router: Router,
        private heroService: HeroService,
    ) { }

    ngOnInit(): void {
        this.getHeroes()
    }

    getHeroes(): void{
        this.heroService.getHeroes().then(heroes => this.heroes = heroes);
        //this.heroService.getHeroesSlowly().then(heroes => this.heroes = heroes);
    }

    onSelect(hero: Hero): void {
        this.selectedHero = hero;
    }

    gotoDetail(): void {
        this.router.navigate([ '/detail', this.selectedHero.id ]);
    }
}
