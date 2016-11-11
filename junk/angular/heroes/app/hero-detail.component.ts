import { Component, Input, OnInit } from '@angular/core';
import { ActivatedRoute, Params } from '@angular/router';
import { Location } from '@angular/common';

import { Hero } from './hero';
import { HeroService } from './hero.service';

@Component({
    moduleId: module.id,
    selector: 'my-hero-detail',
    templateUrl: '../html/hero-detail.component.html',
    providers: [ HeroService ],
})
export class HeroDetailComponent implements OnInit {
    constructor(
        private heroService: HeroService,
        private route: ActivatedRoute,
        private location: Location,
    ) {}

    ngOnInit(): void {
        this.route.params.forEach((params: Params) => {
            let id = +params['id'];  // + converts str -> number
            this.heroService.getHero(id)
                .then(hero => this.hero = hero);
        })
    }

    save(): void {
        this.heroService.update(this.hero)
            .then(() => this.goBack());
    }

    // go back one step in the browser's history
    goBack(): void {
        this.location.back();
    }

    @Input()
    hero: Hero;


}
