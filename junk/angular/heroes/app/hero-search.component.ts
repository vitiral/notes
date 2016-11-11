import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Observable } from 'rxjs/Observable';
import { Subject } from 'rxjs/Subject';

import { HeroSearchService } from './hero-search.service';
import { Hero } from './hero';

@Component({
    moduleId: module.id,
    selector: 'hero-search',
    templateUrl: '../html/hero-search.component.html',
    styleUrls: [ '../html/hero-search.component.css' ],
})
export class HeroSearchComponent implements OnInit {
    heroes: Observable<Hero[]>;
    private searchTerms = new Subject<string>();

    constructor(
        private heroSearchService: HeroSearchService,
        private router: Router,
    ) {}

    // push a search term into Observable's stream
    search(term: string): void {
        this.searchTerms.next(term);
    }

    ngOnInit(): void {
        this.heroes = this.searchTerms
            .debounceTime(300)          // wait for 300ms pause in events
            .distinctUntilChanged()     // ignore if next search term is same as prev
            .switchMap(term => {
                console.log(`Got term: ${term}`);
                if (term) {     // switch to new observable each time
                    return this.heroSearchService.search(term);
                } else {
                    return Observable.of<Hero[]>([]);
                }
            })
            .catch(error => {
                // TODO: real error handling
                console.log(error);
                return Observable.of<Hero[]>([]);
            });
    }

    gotoDetail(hero: Hero): void {
        let link = [ '/detail', hero.id ];
        this.router.navigate(link);
    }
}
