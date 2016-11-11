//! this is the entry point for the app

import './rxjs-extensions';

import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';

// the routing module provides facilities to change where the
// app points (the url address)
import { AppRoutingModule } from './app-routing.module';

// Imports for loading & configuring the in-memory web api
import { InMemoryWebApiModule } from 'angular-in-memory-web-api';
import { InMemoryDataService }  from './in-memory-data.service';

// the app component is the base view that all components share
// it has the title and navigation (nav) links
import { AppComponent } from './app.component';

// the dashboard is the view for "top heroes" or whatever
import { DashboardComponent } from './dashboard.component';

// the HeroesComponent is the view that lists all heroes
import { HeroesComponent } from './heroes.component';

// the HeroDetailComponet is the "drilled-down" detail and editting interface
import { HeroDetailComponent } from './hero-detail.component';

import { HeroService } from './hero.service';
import { HeroSearchService } from './hero-search.service'

import { HeroSearchComponent } from './hero-search.component';

// NgModule decarator declares the class as a "module"
// A module is a consolodation of components similar to a python module.
// You can completely define what is accessible within the module and what
// is exported by it
//
// Modules are a good way to break up an app into features or to export
// functionality in a library
@NgModule({
    imports: [
        BrowserModule,
        FormsModule,
        HttpModule,
        InMemoryWebApiModule.forRoot(InMemoryDataService),
        AppRoutingModule,
    ],
    declarations: [
        AppComponent,
        DashboardComponent,
        HeroDetailComponent,
        HeroesComponent,
        HeroSearchComponent,
    ],
    providers: [ HeroService, HeroSearchService ],
    bootstrap: [ AppComponent ],
})
export class AppModule { }
