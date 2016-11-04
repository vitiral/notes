
import { Component } from '@angular/core';

import { Hero } from './hero';


@Component({
    moduleId: module.id,
    selector: 'hero-form',
    templateUrl: './hero-form.component.html',
})
export class HeroFormComponent {
    // available powers
    // quick note: if `=` is replaced with `:` it does not generate an error but also
    // doesn't work... apparently a list of strings is itself a type???
    powers = [
        "really smart", "super flexible",
        "super hot", "weather changer"
    ];
    model = new Hero(45, "Windstorm", this.powers[0], "Igor the Farty");
    submitted = false;
    active = true;
    onSubmit() { this.submitted = true }

    newHero() {
        this.model = new Hero(42, "", "");
        // blink active false -> true
        this.active = false;
        setTimeout(() => this.active = true, 0);
    }

    // `get` is js's equivalent to pyton's `property`
    get diagnostic() { return JSON.stringify(this); }
}
