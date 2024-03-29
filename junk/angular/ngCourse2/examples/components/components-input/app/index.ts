import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { platformBrowserDynamic } from '@angular/platform-browser-dynamic';
import { App } from './app.component';
import { Hello } from './hello.component';

@NgModule({
  imports: [
    BrowserModule
  ],
  declarations: [
    App,
    Hello
  ],
  bootstrap: [ App ]
})
export class AppModule {
}
platformBrowserDynamic().bootstrapModule(AppModule);
