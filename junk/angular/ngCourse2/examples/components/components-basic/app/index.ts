import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { platformBrowserDynamic } from '@angular/platform-browser-dynamic';
import { Hello } from './app.component';

@NgModule({
  imports: [
    BrowserModule
  ],
  declarations: [
    Hello
  ],
  bootstrap: [ Hello ]
})
export class AppModule {
}
platformBrowserDynamic().bootstrapModule(AppModule);
