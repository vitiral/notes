import { NgModule }      from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';

import { AppCmt, MyUserInputCmt  }  from './app.component';

@NgModule({
  imports:      [ BrowserModule ],
  declarations: [ AppCmt, MyUserInputCmt ],
  bootstrap:    [ AppCmt ]
})
export class AppModule { }
