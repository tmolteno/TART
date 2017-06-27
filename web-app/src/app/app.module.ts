import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';


import { TartService } from './services/tart.service';
import { KeysPipe } from './keys-pipe';
import { AppComponent } from './app.component';
import { StatusRowHeaderComponent } from './status-row-header/status-row-header.component';
import { NavBarComponent } from './nav-bar/nav-bar.component';

@NgModule({
  declarations: [
    AppComponent,
    KeysPipe,
    StatusRowHeaderComponent,
    NavBarComponent
  ],
  imports: [
    BrowserModule,
    FormsModule,
    HttpModule
  ],
  providers: [TartService],
  bootstrap: [AppComponent]
})
export class AppModule { }
