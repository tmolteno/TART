import { RouterModule, Routes } from '@angular/router';
import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';


import { TartService } from './services/tart.service';
import { KeysPipe } from './keys-pipe';

import { AppComponent } from './app.component';
/** Views */
import { StatusComponent } from './views/status/status.component';
import { LoginComponent } from './views/login/login.component';
/** Components */
import { StatusRowHeaderComponent } from './components/status-row-header/status-row-header.component';
import { NavBarComponent } from './components/nav-bar/nav-bar.component';
import { FooterComponent } from './components/footer/footer.component';
/** Routes */
const appRoutes = [
    {
        path: 'status',
        component: StatusComponent
    },
    {
        path: 'login',
        component: LoginComponent
    },
    {
        path: '',
        redirectTo: '/status',
        pathMatch: 'full'
    }
]

@NgModule({
  declarations: [
    AppComponent,
    KeysPipe,
    StatusComponent,
    StatusRowHeaderComponent,
    NavBarComponent,
    LoginComponent,
    FooterComponent
  ],
  imports: [
    RouterModule.forRoot(appRoutes),
    BrowserModule,
    FormsModule,
    HttpModule
  ],
  providers: [TartService],
  bootstrap: [AppComponent]
})
export class AppModule { }
