import { RouterModule, Routes } from '@angular/router';
import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';
import { NgLoadingBarModule } from 'ng-loading-bar';

import { KeysPipe } from './keys-pipe';
/** Services */
import { ModeService } from './services/mode.service';
import { AuthService } from './services/auth.service';
import { TartService } from './services/tart.service';

import { AppComponent } from './app.component';
/** Views */
import { ModeComponent } from './views/mode/mode.component';
import { LoginComponent } from './views/login/login.component';

import { OffModeComponent } from './views/off-mode/off-mode.component';
import { DiagnoseModeComponent } from './views/diagnose-mode/diagnose-mode.component';
import { RawModeComponent } from './views/raw-mode/raw-mode.component';
import { VisModeComponent } from './views/vis-mode/vis-mode.component';
import { CalModeComponent } from './views/cal-mode/cal-mode.component';
import { HdImgModeComponent } from './views/hd-img-mode/hd-img-mode.component';
import { HomeComponent } from './views/home/home.component';
/** Components */
import { StatusRowHeaderComponent } from './components/status-row-header/status-row-header.component';
import { NavBarComponent } from './components/nav-bar/nav-bar.component';
import { FooterComponent } from './components/footer/footer.component';
/** Routes */
const appRoutes = [
    {
        path: 'home',
        component: HomeComponent
    },
    {
        path: 'off-mode',
        component: OffModeComponent
    },
    {
        path: 'diag-mode',
        component: DiagnoseModeComponent
    },
    {
        path: 'raw-data-mode',
        component: RawModeComponent
    },
    {
        path: 'vis-data-mode',
        component: VisModeComponent
    },
    {
        path: 'calibrate-mode',
        component: CalModeComponent
    },
    {
        path: 'hd-img-mode',
        component: HdImgModeComponent
    },
    {
        path: 'login',
        component: LoginComponent
    },
    {
        path: '',
        redirectTo: '/home',
        pathMatch: 'full'
    }
]

@NgModule({
  declarations: [
    AppComponent,
    KeysPipe,
    StatusRowHeaderComponent,
    NavBarComponent,
    LoginComponent,
    FooterComponent,
    ModeComponent,
    OffModeComponent,
    DiagnoseModeComponent,
    RawModeComponent,
    VisModeComponent,
    CalModeComponent,
    HdImgModeComponent,
    HomeComponent
  ],
  imports: [
    RouterModule.forRoot(appRoutes),
    NgLoadingBarModule.forRoot(),
    BrowserModule,
    FormsModule,
    HttpModule
  ],
  providers: [
      TartService,
      AuthService,
      ModeService
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
