import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';
import { NgLoadingBarModule } from 'ng-loading-bar';
import { PopoverModule } from 'ngx-popover';
import { IsoTimestampDisplay } from './pipes/display-timestamp-pipe';
import { KeysPipe } from './pipes/keys-pipe';
import { DecimalPipe } from '@angular/common';
import { NouisliderModule } from 'ng2-nouislider';
import { ChartsModule } from 'ng2-charts/ng2-charts';
/** Services */
import { ModeService } from './services/mode.service';
import { AuthService } from './services/auth.service';
import { TartService } from './services/tart.service';
import { ImagingService } from './services/imaging.service';
import { InfoService } from './services/info.service';
import { DataAcquisitionService } from './services/data-acquisition.service';
import { CalibrationService } from './services/calibration.service';
import { ColourService } from './services/colour.service';
import { CatalogService } from './services/catalog.service';
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
import { AppComponent } from './app.component';
import { StatusRowHeaderComponent } from './components/status-row-header/status-row-header.component';
import { NavBarComponent } from './components/nav-bar/nav-bar.component';
import { FooterComponent } from './components/footer/footer.component';
import { ImagingComponent } from './components/imaging/imaging.component';
import { VisiblesConfigSliderComponent } from './components/visibles-config-slider/visibles-config-slider.component';
import { InfoComponent } from './components/info/info.component';
import { FpgaStatusComponent } from './components/fpga-status/fpga-status.component';
import { ChannelsStatusComponent } from './components/channels-status/channels-status.component';
import { ChannelCardComponent } from './components/channel-card/channel-card.component';
import { DataAcquisitionDisplayComponent } from './components/data-acquisition-display/data-acquisition-display.component';
import { StatusMapComponent } from './components/status-map/status-map.component';
import { GifRecorderComponent } from './components/gif-recorder/gif-recorder.component';
import { VisCalibrationComponent } from './components/vis-calibration/vis-calibration.component';
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
        redirectTo: 'home',
        pathMatch: 'full'
    }
]

@NgModule({
  declarations: [
    AppComponent,
    KeysPipe,
    IsoTimestampDisplay,
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
    HomeComponent,
    ImagingComponent,
    VisiblesConfigSliderComponent,
    InfoComponent,
    FpgaStatusComponent,
    ChannelsStatusComponent,
    ChannelCardComponent,
    DataAcquisitionDisplayComponent,
    StatusMapComponent,
    GifRecorderComponent,
    VisCalibrationComponent
  ],
  imports: [
    RouterModule.forRoot(appRoutes),
    NgLoadingBarModule.forRoot(),
    BrowserModule,
    FormsModule,
    HttpModule,
    PopoverModule,
    NouisliderModule,
    ChartsModule
  ],
  providers: [
      TartService,
      AuthService,
      ModeService,
      ImagingService,
      InfoService,
      DataAcquisitionService,
      CalibrationService,
      ColourService,
      CatalogService
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
