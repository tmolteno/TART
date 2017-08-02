/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { Router } from '@angular/router';
import { DebugElement } from '@angular/core';
import {
     HttpModule,
     Http,
     Response,
     ResponseOptions,
     XHRBackend
} from '@angular/http';
import { MockBackend } from '@angular/http/testing';
import { FormsModule } from '@angular/forms';
import { TimeElapsedPipe } from '../../pipes/time-elapsed-pipe';
import { FileNameParser } from '../../pipes/parse-file-pipe';

import { HomeComponent } from './home.component';
import { InfoComponent } from '../../components/info/info.component';
import { ImagingComponent } from '../../components/imaging/imaging.component';
import { FpgaStatusComponent } from '../../components/fpga-status/fpga-status.component';
import { VisiblesConfigSliderComponent } from
'../../components/visibles-config-slider/visibles-config-slider.component';
import { StatusRowHeaderComponent } from
'../../components/status-row-header/status-row-header.component';
import { FileDownloadComponent } from
'../../components/file-download/file-download.component';
import { GifRecorderComponent } from
'../../components/gif-recorder/gif-recorder.component';


import { IsoTimestampDisplay } from  '../../pipes/display-timestamp-pipe';
import { NouisliderModule } from 'ng2-nouislider';

import { ModeService } from '../../services/mode.service';
import { MockAuthService } from '../../testing/mock.auth.service';
import { AuthService } from '../../services/auth.service';
import { TartService } from '../../services/tart.service';
import { ImagingService } from '../../services/imaging.service';
import { InfoService } from '../../services/info.service';
import { CalibrationService } from '../../services/calibration.service';
import { DataAcquisitionService } from '../../services/data-acquisition.service';
import { ColourService } from '../../services/colour.service';
import { CatalogService } from '../../services/catalog.service';

describe('HomeComponent', () => {
  let component: HomeComponent;
  let fixture: ComponentFixture<HomeComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
        imports: [NouisliderModule, HttpModule, FormsModule],
        providers: [
            { provide: XHRBackend, useClass: MockBackend },
            { provide: AuthService, useClass: MockAuthService },
            { provide: Router, useClass: class {
                navigate = jasmine.createSpy('navigate');
            }},
            ModeService,
            InfoService,
            TartService,
            ImagingService,
            CalibrationService,
            DataAcquisitionService,
            ColourService,
            CatalogService
        ],
        declarations: [
            HomeComponent,
            InfoComponent,
            ImagingComponent,
            FpgaStatusComponent,
            VisiblesConfigSliderComponent,
            IsoTimestampDisplay,
            StatusRowHeaderComponent,
            FileDownloadComponent,
            GifRecorderComponent,
            TimeElapsedPipe,
            FileNameParser
        ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(HomeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
