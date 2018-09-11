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

import { VisModeComponent } from './vis-mode.component';
import { ImagingComponent } from
'../../components/imaging/imaging.component';
import { DataAcquisitionDisplayComponent } from
'../../components/data-acquisition-display/data-acquisition-display.component';
import { VisiblesConfigSliderComponent } from
'../../components/visibles-config-slider/visibles-config-slider.component';
import { VisCalibrationComponent } from
'../../components/vis-calibration/vis-calibration.component';
import { GifRecorderComponent } from
'../../components/gif-recorder/gif-recorder.component';

import { IsoTimestampDisplay } from  '../../pipes/display-timestamp-pipe';
import { NouisliderModule } from 'ng2-nouislider';

import { AuthService } from '../../services/auth.service';
import { MockAuthService } from '../../testing/mock.auth.service';
import { ModeService } from '../../services/mode.service';
import { InfoService } from '../../services/info.service';
import { DataAcquisitionService } from '../../services/data-acquisition.service';
import { ImagingService } from '../../services/imaging.service';
import { ColourService } from '../../services/colour.service';
import { CalibrationService } from '../../services/calibration.service';
import { CatalogService } from '../../services/catalog.service';

describe('VisModeComponent', () => {
  let component: VisModeComponent;
  let fixture: ComponentFixture<VisModeComponent>;

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
          DataAcquisitionService,
          ImagingService,
          CalibrationService,
          ColourService,
          CatalogService
      ],
      declarations: [
          VisModeComponent,
          DataAcquisitionDisplayComponent,
          ImagingComponent,
          GifRecorderComponent,
          VisCalibrationComponent,
          VisiblesConfigSliderComponent,
          IsoTimestampDisplay
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(VisModeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
