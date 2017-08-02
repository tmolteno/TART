/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { NouisliderModule } from 'ng2-nouislider';
import { HttpModule } from '@angular/http';

import { ImagingComponent } from './imaging.component';
import { VisiblesConfigSliderComponent } from
'../visibles-config-slider/visibles-config-slider.component';
import { GifRecorderComponent } from '../gif-recorder/gif-recorder.component';
import { IsoTimestampDisplay } from '../../pipes/display-timestamp-pipe';

import { InfoService } from '../../services/info.service';
import { ImagingService } from '../../services/imaging.service';
import { CalibrationService } from '../../services/calibration.service';
import { AuthService } from '../../services/auth.service';
import { ColourService } from '../../services/colour.service';
import { CatalogService } from '../../services/catalog.service';

describe('ImagingComponent', () => {
  let component: ImagingComponent;
  let fixture: ComponentFixture<ImagingComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
        imports: [ FormsModule, NouisliderModule, HttpModule ],
        providers: [
            ImagingService,
            CalibrationService,
            AuthService,
            ColourService,
            InfoService,
            CatalogService
        ],
        declarations: [
            ImagingComponent,
            VisiblesConfigSliderComponent,
            GifRecorderComponent,
            IsoTimestampDisplay
        ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ImagingComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
