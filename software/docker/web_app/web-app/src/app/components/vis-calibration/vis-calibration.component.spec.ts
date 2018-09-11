import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpModule } from '@angular/http';

import { VisCalibrationComponent } from './vis-calibration.component';
import { VisiblesConfigSliderComponent } from
'../visibles-config-slider/visibles-config-slider.component';

import { ImagingService } from '../../services/imaging.service';
import { CalibrationService } from '../../services/calibration.service';
import { AuthService } from '../../services/auth.service';

import { NouisliderModule } from 'ng2-nouislider';

describe('VisCalibrationComponent', () => {
  let component: VisCalibrationComponent;
  let fixture: ComponentFixture<VisCalibrationComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
        imports: [ HttpModule, NouisliderModule ],
        providers: [ ImagingService, CalibrationService, AuthService ],
        declarations: [
            VisCalibrationComponent,
            VisiblesConfigSliderComponent
        ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(VisCalibrationComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
