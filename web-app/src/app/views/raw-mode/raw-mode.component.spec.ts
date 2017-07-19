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

import { RawModeComponent } from './raw-mode.component';
import { DataAcquisitionDisplayComponent } from
'../../components/data-acquisition-display/data-acquisition-display.component';

import { AuthService } from '../../services/auth.service';
import { MockAuthService } from '../../testing/mock.auth.service';
import { ModeService } from '../../services/mode.service';
import { InfoService } from '../../services/info.service';
import { DataAcquisitionService } from '../../services/data-acquisition.service';

describe('RawModeComponent', () => {
  let component: RawModeComponent;
  let fixture: ComponentFixture<RawModeComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [HttpModule],
      providers: [
          { provide: XHRBackend, useClass: MockBackend },
          { provide: AuthService, useClass: MockAuthService },
          { provide: Router, useClass: class {
              navigate = jasmine.createSpy('navigate');
          }},
          ModeService,
          InfoService,
          DataAcquisitionService
      ],
      declarations: [
          RawModeComponent,
          DataAcquisitionDisplayComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(RawModeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
