/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed, inject } from '@angular/core/testing';
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

import { CalModeComponent } from './cal-mode.component';

import { AuthService } from '../../services/auth.service';
import { MockAuthService } from '../../testing/mock.auth.service';
import { ModeService } from '../../services/mode.service';

describe('CalModeComponent', () => {
  let component: CalModeComponent;
  let fixture: ComponentFixture<CalModeComponent>;
  let authService: AuthService;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [HttpModule],
      providers: [
          { provide: XHRBackend, useClass: MockBackend },
          { provide: AuthService, useClass: MockAuthService },
          { provide: Router, useClass: class {
              navigate = jasmine.createSpy('navigate');
          }},
          ModeService
      ],
      declarations: [CalModeComponent]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CalModeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
      expect(component).toBeTruthy();
  });
});
