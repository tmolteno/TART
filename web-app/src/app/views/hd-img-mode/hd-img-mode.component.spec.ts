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

import { HdImgModeComponent } from './hd-img-mode.component';

import { AuthService } from '../../services/auth.service';
import { MockAuthService } from '../../testing/mock.auth.service';
import { ModeService } from '../../services/mode.service';

describe('HdImgModeComponent', () => {
  let component: HdImgModeComponent;
  let fixture: ComponentFixture<HdImgModeComponent>;

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
        declarations: [HdImgModeComponent]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(HdImgModeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
