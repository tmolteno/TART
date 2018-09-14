/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';
import { FormsModule } from '@angular/forms';
import {
     HttpModule,
     Http,
     Response,
     ResponseOptions,
     XHRBackend
} from '@angular/http';
import { MockBackend } from '@angular/http/testing';

import { LoginComponent } from './login.component';

import { AuthService } from '../../services/auth.service';
import { MockAuthService } from '../../testing/mock.auth.service';
import { ModeService } from '../../services/mode.service';

describe('LoginComponent', () => {
  let component: LoginComponent;
  let fixture: ComponentFixture<LoginComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
        imports: [ FormsModule, HttpModule ],
        providers: [
            { provide: XHRBackend, useClass: MockBackend },
            { provide: AuthService, useClass: MockAuthService },
            { provide: Router, useClass: class {
                navigate = jasmine.createSpy('navigate');
            }},
            ModeService
        ],
        declarations: [ LoginComponent ],

    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(LoginComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
