/* tslint:disable:no-unused-variable */
import {APP_BASE_HREF} from '@angular/common';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { PopoverModule } from 'ngx-popover';
import { RouterModule } from '@angular/router';
import { NgLoadingBarModule } from 'ng-loading-bar';

import { LoginComponent } from '../../views/login/login.component';
import { NavBarComponent } from './nav-bar.component';

import { AuthService } from '../../services/auth.service';
import { InfoService } from '../../services/info.service';
import { ModeService } from '../../services/mode.service';

describe('NavBarComponent', () => {
  let component: NavBarComponent;
  let fixture: ComponentFixture<NavBarComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
        imports: [
            PopoverModule,
            NgLoadingBarModule,
            FormsModule,
            RouterModule.forRoot([])
        ],
        declarations: [ NavBarComponent, LoginComponent ],
        providers: [
            {provide: APP_BASE_HREF, useValue : '/' },
            AuthService,
            InfoService,
            ModeService
        ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(NavBarComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
