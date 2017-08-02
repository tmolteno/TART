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
import { ChartsModule } from 'ng2-charts/ng2-charts';

import { DiagnoseModeComponent } from './diagnose-mode.component';
import { StatusMapComponent } from
'../../components/status-map/status-map.component';
import { FpgaStatusComponent } from
'../../components/fpga-status/fpga-status.component';
import { ChannelsStatusComponent } from
'../../components/channels-status/channels-status.component';
import { StatusRowHeaderComponent } from
'../../components/status-row-header/status-row-header.component';
import { ChannelCardComponent } from
'../../components/channel-card/channel-card.component';

import { TartService } from '../../services/tart.service';
import { AuthService } from '../../services/auth.service';
import { MockAuthService } from '../../testing/mock.auth.service';
import { ModeService } from '../../services/mode.service';
import { ImagingService } from '../../services/imaging.service';

describe('DiagnoseModeComponent', () => {
  let component: DiagnoseModeComponent;
  let fixture: ComponentFixture<DiagnoseModeComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
        imports: [HttpModule, ChartsModule],
        providers: [
            { provide: XHRBackend, useClass: MockBackend },
            { provide: AuthService, useClass: MockAuthService },
            { provide: Router, useClass: class {
                navigate = jasmine.createSpy('navigate');
            }},
            ModeService,
            TartService,
            ImagingService
        ],
        declarations: [
            DiagnoseModeComponent,
            StatusMapComponent,
            FpgaStatusComponent,
            ChannelsStatusComponent,
            StatusRowHeaderComponent,
            ChannelCardComponent
        ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DiagnoseModeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
