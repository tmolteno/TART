import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpModule } from '@angular/http';

import { FpgaStatusComponent } from './fpga-status.component';
import { StatusRowHeaderComponent }
from '../status-row-header/status-row-header.component';
import { FileNameParser } from '../../pipes/parse-file-pipe';

import { TartService } from '../../services/tart.service';
import { AuthService } from '../../services/auth.service';

describe('FpgaStatusComponent', () => {
  let component: FpgaStatusComponent;
  let fixture: ComponentFixture<FpgaStatusComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
        imports: [ HttpModule ],
        providers: [ TartService, AuthService ],
        declarations: [
            FpgaStatusComponent,
            StatusRowHeaderComponent,
            FileNameParser
        ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(FpgaStatusComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
