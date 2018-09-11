import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpModule } from '@angular/http';
import { NouisliderModule } from 'ng2-nouislider';

import { InfoComponent } from './info.component';
import { VisiblesConfigSliderComponent } from
'../visibles-config-slider/visibles-config-slider.component';

import { InfoService } from '../../services/info.service';
import { ModeService } from '../../services/mode.service';
import { AuthService } from '../../services/auth.service';

describe('InfoComponent', () => {
  let component: InfoComponent;
  let fixture: ComponentFixture<InfoComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
        imports: [ HttpModule, NouisliderModule ],
        declarations: [
            InfoComponent,
            VisiblesConfigSliderComponent
        ],
        providers: [ InfoService, ModeService, AuthService ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(InfoComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
