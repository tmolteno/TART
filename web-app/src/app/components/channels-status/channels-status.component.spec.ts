import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { ChartsModule } from 'ng2-charts/ng2-charts';

import { ChannelsStatusComponent } from './channels-status.component';
import { ChannelCardComponent } from '../channel-card/channel-card.component';
import { StatusRowHeaderComponent }
from '../status-row-header/status-row-header.component';

describe('ChannelsStatusComponent', () => {
  let component: ChannelsStatusComponent;
  let fixture: ComponentFixture<ChannelsStatusComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
        imports: [ ChartsModule ],
        declarations: [
            ChannelsStatusComponent,
            StatusRowHeaderComponent,
            ChannelCardComponent
        ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ChannelsStatusComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
