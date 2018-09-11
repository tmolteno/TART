import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { FileDownloadComponent } from './file-download.component';

import { FileNameParser } from '../../pipes/parse-file-pipe';
import { TimeElapsedPipe } from '../../pipes/time-elapsed-pipe';

describe('FileDownloadComponent', () => {
  let component: FileDownloadComponent;
  let fixture: ComponentFixture<FileDownloadComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ FileDownloadComponent, FileNameParser, TimeElapsedPipe ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(FileDownloadComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
