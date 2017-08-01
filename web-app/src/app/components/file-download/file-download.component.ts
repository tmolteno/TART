import { Component, Input, SimpleChanges } from '@angular/core';
import { PlatformLocation } from '@angular/common';

import * as moment from 'moment/moment';

@Component({
    selector: 'app-file-download',
    templateUrl: './file-download.component.html',
    styleUrls: ['./file-download.component.css']
})
export class FileDownloadComponent {

    @Input()
    filePaths: any[] = [];

    @Input()
    timeNow: string;

    baseLocation: string = window.location.protocol + '//' + window.location.host;
}
