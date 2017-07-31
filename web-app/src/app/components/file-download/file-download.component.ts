import { Component, Input } from '@angular/core';
import { PlatformLocation } from '@angular/common';

@Component({
    selector: 'app-file-download',
    templateUrl: './file-download.component.html',
    styleUrls: ['./file-download.component.css']
})
export class FileDownloadComponent {

    @Input()
    filePaths: any[] = [];

    baseLocation: string = window.location.host;
}
