import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { TartService } from '../../services/tart.service';

@Component({
    selector: 'app-status',
    templateUrl: './status.component.html',
    styleUrls: ['./status.component.css'],
    providers: []
})

export class StatusComponent implements OnInit {

    fpgaStatus: Object = {};
    channelsStatus: Object[] = [];

    fpgaStatusVisible: boolean = true;
    channelsVisible: boolean = true;

    constructor(
        private tartService: TartService,
        private ref: ChangeDetectorRef
    ) { }

    ngOnInit() {
        this.getFpgaStatus();
        this.getChannelStatus();
    }

    toggleFpgaStatus(event) {
        this.fpgaStatusVisible = !this.fpgaStatusVisible;
    }

    toggleChannelStatus(event) {
        this.channelsVisible = !this.channelsVisible;
    }

    getFpgaStatus() {
        this.tartService.getFpgaStatus()
            .subscribe(result => {
                this.fpgaStatus = result;
                this.ref.detectChanges();
            });
    }

    getChannelStatus() {
        this.tartService.getChannelStatus()
            .subscribe(result => {
                this.channelsStatus = result;
                this.ref.detectChanges();
            });
    }

    getType(item) {
        return typeof item;
    }
}
