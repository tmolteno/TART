import { Component } from '@angular/core';
import { TartService } from '../../services/tart.service';

@Component({
    selector: 'app-status',
    templateUrl: './status.component.html',
    styleUrls: ['./status.component.css'],
    providers: []
})

export class StatusComponent {

    fpgaStatus: Object = {};
    channelsStatus: Object[] = [];

    fpgaStatusVisible: boolean = true;
    channelsVisible: boolean = true;

    constructor(private tartService: TartService) {
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
            .subscribe(data => this.fpgaStatus = data);
    }

    getChannelStatus() {
        this.tartService.getChannelStatus()
            .subscribe(data => this.channelsStatus = data);
    }

    getType(item) {
        return typeof item;
    }
}
