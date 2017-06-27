import { Component } from '@angular/core';
import { TartService } from './services/tart.service';

@Component({
    selector: 'app-root',
    templateUrl: './app.component.html',
    styleUrls: ['./app.component.css'],
    providers: []
})

export class AppComponent {

    fpgaStatus: Object = {};
    channelsStatus: Object[] = [];

    fpgaStatusVisible: boolean = true;
    channelsVisible: boolean = false;

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
            .subscribe(data => {
                this.channelsStatus = data;
            });
    }

    getType(item) {
        return typeof item;
    }
}
