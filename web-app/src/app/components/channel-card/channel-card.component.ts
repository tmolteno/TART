import {
    Component,
    Input,
    Output,
    ViewChild,
    ElementRef,
    EventEmitter
} from '@angular/core';

import { ChannelStatus } from '../../models/ChannelStatus';

@Component({
    selector: 'app-channel-card',
    templateUrl: './channel-card.component.html',
    styleUrls: ['./channel-card.component.css']
})
export class ChannelCardComponent {
    @ViewChild('channelCardCheckbox') channelCardCheckbox : ElementRef;

    @Input()
    channel : ChannelStatus;

    @Output()
    antennaEnabledChange = new EventEmitter();

    public lineChartData: Array<any> = [];
    public lineChartLabels: any[] = [];
    public lineChartColors: Array<any> = [
        {
             backgroundColor: 'rgba(66, 139, 202, 0.5)',
             borderColor: 'rgba(66, 139, 202, 1)',
             pointBackgroundColor: 'rgba(148, 159, 177, 1)',
             pointBorderColor: '#fff',
             pointHoverBackgroundColor: '#fff',
             pointHoverBorderColor: 'rgba(148, 159, 177, 0.8)'
         }
    ];
    public chartOptions: any = {
        scales: {
            yAxes:[{
                scaleLabel: {
                    display: true,
                    labelString: 'Magnitude [dB/Hz]',
                    fontColor: '#000'
                },
                ticks: {
                    fontColor: '#000'
                }
            }],
            xAxes:[{
                scaleLabel: {
                    display: true,
                    labelString: 'Frequency [MHz]',
                    fontColor: '#000'
                },
                ticks: {
                    fontColor: '#000'
                }
            }]
        }
    };

    ngOnInit() {
        this.generateChannelChart();
    }

    ngAfterViewInit() {
        this.channelCardCheckbox.nativeElement.checked = this.channel.enabled;
    }

    onChannelEnabledChanged(event) {
        this.channel.enabled = this.channelCardCheckbox.nativeElement.checked;
        this.antennaEnabledChange.emit({id: this.channel.id,
            enabled: this.channel.enabled});
    }

    generateChannelChart() {
        this.lineChartLabels = this.channel.freq;
        this.lineChartData = [{ data: this.channel.power, pointRadius: 0 }];

    }
}
