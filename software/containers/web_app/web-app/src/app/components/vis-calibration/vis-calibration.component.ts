import { Component } from '@angular/core';

import { ImagingService } from '../../services/imaging.service';
import { CalibrationService } from '../../services/calibration.service';

import { Observable } from 'rxjs/Rx';
import * as moment from 'moment/moment';

@Component({
    selector: 'app-vis-calibration',
    templateUrl: './vis-calibration.component.html',
    styleUrls: ['./vis-calibration.component.css']
})
export class VisCalibrationComponent {

    elevationSliderLabel: string = 'Elevation';
    elevationSliderMin: number = 10;
    elevationSliderMax: number = 90;
    elevationSliderStepSize: number = 5;
    elevationSliderValue: number = 90;

    azimuthSliderLabel: string = 'Azimuth Degrees';
    azimuthSliderMin: number = 0;
    azimuthSliderMax: number = 360;
    azimuthSliderStepSize: number = 45;
    azimuthSliderValue: number = 0;

    calMeasurementCounter: number = 1;
    calMeasurements: any[] = [];

    constructor(
        private imagingService: ImagingService,
        private calibrationService: CalibrationService) {
    }

    onElevationChanged(value) {
        this.elevationSliderValue = value;
    }

    onAzimuthChanged(value) {
        this.azimuthSliderValue = value;
    }

    onAddCalClicked(event) {
        let vis = null;
        let timestamp = null;


        this.imagingService.getVis().subscribe(result => {
            vis = result;
            timestamp = vis.timestamp;
            let calMeasurement = this.generateCalMeasurement(vis, timestamp);
            this.calMeasurements.push(calMeasurement);
        });


        //this.imagingService.getVis()
        //.catch(() => { return Observable.of({}); })
        //.flatMap(result => {
        //    if (!!result) {
        //        vis = result;
        //    }
        //    return this.imagingService.getTimestamp()
        //  })
        //.catch(() => { return Observable.of(moment().format()); })
        //.subscribe(result => {
        //    if (!!result) {
        //        timestamp = result;
        //   }
        //    let calMeasurement = this.generateCalMeasurement(vis, timestamp);
        //    this.calMeasurements.push(calMeasurement);
        //});
    }

    generateCalMeasurement(vis: any, timestamp: string) {
        return {
            no: this.calMeasurementCounter++,
            el: this.elevationSliderValue,
            az: this.azimuthSliderValue,
            data: {
                vis: vis,
                timestamp: timestamp
            }
        };
    }

    onExportCalClicked(event) {
        this.calibrationService.startCalibration(this.calMeasurements);
    }

    onRemoveCalClicked(calId) {
        this.calMeasurements = this.calMeasurements
            .filter(cal => cal.no != calId);
    }
}
