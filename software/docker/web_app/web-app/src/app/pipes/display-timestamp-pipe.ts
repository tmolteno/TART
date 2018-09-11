import { Pipe, PipeTransform } from '@angular/core';
import * as moment from 'moment/moment';

@Pipe({ name: 'isoFormatTransform' })
export class IsoTimestampDisplay implements PipeTransform {

    transform(value) : string {
        if (!value) {
            return '';
        }
        let gmtDateTime = moment.utc(value);
        return gmtDateTime.local().format("ddd MMM DD YYYY HH:mm:ss");
    }
}
