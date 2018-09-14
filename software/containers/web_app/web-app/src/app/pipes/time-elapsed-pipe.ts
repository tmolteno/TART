import { Pipe, PipeTransform } from '@angular/core';
import * as moment from 'moment/moment';

@Pipe({ name: 'timeElapsed' })
export class TimeElapsedPipe implements PipeTransform {
  transform(timeEnd: string, timeStart: string): string {
    if(!timeEnd || !timeStart) {
        return 'unknown';
    }

    let start = moment(timeStart);
    let end = moment(timeEnd);

    let diff = end.diff(start, 'minutes');
    if (diff < 1) {
        return '< 1 minute';
    } else if (diff == 1) {
        return '1 minute';
    } else if (diff < 60) {
        return `${diff} minutes`;
    }
    diff = end.diff(start, 'hours');
    if (diff === 1) {
        return '1 hour';
    } else if (diff < 24) {
        return `${diff} hours`;
    }
    diff = end.diff(start, 'days');
    if (diff === 1) {
        return '1 day';
    }
    return `${diff} days`;
  }
}
