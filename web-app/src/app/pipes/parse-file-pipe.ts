import { Pipe, PipeTransform } from '@angular/core';

@Pipe({ name: 'fileNameParse' })
export class FileNameParser implements PipeTransform {
    transform(value: string) : string {
        if (!value) {
            return '';
        }
        let parts = value.split('/');
        return parts[parts.length - 1];
    }
}
