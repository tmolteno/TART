import { Component, Input, Output, EventEmitter } from '@angular/core';

@Component({
    selector: 'app-status-row-header',
    templateUrl: './status-row-header.component.html',
    styleUrls: ['./status-row-header.component.css']
})
export class StatusRowHeaderComponent {

    @Input()
    headerTitle: string;

    @Input()
    contentVisible: boolean;

    @Output()
    toggleVisible = new EventEmitter();

    constructor() { }

    onToggleVisibleClick() {
        this.toggleVisible.emit();
    }
}
