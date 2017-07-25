/**
 * Channel Status data model.
 * @author Tim Miller
 */
export class ChannelStatus {
    public id: number;
    public enabled: boolean;
    public phase: any; // { N_samples: number, measured: number, ok: number, stability: number, threshold: number }
    public radioMean: any; // { mean: number, ok: number, threshold: number }

    constructor(id: number, enabled: boolean, phase: any, radioMean: any) {
        this.id = id;
        this.enabled = enabled;
        this.phase = phase;
        this.radioMean = radioMean;
    }
}
