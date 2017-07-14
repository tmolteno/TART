/**
 * Channel Status data model.
 * @author Tim Miller
 */
export class ChannelStatus {
    public id: number;
    public phase: any; // { N_samples: number, measured: number, ok: number, stability: number, threshold: number }
    public radioMean: any; // { mean: number, ok: number, threshold: number }

    constructor(id: number, phase: any, radioMean: any) {
        this.id = id;
        this.phase = phase;
        this.radioMean = radioMean;
    }
}
