/**
 * Channel Status data model.
 * @author Tim Miller
 */
export class ChannelStatus {
    public id: number;
    public enabled: boolean;
    public phase: any; // { N_samples: number, measured: number, ok: number, stability: number, threshold: number }
    public radioMean: any; // { mean: number, ok: number, threshold: number }
    public freq: number[];  // x axis
    public power: number[]; // y axis

    constructor(id: number, enabled: boolean, phase: any, radioMean: any,
        freq: number[], power: number[]) {
        this.id = id;
        this.enabled = enabled;
        this.phase = phase;
        this.radioMean = radioMean;
        this.freq = freq;
        this.power = power;
    }
}
