/**
 * FPGA Status data model.
 * @author Tim Miller
 */
export class FpgaStatus {
    public hostname: string;
    public timestamp: string;
    public aqStream: any;
    public aqSystem: any;
    public spiStats: any;
    public sysStats: any;
    public tcCentre: any;
    public tcDebug: any;
    public tcStatus: any;
    public tcSystem: any;
    public vxDebug: any;
    public vxStatus: any;
    public vxStream: any;
    public vxSystem: any;

    constructor(hostname: string, timestamp: string, aqStream: any, aqSystem: any,
                spiStats: any, sysStats: any, tcCentre: any, tcDebug: any, tcStatus: any,
                tcSystem: any, vxDebug: any, vxStatus: any,vxStream: any,
                vxSystem: any) {
        this.hostname = hostname;
        this.timestamp = timestamp;
        this.aqStream = aqStream;
        this.aqSystem = aqSystem;
        this.spiStats = spiStats;
        this.sysStats = sysStats;
        this.tcCentre = tcCentre;
        this.tcDebug = tcDebug;
        this.tcStatus = tcStatus;
        this.tcSystem = tcSystem;
        this.vxDebug = vxDebug;
        this.vxStatus = vxStatus;
        this.vxStream = vxStream;
        this.vxSystem = vxSystem;
    }
}
