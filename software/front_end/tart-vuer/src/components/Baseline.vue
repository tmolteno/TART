<template>
<v-flex class="baseline">
    <v-card class="mx-auto" flat outlined elevation="3">

        <v-card-title class="my-0 mx-0 pt-1 pb-0">
            <h4 class="teal--text text--lighten-2 text-uppercase">
                Visibility Amplitude
            </h4>
        </v-card-title>

        <v-btn text x-small color='teal'></v-btn>
        <VueApexCharts height="120" type="line" :options="apex.options" :series="apex_vis_amp" />

        <v-card-title class="my-0 mx-0 pt-0 pb-0">
            <h4 class="teal--text text--lighten-2 text-uppercase">
                Visibility Phase
            </h4>
        </v-card-title>

        <VueApexCharts height="120" type="line" :options="apex_options_phase" :series="apex_vis_phase" />

        <v-card-actions class="pb-0">
            <v-range-slider v-model="sel_baseline" @input="updateBL($event)" label="Baseline" thumb-label="always" :thumb-size="20" min=0 max=23 outlined></v-range-slider>
        </v-card-actions>
    </v-card>
</v-flex>
</template>

<script>
import VueApexCharts from 'vue-apexcharts'

export default {
    name: 'BaselineComponent',
    components: {
        VueApexCharts,
    },
    props: {},
    data: function () {
        return {
            gain: false,
            antennas: false,
            sel_baseline: [0, 23],
            refresher: null,
            curl: null,
            apex: {
                options: {
                    grid: {
                        padding: {
                            top: -20,
                            right: 0,
                            bottom: -25,
                            left: 0
                        },
                    },
                    xaxis: {
                        show: false,
                        type: 'datetime',
                        labels: {
                            datetimeFormatter: {
                                year: 'yyyy',
                                month: 'MMM \'yy',
                                day: 'dd MMM',
                                hour: 'HH:mm:ss',
                                minute: 'HH:mm:ss.ff',
                                // second: 'ss'
                            }
                        },
                        // labels: {
                        //   formatter: function (value, timestamp) {
                        //     return timestamp.getHours() + ' ' + timestamp.getMinutes() // The formatter function overrides format property
                        //   },
                        // }
                    },
                    stroke: {
                        curve: 'smooth',
                        width: 2,
                    },
                    chart: {
                        id: 'vuechart',
                        group: 'vis',
                        toolbar: {
                            show: false,
                        },
                        animations: {
                            enabled: false,
                            easing: 'easeinout',
                            speed: 50,
                            animateGradually: {
                                enabled: false,
                                delay: 50
                            },
                            dynamicAnimation: {
                                enabled: false,
                                speed: 50
                            }
                        }
                    },
                },
            },
        }
    },

    methods: {
        updateBL(val) {
            this.$store.dispatch('newBaseline', val)
        },
        calibrated_vis: function (vis, cal) {
            let gain_corr_real_part = cal.gain[vis.i] * cal.gain[vis.j] * Math.cos(cal.phase_offset[vis.i] - cal.phase_offset[vis.j]);
            let gain_corr_imag_part = -cal.gain[vis.i] * cal.gain[vis.j] * Math.sin(cal.phase_offset[vis.i] - cal.phase_offset[vis.j]);
            let vis_cal_real = vis.re * gain_corr_real_part - vis.im * gain_corr_imag_part;
            let vis_cal_imag = vis.re * gain_corr_imag_part + vis.im * gain_corr_real_part;
            let cal_vis = {
                i: vis.i,
                j: vis.j,
                re: vis_cal_real,
                im: vis_cal_imag
            }
            return cal_vis
        },
    },

    computed: {
        ant_sel_i() {
            return this.sel_baseline[0]
        },
        ant_sel_j() {
            return this.sel_baseline[1]
        },
        cal() {
            return this.$store.state.gain
        },
        vis() {
            return this.$store.state.vis
        },
        apex_options_phase() {
            let options = {
                ...this.apex.options,
                yaxis: {
                    max: 180,
                    min: -180,
                }
            }
            return options
        },
        apex_vis_amp() {
            var series = [{
                name: 'visReal',
                data: []
            }]
            if (this.vis_history.length > 0) {
                series = [{
                    name: 'Uncalibrated Amplitude',
                    data: this.vis_history.map((x_h, idx) => ({
                        x: this.categories[idx],
                        y: x_h.data.filter(x => x.i == this.ant_sel_i && x.j == this.ant_sel_j)
                            .map(x => Math.sqrt(x.re ** 2 + x.im ** 2).toFixed(3))
                    }))
                }]
            }
            return series
        },
        apex_vis_phase() {
            var series = [{
                name: 'Phase',
                data: []
            }]
            if (this.vis_history.length > 0) {
                series = [{
                    name: 'Uncalibrated Phase',
                    data: this.vis_history.map((x_h, idx) => ({
                        x: this.categories[idx],
                        y: x_h.data.filter(x => x.i == this.ant_sel_i && x.j == this.ant_sel_j)
                            .map(x => ((Math.atan2(x.im, x.re) * 180. / Math.PI)).toFixed(0))
                    }))
                }, ]
            }
            return series
        },

        categories() {
            return this.vis_history.map(x_h => x_h.timestamp)
        },

        vis_history() {
            return this.$store.state.vis_history
        }
    },
    mounted: function () {},
}
</script>
