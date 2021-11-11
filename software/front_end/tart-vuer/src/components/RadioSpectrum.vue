<template>
<v-card flat outlined elevation="3">
    <v-card-title class="my-0 mx-0 pt-1 pb-0">
        <h4 class="teal--text text--lighten-2 text-uppercase">
            Radio Spectrum
        </h4>
    </v-card-title>
    <VueApexCharts height="400" type="line" :options="apex.options" :series="series" />
</v-card>
</template>

<script>
import VueApexCharts from "vue-apexcharts";

export default {
    name: "BaselineComponent",
    components: {
        VueApexCharts,
    },

    computed: {
        aMin() {
            return Math.min(...this.series.map(ant => ant.data.map(el => el.y)).flat())
        },
        aMax() {
            return Math.max(...this.series.map(ant => ant.data.map(el => el.y)).flat())
        },
        apex() {
            return {
                options: {
                    grid: {
                        padding: {
                            top: -20,
                            right: 0,
                            bottom: 0,
                            left: 0
                        },
                    },
                    yaxis: {
                        min: this.aMin || -80,
                        max: this.aMax || -50,
                    },
                    stroke: {
                        curve: "smooth",
                        width: 0.8,
                    },
                    chart: {
                        id: "vuechart",
                        toolbar: {
                            show: false,
                        },
                        animations: {
                            enabled: false,
                            easing: "easeinout",
                            speed: 50,
                            animateGradually: {
                                enabled: false,
                                delay: 50,
                            },
                            dynamicAnimation: {
                                enabled: false,
                                speed: 50,
                            },
                        },
                    },
                },
            }
        },

        channels() {
            return this.$store.state.channels;
        },
        series() {
            var series = [];
            if (this.channels.length) {
                series = this.channels.filter(ch => ch.radio_mean.ok).map((ch) => {
                    return {
                        name: ch.id,
                        data: ch.freq.map((fi, xi) => {
                            return {
                                x: fi,
                                y: ch.power[xi],
                            };
                        }).filter((f, fi) => ((fi % 4) === 0)),
                    };
                });
            }
            return series;
        },
    },
};
</script>
