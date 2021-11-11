<template>
<g class="grid">
    <circle v-for="r_i in r" :key="r_i" fill="transparent" :cx="num_bin/2" :cy="num_bin/2" :r="r_i" style="stroke-width:1;stroke:rgb(0,0,0);"></circle>
    <line :x1="num_bin/2" y1="0" :x2="num_bin/2" :y2="num_bin" stroke="black" />
    <line x1="0" :y1="num_bin/2" :x2="num_bin" :y2="num_bin/2" stroke="black" />
    <text class="heavy" text-anchor="middle" dominant-baseline="middle" v-for="l in orientationLabels" :key="l.text" :x="l.loc.x" :y="l.loc.y">{{l.text}}</text>
</g>
</template>

<script>
import syn from '@/plugins/api_synthesis.js';

export default {
    name: "GridOverlay",
    props: {
        dims: String,
    },
    data: function () {
        return {};
    },

    methods: {},
    computed: {
        orientationLabels() {
            var ret = [];
            ret.push({
                loc: syn.horizontal_2_px(80, 0, this.nw, this.num_bin),
                text: "N",
            });
            ret.push({
                loc: syn.horizontal_2_px(80, 90, this.nw, this.num_bin),
                text: "E",
            });
            ret.push({
                loc: syn.horizontal_2_px(80, 180, this.nw, this.num_bin),
                text: "S",
            });
            ret.push({
                loc: syn.horizontal_2_px(80, 270, this.nw, this.num_bin),
                text: "W",
            });
            return ret;
        },
        r: function () {
            var ret = [];
            let angles = [80, 70, 60, 50, 40, 30, 20, 10, 0];
            for (var r_i = 0; r_i < angles.length; r_i++) {
                ret.push(
                    syn.proj_ang_2_px(90 - angles[r_i], this.nw, this.num_bin)
                );
            }
            return ret;
        },
        num_bin() {
            return this.dims; //this.$store.state.num_bin
        },
        nw() {
            return this.$store.state.nw;
        },
    },
    mounted: function () {},
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->

<style scoped>
.heavy {
    font: 16px sans-serif;
    fill: white;
    outline: 1px;
    outline-color: black;
}
</style>
