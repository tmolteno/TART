<template>
<g class="satellite">
    <circle fill="transparent" :cx="sat.pxpos.x" :cy="sat.pxpos.y" :r="satR" stroke-width="1" stroke="red">
    </circle>
    <text text-anchor="middle" dominant-baseline="middle" :x="sat.pxpos.x" :y="sat.pxpos.y+16" fill="white" font-weight="800" font-size="10" class='label'> {{sat.name.split(' (')[0]}} </text>
</g>
</template>

<script>
import syn from '@/plugins/api_synthesis.js';

export default {
    name: 'Satellite',
    props: {
        sat: Object,
    },
    data: function () {
        return {}
    },

    methods: {},
    computed: {
        satR() {
            return syn.proj_ang_2_px(2.0, this.nw, this.num_bin)
        },
        num_bin() {
            return this.$store.state.num_bin
        },
        nw() {
            return this.$store.state.nw
        }

    },
    mounted: function () {

    }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->

<style scoped>
.label {
    paint-order: stroke;
    stroke: #000000;
    stroke-width: 1.5px;
}
</style>
