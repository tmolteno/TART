<template>
  <v-flex class="baseline">
    <v-card
      class="mx-auto"
      :flat="true"
      :outlined="true"
      :raised="true"
      elevation="5"
      width="512"
      >

      <v-card-title>
        <v-btn  text x-small  color='teal'>Visibility Amplitude</v-btn>
      </v-card-title>
      <VueApexCharts  height="150" type="line" :options="apex.options" :series="apex_vis_amp"/>

      <v-card-title>
        <v-btn  text x-small  color='teal'>Visibility Phase</v-btn>
      </v-card-title>

      <VueApexCharts  height="150" type="line" :options="apex.options" :series="apex_vis_phase"/>

      <v-card-actions>
          <v-range-slider
            v-model="sel_baseline"
            label="Baseline"
            thumb-label="always"
            :thumb-size="20"
            min=0
            max=23
            outlined
          ></v-range-slider>
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
  props: {
  },
  data: function() {return {
    gain: false,
    antennas: false,
    sel_baseline: [0,23],
    refresher: null,
    curl: null,
    apex: {
      options: {
        xaxis: {
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
        chart: {
          id: 'vuechart',
          animations: {
              enabled: true,
              easing: 'easeinout',
              speed: 50,
              animateGradually: {
                  enabled: true,
                  delay: 50
              },
              dynamicAnimation: {
                  enabled: true,
                  speed: 50
              }
          }
        },
      },
    },
  }},

  methods: {

  },

  computed: {
    ant_sel_i () {
      return this.sel_baseline[0]
    },
    ant_sel_j () {
      return this.sel_baseline[1]
    },
    vis () {
      return this.$store.state.vis
    },
    apex_vis_amp () {
      var series = [{name: 'visReal',data:[]}]
      if (this.vis_history.length>0){
        let cat = this.categories
        series = [{
          name: 'Amplitude',
          data: this.vis_history.map((x_h,idx) => ({x:cat[idx],y:x_h.data.filter(x => x.i==this.ant_sel_i && x.j==this.ant_sel_j)
                                                                      .map(x=>Math.sqrt(x.re**2+x.im**2).toFixed(4))
                                                                    }))
                                                                      //.flat()
          }
        ]
      }
      return series
    },
    apex_vis_phase () {
      var series = [{name: 'Phase',data:[]}]
      if (this.vis_history.length>0){
        // this.apex.options.xaxis.categories = this.categories
        // console.log(this.categories)
        let cat = this.categories
        series = [{
          name: 'Phase',
          data: this.vis_history.map((x_h,idx) => ({x:cat[idx],y:x_h.data.filter(x => x.i==this.ant_sel_i && x.j==this.ant_sel_j)
                                                                        .map(x=>((Math.atan2(x.im, x.re)*180./Math.PI)).toFixed(1))
                                                                      }))//.flat()
          }
        ]
      }
      return series
    },

    categories() {
      // console.log(this.vis_history.map(x_h => new Date(x_h.timestamp).toSource()))
      return this.vis_history.map(x_h => x_h.timestamp)
      //
      // options.xaxis.categories
      // return []
    },

    vis_history () {
      return this.$store.state.vis_history
    }
  },
  mounted: function (){
  },
}
</script>

<style scoped>
#overlaySVG {
  position: absolute;
  left: 0px;
  top: 0px;
  z-index: 2;
}

</style>
