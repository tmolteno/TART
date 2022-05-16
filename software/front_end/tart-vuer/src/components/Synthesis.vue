<template>
  <div>
    <v-alert dense v-if="mode != 'vis'" type="warning" prominent outlined>
      <div class="title">Operating Mode: {{ mode }}</div>
      <div>
        Visibilities most likely outdated because the telescope is currently not
        operating in visibility mode.
      </div>
    </v-alert>
    <v-card class="mx-auto" elevation="3">
      <v-card-title class="my-0 py-1 pr-0">
        <h4 class="teal--text text--lighten-2 text-uppercase">Realtime View</h4>
        <v-spacer />
        <slot name="enlarge"></slot>
      </v-card-title>
      <div id="container" class="mx-2">Getting ready... Loading...</div>

      <v-card-actions class="py-0 my-0">
        <v-slider @change="nside = $event" :value="20" thumb-label="always" label="NSide" min="2" max="64">
        </v-slider>
      </v-card-actions>
      <v-card tile elevation="0"> 
        <!-- limit absolute v-overlay by introducing another v-card here -->
      <v-card-actions class="py-0 my-0">
        <v-spacer />
        <v-switch v-model="show_sat" label="Overlay Satellites" />
        <v-spacer />
        <v-switch v-model="show_antennas" label="Toggle Antennas" />
        <v-spacer />
        
        <v-overlay absolute v-if="srcLoc[0] > 0">
          <v-spacer />
          <v-btn small tile> {{ srcLoc[2] }} </v-btn>
          <br />
          <v-btn small tile> Elevation {{ srcLoc[0].toFixed(3) }} </v-btn>
          <v-btn small tile> Azimuth {{ srcLoc[1].toFixed(3) }} </v-btn>
        </v-overlay>
      </v-card-actions>

        </v-card>


      <v-expand-transition>
        <div v-show="show_antennas">
          <v-divider></v-divider>

          <v-card-title class="my-0 py-1 pr-0">
            <h4 class="teal--text text--lighten-2 text-uppercase">Antennas used for imaging</h4>
          </v-card-title>
          <v-card-actions class="py-0 my-0">
            <v-row class="ma-0 pa-0">
              <v-col v-for="ant in antennasIdx" :key="'ant' + ant" cols="2" class="ma-0 pa-0 mx-auto">
                <v-checkbox v-model="antennasUsed" :label="ant.toString()" :value="ant" class="mx-auto"></v-checkbox>
              </v-col>
            </v-row>
          </v-card-actions>
          <v-card-actions class="py-0 my-0">

            <div v-if="reducedVis">
              Number of contributing baselines: {{ reducedVis.data.length }}
            </div>
          </v-card-actions>


        </div>
      </v-expand-transition>
    </v-card>


  </div>
</template>

<script>
export default {
  name: "SynthesisComponent",
  components: {},
  async beforeCreate() {
    const wasm = await import("wasm-tart-imaging");
    this.json_to_svg_ext = wasm.json_to_svg_ext;
  },
  data: function () {
    return {
      show_sat: true,
      show_antennas: false,
      nside: 16,
      srcLoc: [0, 0, ""],
      antennasIdx: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23],
      antennasUsed: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23],
    };
  },
  watch: {
    reducedVis: function () {
      this.redraw();
    },
    nside: function () {
      this.redraw();
    },
    show_sat: function () {
      this.redraw();
    },
  },
  methods: {
    redraw: async function () {
      if (this.reducedVis && this.antennas && this.gain && this.json_to_svg_ext) {

        let newJ = {
          info: { info: this.info },
          ant_pos: this.antennas,
          gains: this.gain,
          data: [
            [
              this.reducedVis,
              this.sat_list
            ],
          ],
        };

        let ret = this.json_to_svg_ext(
          JSON.stringify(newJ),
          this.nside,
          this.show_sat
        );
        var container = document.getElementById("container");
        // make svg scaleable

        container.innerHTML = ret.replace('width="12cm" height="12cm"', "");
        let vm = this;
        console.log(container);
        // Add listeners to SVG
        [].forEach.call(document.getElementsByTagName("circle"), function (el) {
          if (el.getAttribute("name")) {
            el.setAttribute("stroke-width", 50);

            el.addEventListener("click", function (e) {
              vm.srcLoc = [
                (parseFloat(e.target.getAttribute("el"))),
                (parseFloat(e.target.getAttribute("az"))),
                e.target.getAttribute("name"),
              ];

              e.target.setAttribute("stroke", "white");
            });
            el.addEventListener("mouseenter", function (e) {
              vm.srcLoc = [
                (parseFloat(e.target.getAttribute("el"))),
                (parseFloat(e.target.getAttribute("az"))),
                e.target.getAttribute("name"),
              ];

              e.target.setAttribute("stroke", "white");
            });
            el.addEventListener("mouseleave", function (e) {
              vm.srcLoc = [0, 0, ""];

              e.target.setAttribute("stroke", "red");
            });
          }
        });
      }
    },
  },
  computed: {
    mode() {
      return this.$store.state.telescope_mode;
    },
    num_bin() {
      return this.$store.state.num_bin;
    },
    nw() {
      return this.$store.state.nw;
    },
    info() {
      return this.$store.state.info;
    },
    vis() {
      return this.$store.state.vis;
    },
    reducedVis() {
      if (this.$store.state.vis) {
        let data = this.$store.state.vis.data.filter(v => (this.antennasUsed.includes(v.i) && this.antennasUsed.includes(v.j)))
        let ts = this.$store.state.vis.timestamp
        var reduced_vis = {
          'data': data,
          'timestamp': ts
        }
        return reduced_vis
      }
      else {
        return null
      }
    },
    gain() {
      return this.$store.state.gain;
    },
    antennas() {
      return this.$store.state.antennas;
    },
    sat_list() {
      return this.$store.state.sat_list;
    },
    ant_sel_i() {
      return this.sel_baseline[0];
    },
    ant_sel_j() {
      return this.sel_baseline[1];
    },
  },
};
</script>
