<template>
  <div>
    <v-alert dense v-if="mode != 'vis'" type="warning" prominent outlined>
      <div class="title">Operating Mode: {{ mode }}</div>
      <div>
        Visibilities most likely outdated because the telescope is currently not
        operating in visibility mode.
      </div>
    </v-alert>
    <v-card class="mx-auto">
      <v-card-title class="my-0 py-1 pr-0">
        <h4 class="teal--text text--lighten-2 text-uppercase">Realtime View</h4>
        <v-spacer />
        <slot name="enlarge"></slot>
      </v-card-title>
      <div id="container" class="mx-2">Getting ready... Loading...</div>
      <v-card elevation="0">
        <v-card-actions class="py-0 my-0">
          <v-slider
            @change="nside = $event"
            :value="20"
            thumb-label="always"
            label="NSide"
            min="2"
            max="64"
          >
          </v-slider>
        </v-card-actions>
        <v-card-actions class="py-0 my-0">
          <v-spacer />
          <v-switch v-model="show_sat" label="Satellites" />
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
      retrieving: false,
      curl: null,
      nside: 16,
      srcLoc: [0, 0, ""],
    };
  },
  watch: {
    vis: function () {
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
      if (this.vis && this.antennas && this.gain && this.json_to_svg_ext) {
        let newJ = {
          info: { info: this.info },
          ant_pos: this.antennas,
          gains: this.gain,
          data: [
            [
              this.vis,
              this.sat_list.map((s) => {
                let a = Object.assign({}, s);
                let newName = s.name
                  .split(" (")[0]
                  .replace(" ", "")
                  .replace(" ", "")
                  .replace(" ", "");
                // Remove spaces as svg attributes does not like them!
                a.name = newName;
                return a;
              }),
            ],
          ],
        };
        // console.log(newJ)
        // console.time("TIMAGING");
        let ret = this.json_to_svg_ext(
          JSON.stringify(newJ),
          this.nside,
          this.show_sat
        );
        var container = document.getElementById("container");
        // make svg scaleable

        container.innerHTML = ret.replace('width="12cm" height="12cm"', "");
        let vm = this;

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
