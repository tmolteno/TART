<template>
<div class="home">
    <v-row>
        <v-col cols="12" sm="12" md="12" lg="12">
            <GeneralInfo />
        </v-col>
        <v-col cols="12" :sm="enlarge ? 12 : 6" :md="enlarge ? 12 : 6" :lg="enlarge ? 12 : 4" v-if="telescopeMode=='vis'">
            <Synthesis>
                <template slot="enlarge">
                    <v-btn icon @click="enlarge = ~enlarge">
                        <v-icon small v-if="enlarge">
                            mdi-magnify-minus
                        </v-icon>
                        <v-icon small v-else>
                            mdi-magnify-plus
                        </v-icon>
                    </v-btn>
                </template>
            </Synthesis>
        </v-col>
        <v-col cols="12" sm="6" md="6" lg="4" v-if="telescopeMode=='vis'">
            <Baseline />
        </v-col>
        <v-col cols="12" sm="6" md="6" lg="4" v-if="telescopeMode=='vis'">
            <ArrayLayout />
        </v-col>
        <v-col cols="12" sm="6" md="6" lg="6" v-if="telescopeMode=='diag'">
            <RadioSpectrum />
        </v-col>
        <v-col cols="12" sm="6" md="6" lg="6">
            <RecentData />
        </v-col>

    </v-row>
</div>
</template>

<script>
// @ is an alias to /src
import Synthesis from '@/components/Synthesis.vue'
import Baseline from '@/components/Baseline.vue'
import ArrayLayout from '@/components/ArrayLayout.vue'
import RadioSpectrum from '@/components/RadioSpectrum.vue'
import GeneralInfo from "@/components/GeneralInfo.vue"
import RecentData from "@/components/RecentData.vue"

export default {
    name: 'home',
    components: {
        Synthesis,
        Baseline,
        ArrayLayout,
        GeneralInfo,
        RadioSpectrum,
        RecentData
    },
    data() {
        return {
            enlarge: false,
        }
    },
    computed: {
        telescopeMode: function () {
            return this.$store.state.telescope_mode
        }

    }
}
</script>

<style>
#home {
    font-family: 'Avenir', Helvetica, Arial, sans-serif;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    text-align: center;
    color: #2c3e50;
}
</style>
