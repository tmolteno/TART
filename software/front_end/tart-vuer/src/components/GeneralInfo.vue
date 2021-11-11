<template>
<v-card class="mx-auto" outlined elevation="3">
    <v-card-title class="my-0 mx-0 pt-1 pb-0 pr-0" @click="show = !show">
        <h6 class="teal--text text--lighten-2 text-uppercase">
            GENERAL INFO
        </h6>
        <v-spacer></v-spacer>

        <v-btn icon right>
            <v-icon>{{ show ? 'mdi-chevron-up' : 'mdi-chevron-down' }}</v-icon>
        </v-btn>
    </v-card-title>

    <v-expand-transition>
        <v-card-text v-show="show">
            <v-row class="pa-0">
                <v-col class="pa-2" cols="6" sm="3" md="3" v-for="(i, key) in info" :key="key">
                    <v-sheet class="pa-1 ma-1" height="100%" rounded color="teal lighten-3" elevation="1">
                        <div class="text-uppercase">{{key.replace("_"," ")}}</div>
                        <div class="">{{i}}</div>
                    </v-sheet>
                </v-col>
            </v-row>

        </v-card-text>
    </v-expand-transition>

</v-card>
</template>

<script>
export default {
    name: "GeneralInfo",
    components: {},
    props: {},
    data: function () {
        return {
            pass: "",
            show: false,
        };
    },

    methods: {},

    computed: {
        info() {
            return this.$store.state.info;
        },
    },
    mounted: function () {
        this.$store.dispatch("renewInfo");
    },
};
</script>
