<template>
    <v-app>
        <v-app-bar app dense dark>
            <v-app-bar-nav-icon @click.stop="drawer = !drawer"></v-app-bar-nav-icon>
            <v-toolbar-title class="headline text-uppercase pa-0">
                <span class="cyan--text font-weight-bold">TART</span>
                <span class="font-weight-light">VIEWER</span>
            </v-toolbar-title>
            <v-spacer />
            <v-btn icon color='cyan' href="https://github.com/tmolteno/TART">
                <v-icon>mdi-github</v-icon>
            </v-btn>
        </v-app-bar>

        <v-navigation-drawer v-model="drawer" app temporary dark>
            <v-list-item class="pt-5">
                <v-select label="Refresh Interval (seconds)" v-model="refreshInterval" :items="refreshIntervals"
                    outlined />
            </v-list-item>

            <TelescopeModeChange />
            <LoginField />

            <template v-slot:append>
                <v-list-item>
                    <v-text-field :disabled="selected != 'custom'" v-model="TART_URL" label="Telescope API Endpoint" />
                </v-list-item>
                <v-list-item-group v-model="selected" active-class="primary--text" mandatory>
                    <v-list-item value="signal">
                        Dunedin
                    </v-list-item>
                    <v-list-item value="rhodes">
                        Rhodes
                    </v-list-item>
                    <v-list-item value="stellenbosch">
                        Stellenbosch
                    </v-list-item>
                    <v-list-item value="custom">
                        Custom
                    </v-list-item>
                </v-list-item-group>


            </template>

        </v-navigation-drawer>

        <v-main>
            <v-container fluid>
                <router-view />
            </v-container>
        </v-main>
    </v-app>
</template>

<script>
import LoginField from "@/components/LoginField.vue";
import TelescopeModeChange from "@/components/TelescopeModeChange.vue";

export default {
    components: {
        LoginField,
        TelescopeModeChange
    },
    name: "App",
    data: () => ({
        drawer: false,
        selected: 'signal',
        enabled: false,
        refresher: null,
        longTermRefresher: null,
        longTermRefreshInterval: 15,
        refreshInterval: 5,
        refreshIntervals: [1, 5, 10],
    }),
    methods: {
        getData: function () {

            this.$store.dispatch("renewMode");
            if (this.$store.state.telescope_mode == 'vis') {
                this.$store.dispatch("renewVis");
                this.$store.dispatch("renewGain");
                this.$store.dispatch("renewSatellite");
                this.$store.dispatch("renewAntennas");
            }
            if (this.$store.state.telescope_mode == 'diag') {
                this.$store.dispatch("renewChannels");
            }

            this.refresher = window.setTimeout(
                this.getData,
                this.refreshInterval * 1000
            );
        },
        updateCatalogData() {
            this.$store.dispatch("renewVisData");
            this.$store.dispatch("renewRawData");
            this.longTermRefresher = window.setTimeout(
                this.updateCatalogData,
                this.longTermRefreshInterval * 1000
            );

        },
    },
    watch: {
        selected: function (newPostfix) {
            if (newPostfix != 'custom') {
                this.$store.dispatch("setTART_URL", 'https://tart.elec.ac.nz/' + newPostfix);

            }
        }
    },
    created: function () {
        this.$store.dispatch("renewVis");
        this.$store.dispatch("renewGain");
        this.$store.dispatch("renewAntennas");
        this.$store.dispatch("renewSatellite");
        this.$store.dispatch("renewVisData");
        this.$store.dispatch("renewRawData");
        this.getData();
        this.updateCatalogData();

    },
    beforeDestroy() {
        window.clearTimeout(this.refresher);
        window.clearTimeout(this.longTermRefresher);
    },
    computed: {

        TART_URL: {
            get: function () {
                return this.$store.state.TART_URL;
            },
            set: function (newURL) {
                this.$store.dispatch("setTART_URL", newURL);
                this.$store.dispatch("renewMode");

            },
        },
    },
};
</script>
