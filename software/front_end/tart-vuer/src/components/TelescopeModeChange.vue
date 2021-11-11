<template>
<v-list-item>
    <v-select v-model="telescopeMode" :items="modes" @change="setMode" :label="label" outlined :disabled="!authenticated" />
</v-list-item>
</template>

<script>
export default {
    name: "TelescopeModeChange",
    components: {},
    props: {},
    data() {
        return {
            modes: [{
                    text: 'off',
                    value: 'off'
                },
                {
                    text: 'Raw',
                    value: 'raw'
                },
                {
                    text: 'Diagnose',
                    value: 'diag'
                },
                {
                    text: 'Visibility',
                    value: 'vis'
                }
            ]
        }
    },
    methods: {
        setMode: function (e) {
            this.$store.dispatch("setTelescopeMode", e);
        },
        logout: function () {
            this.pass = ""
            this.$store.dispatch("logout");
        }
    },
    computed: {
        label() {
            return "Operating mode" + (!this.authenticated ? " (Login Required)" : "")
        },
        authenticated() {
            return this.$store.state.token ? true : false
        },
        telescopeMode() {
            return this.$store.state.telescope_mode;
        },
    },
    mounted: function () {
        this.$store.dispatch("renewInfo");
    },
};
</script>
