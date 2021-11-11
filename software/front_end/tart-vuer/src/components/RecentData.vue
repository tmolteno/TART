<template>
<v-card class="mx-auto" outlined elevation="3">
    <v-card-title class="my-0 mx-0 pt-1 pb-0 pr-0">
        <h4 class="teal--text text--lighten-2 text-uppercase">
            Recent Data
        </h4>

    </v-card-title>
    <v-data-table dense :headers="headers" :items="visFileList" hide-default-footer>

        <template v-slot:item.timestamp="{ item }">
            <v-chip small :href="BASE_URL + '/'+ item.filename">
                <v-icon x-small left>
                    mdi-download
                </v-icon>
                {{ item.timestamp }}
            </v-chip>
        </template>

        <template v-slot:item.checksum="{ item }">
            <div>
                <v-btn small icon @click="copyToClipboard(item.checksum)">
                    <v-icon small>
                        mdi-clipboard
                    </v-icon>
                </v-btn>
                <v-snackbar v-model="snackbar" :timeout="1000">
                    Copied sha256 checksum to clipboard

                    <template v-slot:action="{ attrs }">
                        <v-btn color="cyan" text v-bind="attrs" @click="snackbar = false">
                            Close
                        </v-btn>
                    </template>
                </v-snackbar>
            </div>

        </template>

    </v-data-table>
</v-card>
</template>

<script>
export default {
    name: "RecentData",
    components: {},
    props: {},
    data() {
        return {
            snackbar: false,
            headers: [
                // {
                //     text: 'Timestamp',
                //     value: 'timestamp',
                //     align: 'center'
                // },
                {
                    text: 'Timestamp',
                    value: 'timestamp',
                    align: 'center',
                    sortable: false,
                },
                {
                    text: 'SHA256 Checksum',
                    value: 'checksum',
                    align: 'center',
                    sortable: false,

                },

            ]

        }
    },
    methods: {
        copyToClipboard(text) {
            navigator.clipboard.writeText(text)
            this.snackbar = true
        }
    },
    computed: {
        visFileList() {
            return this.$store.state.visFileList
        },
        rawFileList() {
            return this.$store.state.rawFileList
        },
        BASE_URL() {
            return this.$store.state.TART_URL;
        },

        telescopeMode() {
            return this.$store.state.telescope_mode;
        },
    },
    mounted: function () {},
};
</script>
