<template>
<v-list-item>
    <v-list-item-content>
        <template v-if="authenticated">
            <v-btn color="error" @click="logout">Logout</v-btn>
        </template>
        <template v-else>
            <v-text-field type="password" v-model="pass" outlined label="Password" @keydown.enter="getAuth">
            </v-text-field>
            <v-spacer />
            <v-btn :loading="authenticating" color="cyan" @click="getAuth">
                Login
                <v-icon right>mdi-key</v-icon>
            </v-btn>
        </template>
    </v-list-item-content>
</v-list-item>
</template>

<script>
export default {
    name: "LoginComponent",
    components: {},
    props: {},
    data() {
        return {
            pass: "",
            loading: false,
        }
    },
    methods: {
        getAuth: function () {
            this.$store.dispatch("auth", this.pass);
        },
        logout: function () {
            this.pass = ""
            this.$store.dispatch("logout");

        }
    },
    computed: {
        authenticated() {
            return this.$store.state.token ? true : false
        },
        authenticating() {
            return this.$store.state.authenticating
        },
        info() {
            return this.$store.state.info;
        },
    },
    mounted: function () {
        this.$store.dispatch("renewInfo");
    },
};
</script>
