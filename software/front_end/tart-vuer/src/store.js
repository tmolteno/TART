import Vue from 'vue'
import Vuex from 'vuex'

Vue.use(Vuex)

export default new Vuex.Store({
    strict: process.env.NODE_ENV !== 'production',
    state: {
        TART_URL: "https://tart.elec.ac.nz/signal",
        CATALOG_URL: "https://tart.elec.ac.nz/catalog",
        num_bin: 512,
        nw: 128,
        vis: null,
        gain: null,
        antennas: [],
        selectedBaseline: [0, 23],
        sat_list: [],
        vis_history: [],
        telescope_mode: 'vis',
        telescope_modes: ['off', 'diag', 'raw', 'vis'],
        loading: false,
        info: {},
        token: '',
        visDataList: [],
        rawDataList: [],
        visFileList: [],
        rawFileList: [],
        channels: [],
        authenticating: false,
    },
    mutations: {
        setAuthenticating(state, bool) {
            state.authenticating = bool
        },
        newRawData(state, list) {
            state.rawDataList = list
        },
        newVisData(state, list) {
            state.visDataList = list
        },
        newTART_URL(state, url) {
            state.TART_URL = url
        },
        newMode(state, mode) {
            state.telescope_mode = mode
        },
        newVis(state, visArr) {
            state.vis = visArr
        },
        newGain(state, gainArr) {
            state.gain = gainArr
        },
        newAntennas(state, antennas) {
            state.antennas = antennas
        },
        newSatellite(state, satArr) {
            state.sat_list = satArr
        },
        newChannels(state, channelArr) {
            state.channels = channelArr
        },
        newBaseline(state, BL) {
            state.selectedBaseline = BL
        },
        changeLoadingState(state, bool) {
            state.loading = bool
        },
        appendVis(state, visArr) {
            state.vis_history.push(visArr)
        },
        removeVis(state) {
            state.vis_history.shift()
        },
        newInfo(state, info) {
            state.info = info
        },
        authenticate(state, token) {
            state.token = token
        },
        newVisFileList(state, list) {
            state.visFileList = list
        },
        newRawFileList(state, list) {
            state.rawFileList = list
        },

    },
    actions: {
        auth(context, pw) {
            context.commit('setAuthenticating', true)
            let data = { username: 'admin', password: pw }
            Vue.axios.post(this.state.TART_URL + "/api/v1/auth", data).then((response) => {
                let token = Object.freeze(response.data).access_token
                context.commit('authenticate', token)
                context.commit('setAuthenticating', false)
            }).catch(() => {
                context.commit('setAuthenticating', false)
            })
        },
        setTelescopeMode(context, newMode) {
            let headers = { 'Authorization': 'JWT ' + this.state.token }
            Vue.axios.post(this.state.TART_URL + "/api/v1/mode/" + newMode, {}, { headers: headers }).then((response) => {
                let data = Object.freeze(response.data)
                context.commit('newMode', data)
            })

        },
        logout(context) {
            context.commit('authenticate', '')
        },
        newVisData(context) {
            Vue.axios.post(this.state.TART_URL + "/api/v1/vis/data").then((response) => {
                let data = Object.freeze(response.data)
                context.commit('newVisData', data)
            })
        },
        newRawData(context) {
            Vue.axios.post(this.state.TART_URL + "/api/v1/raw/data").then((response) => {
                let data = Object.freeze(response.data)
                context.commit('newRawData', data)
            })
        },
        setTART_URL(context, newURL) {
            context.commit('newTART_URL', newURL)
        },
        renewChannels(context) {
            Vue.axios.get(this.state.TART_URL + "/api/v1/status/channel").then((response) => {
                let data = Object.freeze(response.data)
                    // console.log(data)
                context.commit('newChannels', data)
            })
        },
        renewInfo(context) {
            Vue.axios.get(this.state.TART_URL + "/api/v1/info").then((response) => {
                let data = Object.freeze(response.data.info)
                context.commit('newInfo', data)
            })
        },
        renewVisData(context) {
            Vue.axios.get(this.state.TART_URL + "/api/v1/vis/data").then((response) => {
                let data = Object.freeze(response.data)
                context.commit('newVisFileList', data)
            })
        },
        renewRawData(context) {
            Vue.axios.get(this.state.TART_URL + "/api/v1/raw/data").then((response) => {
                let data = Object.freeze(response.data)
                context.commit('newRawFileList', data)
            })
        },
        renewAntennas(context) {
            Vue.axios.get(this.state.TART_URL + "/api/v1/imaging/antenna_positions").then((response) => {
                let data = Object.freeze(response.data)
                context.commit('newAntennas', data)
            })
        },
        renewMode(context) {
            Vue.axios.get(this.state.TART_URL + "/api/v1/mode/current").then((response) => {
                let data = Object.freeze(response.data.mode)
                    // context.commit('changeLoadingState', false)
                context.commit('newMode', data)
            })
        },
        renewVis(context) {
            Vue.axios.get(this.state.TART_URL + "/api/v1/imaging/vis").then((response) => {
                let visArr = Object.freeze(response.data)
                context.commit('changeLoadingState', false)
                context.commit('newVis', visArr)
                if (this.state.vis_history.length > 40) { context.commit('removeVis') }
                context.commit('appendVis', visArr)
            })
        },
        renewGain(context) {
            Vue.axios.get(this.state.TART_URL + "/api/v1/calibration/gain").then((response) => {
                let d = Object.freeze(response.data)
                context.commit('newGain', d)
            })
        },
        renewSatellite(context) {
            var api_call
            if (this.state.vis) {
                api_call = this.state.CATALOG_URL + "/catalog?date=" + this.state.vis.timestamp + "&lat=-45.85177&lon=170.5456"
            } else {
                api_call = this.state.CATALOG_URL + "/catalog?date=" + (new Date).toISOString() + "&lat=-45.85177&lon=170.5456"
            }

            Vue.axios.get(api_call).then((response) => {
                context.commit('newSatellite', Object.freeze(response.data))
            })
        },
        newBaseline(context, val) {
            context.commit('newBaseline', val)
        },



    }
})