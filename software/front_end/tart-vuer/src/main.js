import Vue from 'vue'
import App from './App.vue'
import router from './router'
import store from './store'

import axios from 'axios'
import VueAxios from 'vue-axios'

import vuetify from '@/plugins/vuetify';
Vue.use(VueAxios, axios)


// import syn from '@/plugins/api_synthesis.js';

// const wasm = import ("@/wasmimport.js")


Vue.config.productionTip = false

new Vue({
    router,
    // syn,
    // wasm,
    store,
    vuetify,
    render: h => h(App)
}).$mount('#app')