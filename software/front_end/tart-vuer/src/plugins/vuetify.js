import Vue from 'vue'
import Vuetify from 'vuetify'
import 'vuetify/dist/vuetify.min.css'


Vuetify.config.silent = true

Vue.use(Vuetify)

const opts = {

  icons: {
    iconfont: 'mdi',
  },
}

export default new Vuetify(opts)

