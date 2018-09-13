from flask import Flask
from flask import render_template, jsonify, send_file
from flask_jwt_extended import JWTManager, jwt_required

import numpy as np

from tart_web_api.main import app
import tart_web_api.database as db

@app.route('/status/fpga', methods=['GET',])
def get_status_fpga():
    """
      @api {get} /status/fpga Request fpga information
      @apiName get_status_fpga
      @apiGroup Status
      @apiSuccess {String} hostname Hostname of the RPI
      @apiSuccess {String} timestamp UTC Timestamp
      @apiSuccess {Object} AQ_STREAM AQ_STREAM
      @apiSuccess {Number} AQ_STREAM.data Acquisition data register value
      @apiSuccess {Object} AQ_SYSTEM AQ_SYSTEM
      @apiSuccess {Number=0,1} AQ_SYSTEM.512Mb Report flag if firmware is compiled for 512Mb SDRAM
      @apiSuccess {Number=0,1} AQ_SYSTEM.SDRAM_ready Report flag for SDRAM
      @apiSuccess {Number=0,1} AQ_SYSTEM.enabled Report flag for Acquisition system beeing enabled.
      @apiSuccess {Number=0,1} AQ_SYSTEM.error Report flag for Acquisition system beeing enabled.
      @apiSuccess {Number=0,1} AQ_SYSTEM.overflow Report flag for Acquisition system beeing enabled.
      @apiSuccess {Number=0,7} AQ_SYSTEM.state Report flag for Acquisition system beeing enabled.
      @apiSuccess {Object} SPI_STATS SPI_STATS
      @apiSuccess {Number=0,1} SPI_STATS.FIFO_overflow Overflow flag
      @apiSuccess {Number=0,1} SPI_STATS.FIFO_underrun Underrun flag
      @apiSuccess {Number=0,1} SPI_STATS.spi_busy spi_busy
      @apiSuccess {Object} SYS_STATS SYS_STATS
      @apiSuccess {Number=0,1} SYS_STATS.acq_en Acquisition Unit enabled
      @apiSuccess {Number=0,1} SYS_STATS.cap_debug
      @apiSuccess {Number=0,1} SYS_STATS.cap_en
      @apiSuccess {Number=0,1} SYS_STATS.state
      @apiSuccess {Number=0,1} SYS_STATS.viz_en Visibility enabled
      @apiSuccess {Number=0,1} SYS_STATS.viz_pend Visibility pending
      @apiSuccess {Object}     TC_CENTRE TC_CENTRE
      @apiSuccess {Number=0,1} TC_CENTRE.centre Acqusion
      @apiSuccess {Number}     TC_CENTRE.delay Delay
      @apiSuccess {Number=0,1} TC_CENTRE.drift Acqusion
      @apiSuccess {Number=0,1} TC_CENTRE.invert Acqusion
      @apiSuccess {Object}     TC_DEBUG TC_DEBUG
      @apiSuccess {Number=0,1} TC_DEBUG.count Acqusion
      @apiSuccess {Number=0,1} TC_DEBUG.debug Acqusion
      @apiSuccess {Number}     TC_DEBUG.numantenna Number of antennas
      @apiSuccess {Number=0,1} TC_DEBUG.shift Acqusion
      @apiSuccess {Object} TC_STATUS TC_STATUS
      @apiSuccess {Number} TC_STATUS.delta Acqusion
      @apiSuccess {Number} TC_STATUS.phase Acqusion
      @apiSuccess {Object} TC_SYSTEM TC_SYSTEM
      @apiSuccess {Number=0,1} TC_SYSTEM.enabled
      @apiSuccess {Number=0,1} TC_SYSTEM.error
      @apiSuccess {Number=0,1} TC_SYSTEM.locked
      @apiSuccess {Number} TC_SYSTEM.source
      @apiSuccess {Object} VX_DEBUG VX_DEBUG
      @apiSuccess {Number} VX_DEBUG.limp
      @apiSuccess {Number} VX_DEBUG.stuck
      @apiSuccess {Object} VX_STATUS VX_STATUS
      @apiSuccess {Number} VX_STATUS.accessed VX_STATUS
      @apiSuccess {Number} VX_STATUS.available VX_STATUS
      @apiSuccess {Number} VX_STATUS.bank VX_STATUS
      @apiSuccess {Number} VX_STATUS.overflow VX_STATUS
      @apiSuccess {Object} VX_STREAM VX_STREAM
      @apiSuccess {Number} VX_STREAM.data Byte of data
      @apiSuccess {Object} VX_SYSTEM VX_SYSTEM
      @apiSuccess {Number} VX_SYSTEM.blocksize VX_SYSTEM
      @apiSuccess {Number} VX_SYSTEM.enabled VX_SYSTEM
      @apiSuccess {Number} VX_SYSTEM.overwrite VX_SYSTEM
    """
    runtime_config = app.config['CONFIG']
    if runtime_config.has_key("status"):
        ret = runtime_config["status"]
        ret['hostname'] = runtime_config['hostname']
        return jsonify(ret)
    else:
        return jsonify({})

@app.route('/status/channel', methods=['GET',])
def get_status_channel_all():
    """
    @api {get} /status/channel Request telescope all channel information.
    @apiGroup Status

    @apiName get_status_channel_all
    @apiSuccess {Object[]} Array of channel information.
    """

    runtime_config = app.config['CONFIG']
    if runtime_config.has_key("channels"):
        channel_list = db.get_manual_channel_status()
        ret = runtime_config["channels"]
        for ch in ret:
            ch['enabled'] = channel_list[ch['id']]['enabled']
            ch['phase']['stability'] = int(ch['phase']['stability']*100)/100.
            ch['radio_mean']['mean'] = int(ch['radio_mean']['mean']*10000)/10000.
        return jsonify(ret)
    else:
        return jsonify({})

@app.route('/status/channel/<int:channel_idx>', methods=['GET',])
def get_status_channel_i(channel_idx):
    """
    @api {get} /status/channel/:channel_idx Request specific channel information.
    @apiGroup Status
    @apiParam {Number{0-23}} channel_idx Channel index

    @apiName get_status_channel_i
    @apiSuccess {Object} channel information of provided index.

    @apiSuccessExample {json} Success-Response:
      HTTP/1.1 200 OK
      {
        "id": 23,
        "phase": {
          "N_samples": 200,
          "measured": 3,
          "ok": 1,
          "stability": 1.0,
          "threshold": 0.95
        },
        "radio_mean": {
          "mean": 1.0,
          "ok": 0,
          "threshold": 0.2
        },
        "freq": [...],
        "power": [...]
        }
      }
    """
    runtime_config = app.config['CONFIG']
    if runtime_config.has_key("channels"):
        if (channel_idx < 24) and (channel_idx > -1):
            channel_list = db.get_manual_channel_status()
            ret = runtime_config["channels"][channel_idx]
            ret['enabled'] = channel_list[channel_idx]['enabled']
            return jsonify(ret)
        else:
            return jsonify({})
    else:
        return jsonify({})


@app.route('/mode/current', methods=['GET',])
def get_current_mode():
    """
    @api {get} /mode/current Request current telescope operating mode
    @apiGroup Operation

    @apiName get_current_mode
    @apiSuccess {String} mode Current mode of the telescope.
    """
    runtime_config = app.config['CONFIG']
    return jsonify({'mode':runtime_config['mode']})

@app.route('/mode', methods=['GET',])
def get_mode():
    """
    @api {get} /mode Request telescopes available operating modes
    @apiGroup Operation

    @apiName get_mode
    @apiSuccess {String[]} modes Available operating modes.
    """
    runtime_config = app.config['CONFIG']
    return jsonify({'modes':runtime_config['modes_available']})


@app.route('/mode/<mode>', methods=['POST',])
@jwt_required
def set_mode(mode):
    """
    @api {post} /mode/<mode> Set telescope operating mode
    @apiGroup Operation
    @apiHeader (Authorization) {String} Authorization JWT authorization value.

    @apiName set_mode
    @apiParam {String ="off","diag","raw","vis","vis_save","cal","rt_syn_img"} mode Telescope operation mode.

    @apiHeaderExample {String} Authorization Header Example
        Authorization: JWT eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZGVudGl0eSI6MSwiaWF0IjoxNDk5Mzg2ODE3LCJuYmYiOjE0OTkzODY4MTcsImV4cCI6MTQ5OTM4NzExN30.q7k0HmcDCMHIGOqc6wACh08B1abBZ4GApXF0ap5zJFs

    @apiSuccess (200) {String} mode Current mode of the telescope.

    @apiSuccess (401) {Object} message Error Information
    @apiSuccess (401) {String} message.description Request does not contain an access token
    @apiSuccess (401) {String} message.error Authorization Required
    @apiSuccess (401) {Numbe} message.status_code Status Code

    """

    runtime_config = app.config['CONFIG']
    if mode in runtime_config['modes_available']:
        runtime_config['mode'] = mode
        return jsonify({'mode':runtime_config['mode']})
    return jsonify({})


@app.route('/loop/<loop_mode>', methods=['POST',])
@jwt_required
def set_loop_mode(loop_mode):
    """
    @api {post} /loop/<loop_mode> Set telescopes loop mode.
    @apiGroup Operation
    @apiHeader (Authorization) {String} Authorization JWT authorization value.

    @apiName set_loop_mode
    @apiParam {String ="loop","single","loop_n"} mode Telescopes loop mode. Perform single, loop n times or loop indefinit in selected mode before returning to offline mode.
    @apiSuccess {String} loop_mode Current mode of the telescope.

    """
    runtime_config = app.config['CONFIG']
    if loop_mode in runtime_config['loop_mode_available']:
        runtime_config['loop_mode'] = loop_mode
        return jsonify({'loop_mode':runtime_config['loop_mode']})


@app.route('/loop/<int:loop_n>', methods=['POST',])
@jwt_required
def set_loop_n(loop_n):
    """
    @api {post} /loop/<loop_n> Set telescopes loop mode.
    @apiGroup Operation
    @apiHeader (Authorization) {String} Authorization JWT authorization value.

    @apiName set_loop_mode
    @apiParam {int =0-100} loop_n Number of loops in selected mode before returning to offline mode.
    @apiSuccess {String} loop_mode Current mode of the telescope.

    """
    runtime_config = app.config['CONFIG']
    runtime_config['loop_n'] = loop_mode
    return jsonify({'loop_mode':runtime_config['loop_mode']})

@app.route('/imaging/vis')
def get_latest_vis():
    """
    @api {get} /imaging/vis Request latest visibilities.
    @apiGroup Imaging

    @apiName get_latest_vis
    @apiSuccess {Object[]} vis Get visibilities.
    """
    runtime_config = app.config['CONFIG']
    ret = []
    if runtime_config.has_key('vis_current'):
        ret = runtime_config['vis_current']
        channel_list = db.get_manual_channel_status()
        active_channels = np.zeros(len(channel_list))
        for ch in channel_list:
            active_channels[ch['channel_id']] = ch['enabled']
        active_vis = []
        for v in ret['data']:
            if active_channels[v['i']] and active_channels[v['j']]:
                active_vis.append(v)
        ret['data'] = active_vis
    return jsonify(ret)

@app.route('/imaging/antenna_positions')
def get_imaging_antenna_positions():
    """
    @api {get} /imaging/antenna_positions Request antenna_positions.
    @apiGroup Imaging

    @apiName get_imaging_antenna_positions
    @apiSuccess {Object[]} antenna_positions Array of antenna positions in East-North-Up Coordinate system [[e,n,u],[e,n,u],..]].
    """
    runtime_config = app.config['CONFIG']
    if runtime_config.has_key('antenna_positions'):
        return jsonify(runtime_config['antenna_positions'])

@app.route('/imaging/timestamp')
def get_imaging_timestamp():
    """
    @api {get} /imaging/timestamp Request timestamp.
    @apiGroup Imaging

    @apiName get_imaging_timestamp
    @apiSuccess {String} timestamp Get UTC timestamp  of latest visibilities in isoformat .
    """
    runtime_config = app.config['CONFIG']
    if runtime_config.has_key('vis_timestamp'):
        return runtime_config['vis_timestamp'].isoformat()[:-3]+'Z'
    else:
        return jsonify({})

@app.route('/info', methods=['GET',])
def get_info():
    """
    @api {get} /info Request telescope general information
    @apiGroup Info

    @apiName get_info
    @apiSuccess {Object} info General site and telescope information.
    @apiSuccess {Number[]} info.location Location Coordinates
    @apiSuccess {Number} info.location.lon Longitude
    @apiSuccess {Number} info.location.lan Latitude
    @apiSuccess {Number} info.location.alt Altitude
    @apiSuccess {Number} info.operating_frequency Operating frequency of the radio
    @apiSuccess {Number} info.L0_frequency L0 frequency of the radio
    @apiSuccess {Number} info.baseband_frequency Baseband frequency of the radio
    @apiSuccess {Number} info.sampling_frequency Sampling frequency of the radio
    @apiSuccess {Number} info.bandwidth Bandwidth of the radio
    @apiSuccess {Number} info.num_antenna Number of antennas in the telescope
    @apiSuccess {Number} info.name Telescope site name
    """

    runtime_config = app.config['CONFIG']
    t_c = runtime_config['telescope_config']
    ret = {}
    ret['name'] = t_c['name']
    ret['operating_frequency'] = t_c['frequency']
    ret['L0_frequency'] = t_c['L0_frequency']
    ret['baseband_frequency'] = t_c['baseband_frequency']
    ret['sampling_frequency'] = t_c['sampling_frequency']
    ret['bandwidth'] = t_c['bandwidth']
    ret['num_antenna'] = t_c['num_antenna']
    ret['location'] = {'lon':t_c['lon'], 'lat':t_c['lat'], 'alt':t_c['alt']}
    return jsonify({'info':ret})



# Example to serve an image without creating a file.
#def serve_pil_image(pil_img):
#    import StringIO
#    img_io = StringIO.StringIO()
#    pil_img.save(img_io, 'JPEG', quality=70)
#    img_io.seek(0)
#    return send_file(img_io, mimetype='image/jpeg')

#@app.route('/pic')
#def serve_img():
#    from PIL import Image
#    import numpy as np
#    img = Image.fromarray(np.random.uniform(0,255,(255,255)).astype(np.uint8))
#    return serve_pil_image(img)
