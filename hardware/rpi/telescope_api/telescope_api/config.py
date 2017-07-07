import json
import socket

def init_config(manager):
    runtime_config = manager.dict()
    runtime_config['spi_speed'] = 32000000
    runtime_config['acquire'] = False
    runtime_config['shifter'] = False
    runtime_config['counter'] = False
    runtime_config['verbose'] = False
    runtime_config['centre'] = True
    runtime_config['blocksize'] = 22
    runtime_config['modes_available'] = ['off','diag','raw','vis', 'vis_save', 'cal', 'rt_syn_img']
    runtime_config['mode'] = 'off'
    runtime_config['loop_mode'] = 'loop'
    runtime_config['loop_mode_available'] = ['loop','single','loop_n']
    runtime_config['loop_n'] = 5
    runtime_config['loop_idx'] = 0

    runtime_config['raw'] = {'save': 0, \
                          'N_samples_exp': 22,\
                          'config'   : '24_ant_setup/telescope_config.json',\
                          'base_path': '.'}
    runtime_config['diagnostic'] = {'num_ant': 24, 'N_samples' : 150, 'stable_threshold' : 0.95}
    runtime_config['telescope_config_path'] = '24_ant_setup/telescope_config.json'
    with open(runtime_config['telescope_config_path']) as t_c:
        runtime_config['telescope_config'] = json.load(t_c)
        t_c.close()
    runtime_config['calibration_dir'] = '24_ant_setup/'
    runtime_config['realtime_image_path'] = '/var/www/html/assets/img/image.png'
    runtime_config['chunksize'] = 50
    runtime_config['vis_prefix'] = './fpga'
    runtime_config['hostname'] = socket.gethostname()
    return runtime_config
