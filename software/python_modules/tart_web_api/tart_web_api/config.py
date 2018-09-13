import json
import socket

def init_config(manager):
    
    data_root = '/telescope_data'
    config_root = '/config_data'

    runtime_config = manager.dict()
    runtime_config['spi_speed'] = 32000000
    runtime_config['acquire'] = False
    runtime_config['shifter'] = False
    runtime_config['counter'] = False
    runtime_config['verbose'] = False
    runtime_config['centre'] = True
    runtime_config['modes_available'] = ['off', 'diag', 'raw', 'vis', 'vis_save', 'cal', 'rt_syn_img']
    runtime_config['mode'] = 'off'
    runtime_config['loop_mode'] = 'loop'
    runtime_config['loop_mode_available'] = ['loop', 'single', 'loop_n']
    runtime_config['loop_n'] = 5
    runtime_config['loop_idx'] = 0

    runtime_config['optimisation'] = 'idle'
    runtime_config['data_root'] = data_root

    runtime_config['raw'] = {
        'save': 1, \
        'N_samples_exp': 20,\
        'base_path': '{}/raw'.format(data_root)
    }
    runtime_config['diagnostic'] = {
        'num_ant': 24,\
        'N_samples' : 15,\
        'stable_threshold' : 0.95,\
        'N_samples_exp' : 20,\
        'spectre': {'NFFT': 4096, 'N_samples_exp': 18}

    }
    runtime_config['vis'] = {
        'save' : 1, \
        'chunksize' : 60,\
        'N_samples_exp' : 24,\
        'base_path' : '{}/vis'.format(data_root),
    }
    
    runtime_config['telescope_config_path'] = '{}/telescope_config.json'.format(config_root)
    with open(runtime_config['telescope_config_path']) as t_c:
        runtime_config['telescope_config'] = json.load(t_c)
        t_c.close()

    antenna_positions_path = '{}/calibrated_antenna_positions.json'.format(config_root)
    with open(antenna_positions_path) as a_c:
        runtime_config['antenna_positions'] = json.load(a_c)
        a_c.close()
        
    runtime_config['calibration_dir'] = '/{}/'.format(config_root)
    runtime_config['realtime_image_path'] = '{}/assets/img/image.png'.format(data_root)
    runtime_config['hostname'] = socket.gethostname()
    return runtime_config
