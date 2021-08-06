import argparse
import os, stat

import multiprocessing
import logging.config
import yaml

logger = logging.getLogger(__name__)

from tart_hardware_interface.tartspi import TartSPI
from tart_hardware_interface.highlevel_modes_api import *
from tart_hardware_interface.stream_vis import *

if __name__ == "__main__":
    PARSER = argparse.ArgumentParser()
    PARSER.add_argument("--config", required=False, help="Config file.")
    PARSER.add_argument(
        "--calibration_dir",
        required=False,
        type=str,
        help="Config file.",
        default="6_ant_setup/",
    )
    PARSER.add_argument(
        "--vis_prefix",
        required=False,
        type=str,
        default="vis",
        help="generate abs and angle for vis",
    )
    PARSER.add_argument(
        "--blocksize", default=23, type=int, help="exponent of correlator block-size"
    )
    PARSER.add_argument(
        "--chunksize", default=10, type=int, help="number of vis objects per file"
    )
    PARSER.add_argument(
        "--mode",
        required=False,
        type=str,
        default="",
        help="save, save_single, qt, #syn, absang, calib",
    )

    ARGS = PARSER.parse_args()

    # Setup up multiprocessing logging
    path = "logging.yaml"
    if os.path.exists(path):
        with open(path, "rt") as f:
            log_config = yaml.load(f.read())
        logging.config.dictConfig(log_config)

    runtime_config = {}
    runtime_config["mode"] = ARGS.mode
    runtime_config["blocksize"] = ARGS.blocksize
    runtime_config["telescope_config_path"] = ARGS.config
    runtime_config["calibration_dir"] = ARGS.calibration_dir
    runtime_config["chunksize"] = ARGS.chunksize
    runtime_config["vis_prefix"] = ARGS.vis_prefix
    tart_instance = TartSPI()

    if ARGS.mode in ["save", "save_single"]:
        single = False
        if ARGS.mode == "save_single":
            single = True
        (
            vis_queue,
            vis_calc_process,
            capture_process,
            vis_calc_cmd_q,
            capture_cmd_q,
        ) = stream_vis_to_queue(tart_instance, runtime_config)
        from save_vis import result_loop

        post_process = multiprocessing.Process(
            target=result_loop, args=(vis_queue, runtime_config, logger, single)
        )
        post_process.start()
        post_process.join()
        capture_cmd_q.put("stop")
        vis_calc_cmd_q.put("stop")
        vis_calc_process.join()
        capture_process.join()

    elif ARGS.mode == "rt_syn_img":
        vis_to_latest_image(tart_instance, runtime_config)

    elif ARGS.mode == "qt":
        (
            vis_queue,
            vis_calc_process,
            capture_process,
            vis_calc_cmd_q,
            capture_cmd_q,
        ) = stream_vis_to_queue(tart_instance, runtime_config)
        from monitor_vis import result_loop

        from pyqtgraph.Qt import QtGui, QtCore
        import qt_view

        app = QtGui.QApplication([])
        plotter = qt_view.QtPlotter(app)
        plotQ = plotter.getPort()
        post_process = multiprocessing.Process(
            target=result_loop,
            args=(
                vis_queue,
                runtime_config["chunksize"],
                runtime_config["mode"],
                plotQ,
                runtime_config["calibration_dir"],
            ),
        )
        post_process.start()
        QtGui.QApplication.instance().exec_()
