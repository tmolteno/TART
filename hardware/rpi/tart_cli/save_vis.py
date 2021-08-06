import time, os
import traceback
from tart.imaging import visibility


def result_loop(result_queue, runtime_config, logger, single=False):
    vislist = []
    active = True
    while active:
        if False == result_queue.empty():
            try:
                vis, means = result_queue.get()
                vislist.append(vis)
                if len(vislist) >= runtime_config["chunksize"]:
                    fname = (
                        runtime_config["vis_prefix"]
                        + "_"
                        + vis.timestamp.strftime("%Y-%m-%d_%H_%M_%S.%f")
                        + ".vis"
                    )
                    print("saved ", vis, " to", fname)
                    visibility.Visibility_Save(vislist, fname)
                    if single:
                        active = False
                    vislist = []
            except Exception as e:
                logger.error("PostProcessing Error %s" % str(e))
                logger.error(traceback.format_exc())
        else:
            time.sleep(0.001)
    return 1
