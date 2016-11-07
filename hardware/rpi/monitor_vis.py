
def gen_calib_image(vislist, gfx, fig, ax, cb):
  CAL_MEASURE_VIS_LIST = []
  for vis in vislist:
    cv = calibration.CalibratedVisibility(vis)
    for i in range(6,24):
      cv.flag_antenna(i)
    CAL_MEASURE_VIS_LIST.append(cv)

  CV = CAL_MEASURE_VIS_LIST #[0:1]
  CAL_SYN = synthesis.Synthesis_Imaging(CV)
  CAL_IFT, CAL_EXTENT = CAL_SYN.get_ift(nw=50, num_bin=2**8, use_kernel=False)
  abs_v = CAL_IFT.real
  if gfx is None:
    gfx = ax.imshow(abs_v, extent=CAL_EXTENT, cmap=plt.cm.rainbow)
    cb = fig.colorbar(gfx, orientation='horizontal')
  else:
    gfx.set_data(abs_v)
    cb.set_clim(vmin=np.min(abs_v), vmax=np.max(abs_v))
    cb.draw_all()
    fig.canvas.draw()
  return gfx, cb

def process_loop(process_queue, config, n_samples, chunck_size):
    import matplotlib.pyplot as plt
    from tart.imaging import calibration
    from tart.imaging import synthesis
    plt.ion()
    fig, ax = plt.subplots(1,1)
    gfx = None
    cb = None
    cnt = 0
    res = []
    vislist = []
    while (True):
        if (False == process_queue.empty()):
            try:
                data = process_queue.get()
                vis = get_vis_object(data, n_samples, config)
                vislist.insert(0,vis)
                if ARGS.synthesis:
                  if len(vislist)>5:
                    gfx, cb = gen_calib_image(vislist, gfx, fig, ax, cb)
                    vislist = []
                if ARGS.absang:
                  if gfx is None:
                    gfx, = ax.plot([],[],c='blue')
                    ax2 = ax.twinx()
                    gfx2, = ax2.plot([],[], c='red')
                  max_len = 100
                  update_len = 20
                  if len(vislist)>max_len:
                    vislist.pop()
                  vis_list_01 = [np.angle(i.v[4]) for i in vislist]
                  vis_list_01_m = [np.abs(i.v[4]) for i in vislist]
                  cnt += 1
                  if cnt == update_len:
                    plt.xlim(0,max_len)
                    ax.set_ylim(-4,4)
                    ax2.set_ylim(0.05,0.3)
                    gfx.set_data(np.arange(len(vis_list_01)),vis_list_01)
                    gfx2.set_data(np.arange(len(vis_list_01_m)),vis_list_01_m)
                    fig.canvas.draw()
                    cnt = 0
                if ARGS.calib:
                  cnt +=1
                  print cnt
                  if cnt>10:
                    res = []
                    labels = []
                    for i in range(1):
                      for j in range(i+1,6):
                        temp = []
                        temp_abs = []
                        labels.append('%i_%i_ang' % (i,j))
                        labels.append('%i_%i_abs' % (i,j))
                        for vi in vislist:
                          cv = calibration.CalibratedVisibility(vi)
                          temp.append(np.angle(cv.get_visibility(i,j)))
                          temp_abs.append(np.abs(cv.get_visibility(i,j)))
                        res.append(temp)
                        print 'p', i, j, np.mean(temp)
                        print 'abs', i, j, np.mean(temp_abs)
                        res.append(temp_abs)
                    res = np.array(res)
                    print res
                    for r,label in zip(res, labels):
                      plt.plot(r,label=label)
                    plt.legend()
                    plt.waitforbuttonpress()

            except Exception, e:
                logger.error( "Measurement Processing Error %s" % str(e))
                logger.error(traceback.format_exc())
        time.sleep(0.01)




