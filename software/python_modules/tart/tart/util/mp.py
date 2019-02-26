from multiprocessing import Pool

def parallelized_for_i_in(ilist, func):
    pool = Pool()
    result_list = []
    ret = []
    try:
        for iarg in ilist:
            result_list.append(pool.apply_async(func, (iarg,)))
        pool.close
        pool.join

        for thread in result_list:
            result = thread.get()
            if result is not None:
                ret.append(result)
        pool.terminate()
    except KeyboardInterrupt:
        print('control-c pressed')
        pool.terminate()
    return ret

