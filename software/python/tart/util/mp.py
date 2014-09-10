from multiprocessing import Pool

def parallelized_for_i_in(ilist, func):
  pool = Pool()
  resultList = []
  ret = []
  try:
    for iarg in ilist:
      resultList.append(pool.apply_async(func, (iarg,)))
    pool.close 
    pool.join

    for thread in resultList:
      x2 = thread.get()
      if (x2 != None):
        ret.append(x2)
    pool.terminate()
  except KeyboardInterrupt:
    print 'control-c pressed'
    pool.terminate()
  return ret

