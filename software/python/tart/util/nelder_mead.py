'''
  An implementation of the Nelder-Mead simplex method for minimization.
  Copyright T Molteno 2006. tim@physics.otago.ac.nz

  Some useful documents

  http:#www.cs.berkeley.edu/~jrs/4/lec/23
'''
import numpy as np

# Point is an N-dimensional array
# point = np.zeros(N)

''' Simplex is N+1 points
'''
# np.array((N+1,N))


class NelderMead:
  def __init__(self, f):
     self.fun = f
     self.function_eval_count = 0

  def func(self,x):
    self.function_eval_count += 1
    return self.fun(x)
    
  ''' A Nelder Meade minimization algorithm
  \param func A function pointer to the function to be minimized
  \param start A starting simplex
  \param tolerance The tolerance of the solution. 
  \return The minimum
  '''
  # point<T,N> solve(const simplex<T,N>& start, const T& tolerance)
  def solve(self,start, tolerance, max_iterations = 500):
    # start simplex and tolerance
    # maximum number of iterations
    # T reflection_coefficient(1)       # reflection coefficient
    # T contraction_coefficient = T(1)/2      # contraction coefficient
    # T expansion_coefficient = T(2)    # expansion coefficient

    reflection_coefficient = 1.
    contraction_coefficient = 1/2.
    expansion_coefficient = 2.
  
    # n = N
    n = np.shape(start)[1]
    amoeba = start   # holds vertices of simplex

    # T f[N + 1]            # The function values at the simplex points
    
    # find the initial function values
    f = np.zeros(n+1)
    for j in range(n+1):
      f[j] = self.func(amoeba[j])
    
    
    smallest_vertex = 0# vertex with smallest value
    # track the number of iterations
    for iteration_count in range(1,max_iterations+1):
      largest_vertex = 0# vertex with largest value
      smallest_vertex = 0# vertex with smallest value
      # for (int j=0j<=nj++):
      for j in range(n+1):
        if (f[j] > f[largest_vertex]):
          largest_vertex = j
        if (f[j] < f[smallest_vertex]):
          smallest_vertex = j
      # Test for convergence
      if ((abs(f[largest_vertex] - f[smallest_vertex])/(abs(f[largest_vertex]) + abs(f[smallest_vertex]))) <= tolerance):
        break
  
      # centroid_pt point
      # point<T,N> centroid_pt # Centroid of all but largest vertex
      centroid_pt = np.zeros(n)
      for m in range(n+1):
        if (m != largest_vertex):
          centroid_pt = centroid_pt + amoeba[m]
      centroid_pt = centroid_pt / float(n)

      # centroid_pt = centroid_pt / T(n)
  
      # reflect largest_vertex to new vertex reflect_point
      reflect_point = centroid_pt + (centroid_pt - amoeba[largest_vertex])*reflection_coefficient
      f_reflect = self.func(reflect_point)
  
      if ( f_reflect <  f[smallest_vertex]):
        # We have the best point, now try expansion
        expansion_point = centroid_pt + (reflect_point-centroid_pt)*expansion_coefficient
        f_expand = self.func(expansion_point)
  
        if (f_expand < f_reflect):
          # expansion succeeded - replace the largest point
          amoeba[largest_vertex] = expansion_point
          f[largest_vertex] = f_expand
          continue
        
      
  
      # OK expansion did not succeed (or was not tried). Check to see if reflection is OK
      if (f_reflect < f[largest_vertex]):
        # replace the highest with the reflection point
        amoeba[largest_vertex] = reflect_point
        f[largest_vertex] = f_reflect
        continue
      
      
      # Now try contraction
      # point<T,N> contraction_point
      # T f_contract
  
      contraction_point = centroid_pt + (reflect_point-centroid_pt)*contraction_coefficient
      f_contract = self.func(contraction_point)
      if (f_contract < f[largest_vertex]):
        amoeba[largest_vertex] = contraction_point
        f[largest_vertex] = f_contract
        continue
      
  
      # finally try a shrink step
      half = 1/float(2) #T(1)/2
      for vertex in range(n+1):
        if (vertex != smallest_vertex):
          amoeba[vertex] = amoeba[smallest_vertex] + (amoeba[vertex]-amoeba[smallest_vertex])*half
          f[vertex] = self.func(amoeba[vertex])
        
    print "Nelder-Meade Complete"
    print "Iteration Count: " + str(iteration_count)
    print "Function Evaluations: " + str(self.function_eval_count)

    return amoeba[smallest_vertex]
  
def ftest(x):
  d = np.array([0,1.,1.])
  return np.sum((x-d)**2)

def gen_simplex(point, epsilon):
  ret = []
  N = len(point)
  ret.append(np.zeros(N))
  for j in range(N):
    x = np.zeros(N)
    x[j] += epsilon
    ret.append(x)
  return np.array(ret)


nm = NelderMead(ftest)
start = gen_simplex([0.,0.1,0.],1)
result = nm.solve(start, 0.000000000001)

print result