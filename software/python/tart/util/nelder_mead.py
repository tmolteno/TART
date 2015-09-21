'''
  An implementation of the Nelder-Mead simplex method for minimization.
  Copyright T Molteno 2006. tim@physics.otago.ac.nz

  Some useful documents

  http:#www.cs.berkeley.edu/~jrs/4/lec/23
'''
import numpy as np

# Point is an N-dimensional array
np.array(N)

''' Simplex is N+1 points
'''
array(n+1 points)
  

class NelderMeade:
  def __init__(self, f):
     self.fun = f
     self.function_eval_count = 0

  def self.func(x):
    self.function_eval_count += 1
    return self.fun(x)
    
  ''' A Nelder Meade minimization algorithm
  \param func A function pointer to the function to be minimized
  \param start A starting simplex
  \param tolerance The tolerance of the solution. 
  \return The minimum
  '''
  point<T,N> solve(const simplex<T,N>& start, const T& tolerance)
  {
    int max_iterations = 500        # maximum number of iterations
    T reflection_coefficient(1)       # reflection coefficient
    T contraction_coefficient = T(1)/2      # contraction coefficient
    T expansion_coefficient = T(2)    # expansion coefficient
  
    int n = N
    int iteration_count       # track the number of iterations
    
    simplex<T,N> amoeba = start   # holds vertices of simplex
    T f[N + 1]            # The function values at the simplex points
    
    # find the initial function values
    for (int j=0j<=nj++):
      f[j] = self.func(amoeba[j])
    
    
    int smallest_vertex = 0# vertex with smallest value
  
    for (iteration_count=0iteration_count<max_iterationsiteration_count++):
    {
      int largest_vertex = 0# vertex with largest value
      smallest_vertex = 0# vertex with smallest value
      for (int j=0j<=nj++):
        if (f[j] > f[largest_vertex]):
          largest_vertex = j
        
        if (f[j] < f[smallest_vertex]):
          smallest_vertex = j
      # Test for convergence
      if ((abs(f[largest_vertex] - f[smallest_vertex])/(abs(f[largest_vertex]) + abs(f[smallest_vertex]))) <= tolerance):
        break
  
      
      point<T,N> centroid_pt# Centroid of all but largest vertex
      for (int m=0m<=nm++):
        if (m != largest_vertex):
          centroid_pt = centroid_pt + amoeba[m]
      
      centroid_pt = centroid_pt / T(n)
  
  
      # reflect largest_vertex to new vertex reflect_point
      point<T,N> reflect_point = centroid_pt + (centroid_pt - amoeba[largest_vertex])*reflection_coefficient
      T f_reflect = self.func(reflect_point)
  
      if ( f_reflect <  f[smallest_vertex]):
        # We have the best point, now try expansion
        point<T,N> expansion_point = centroid_pt + (reflect_point-centroid_pt)*expansion_coefficient
        T f_expand = self.func(expansion_point)
  
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
      point<T,N> contraction_point
      T f_contract
  
      contraction_point = centroid_pt + (reflect_point-centroid_pt)*contraction_coefficient
      f_contract = self.func(contraction_point)
      if (f_contract < f[largest_vertex]):
        amoeba[largest_vertex] = contraction_point
        f[largest_vertex] = f_contract
        continue
      
  
      # finally try a shrink step
      T half = T(1)/2
      for (int vertex=0vertex<=nvertex++):
        if (vertex != smallest_vertex):
          amoeba[vertex] = amoeba[smallest_vertex] + (amoeba[vertex]-amoeba[smallest_vertex])*half
          f[vertex] = self.func(amoeba[vertex])
        
      
    std::cout << "Nelder-Meade Complete" << std::endl
    std::cout << "Function Evaluations: " << ++function_eval_count << std::endl
  
    return amoeba[smallest_vertex]
  
def ftest(x):
  return x[0]

def gen_simplex(point, epsilon):
  ret = []
  N = len(point)
  ret.push(np.zeros(N))
  for j in range(0,N):
    x = np.zeros(N)
    x[j] = epsilon
    ret.push(x)
  return np.array(ret)

start = Simplex([a,b,c])
nm = NelderMeade(ftest, start, 0.1)
result = nm.solve()