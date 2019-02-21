'''
    An implementation of the Nelder-Mead simplex method for minimization.
    Copyright T Molteno 2006. tim@physics.otago.ac.nz

    Some useful documents

    http:#www.cs.berkeley.edu/~jrs/4/lec/23
'''
import numpy as np

# Point is an N-dimensional array
# point = np.zeros(N)
# Simplex is N+1 points
# np.array((N+1,N))


class NelderMead(object):
    def __init__(self, f, debug=False):
         self.fun = f
         self.debug = debug
         self.function_eval_count = 0

    def func(self, x):
        self.function_eval_count += 1
        y = self.fun(x)
        if self.debug:
            print(y)
        return y

    ''' A Nelder Mead minimization algorithm
    \param func A function pointer to the function to be minimized
    \param start A starting simplex
    \param tolerance The tolerance of the solution. 
    \return The minimum
    '''
    def solve(self, start, tolerance, max_iterations=500):
        # start simplex and tolerance, optinal set maximum number of iterations
        reflection_coefficient = 1.
        contraction_coefficient = 0.5
        expansion_coefficient = 2.

        n = np.shape(start)[1]
        amoeba = start     # holds vertices of simplex
        # find the initial function values
        f = np.zeros(n+1)
        for j in range(n+1):
            f[j] = self.func(amoeba[j])


        for iteration_count in range(1, max_iterations+1):

            largest_vertex = 0# vertex with largest value
            smallest_vertex = 0# vertex with smallest value
            for j in range(n+1):
                if (f[j] > f[largest_vertex]):
                    largest_vertex = j
                if (f[j] < f[smallest_vertex]):
                    smallest_vertex = j
            # Test for convergence
            test = np.abs(f[largest_vertex] - f[smallest_vertex])/(tolerance+np.abs(f[largest_vertex]) + np.abs(f[smallest_vertex]))
            if test <= tolerance:
                print('convergence tolerance reached {}'.format(test))
                break
    
            centroid_pt = np.zeros(n)
            for m in range(n+1):
                if (m != largest_vertex):
                    centroid_pt = centroid_pt + amoeba[m]
            centroid_pt = centroid_pt / float(n)

            # reflect largest_vertex to new vertex reflect_point
            reflect_point = centroid_pt + (centroid_pt - amoeba[largest_vertex])*reflection_coefficient
            f_reflect = self.func(reflect_point)
    
            if f_reflect < f[smallest_vertex]:
                # We have the best point, now try expansion

                expansion_point = centroid_pt + (reflect_point-centroid_pt)*expansion_coefficient
                f_expand = self.func(expansion_point)
    
                if (f_expand < f_reflect):
                    if (self.debug):
                        print("Improved by reflection and expansion %f" % f_expand)
                    # expansion succeeded - replace the largest point
                    amoeba[largest_vertex] = expansion_point
                    f[largest_vertex] = f_expand
                    continue
                
            # OK expansion did not succeed (or was not tried). Check to see if reflection is OK
            if f_reflect < f[largest_vertex]:
                if (self.debug):
                    print("Improved by reflection %f" % f_reflect)
                # replace the highest with the reflection point
                amoeba[largest_vertex] = reflect_point
                f[largest_vertex] = f_reflect
                continue
            
            # Now try contraction
            contraction_point = centroid_pt + (reflect_point-centroid_pt)*contraction_coefficient
            f_contract = self.func(contraction_point)
            if f_contract < f[largest_vertex]:
                amoeba[largest_vertex] = contraction_point
                f[largest_vertex] = f_contract
                continue
            
            # finally try a shrink step
            if (self.debug):
                print("Doing a shrink step")
            half = 0.5
            center = amoeba[smallest_vertex]
            for vertex in range(n+1):
                if (vertex != smallest_vertex):
                    amoeba[vertex] = center + (amoeba[vertex]-center)*half
                    f[vertex] = self.func(amoeba[vertex])
                
        print("Nelder-Mead Complete")
        print("Iteration Count: " + str(iteration_count))
        print("Function Evaluations: " + str(self.function_eval_count))

        return amoeba[smallest_vertex]
    
if __name__ == '__main__':
    def ftest(x):
        d = np.array([0, 1.0, -1.0])
        return np.sum((x-d)**2)

    def gen_simplex(point, epsilon):
        from copy import deepcopy
        ret = []
        N = len(point)
        ret.append(point)
        for j in range(N):
            x = deepcopy(point)
            x[j] += epsilon
            ret.append(x)
        return np.array(ret)

    nm = NelderMead(ftest)
    start = gen_simplex([1000.0, 1.1, -1.0], 1)
    result = nm.solve(start, tolerance=1e-5, max_iterations=10240)

    print(result)
