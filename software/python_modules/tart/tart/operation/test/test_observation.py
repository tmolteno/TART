import unittest
import datetime
import numpy as np

from tart.operation.observation import Observation, Observation_Load
from tart.operation import settings


class TestObservation(unittest.TestCase):

    def setUp(self):

        ts = datetime.datetime.utcnow()
        self.config = settings.from_file('tart/test/test_telescope_config.json')
        self.test_len = 2**8
        # generate some fake data
        self.data = [np.random.randint(0,2,self.test_len) for _ in range(self.config.get_num_antenna())]
        self.obs = Observation(timestamp=ts, config=self.config, data=self.data)


    def test_load_save(self):
        self.obs.save('data.txt')

        nobs = Observation_Load('data.txt')

        self.assertTrue((self.data == nobs.data).all())
        self.assertTrue((self.obs.get_antenna(1) == nobs.get_antenna(1)).all())
        self.assertEqual(self.obs.get_julian_date(), nobs.get_julian_date())


    def test_hdf5_load_save(self):
        self.obs.to_hdf5('data.hdf')

        nobs = Observation.from_hdf5('data.hdf')

        self.assertTrue((self.obs.get_antenna(1) == nobs.get_antenna(1)).all())
        self.assertEqual(self.obs.get_julian_date(), nobs.get_julian_date())
        self.assertTrue((self.data == nobs.data).all())


    #def test_str2bits(self):
        #init = '101000'
        #res = np.array([1,0,1,0,0,0])
        #self.assertTrue((res == str2bits(init)[0]).all())

    #def test_bit2int(self):
        #init = np.array([1,0,1,0,0,0,1,0,1,0,0,0])
        #res = np.array([ 2**7+2**5+2**1, 2**3])
        #self.assertTrue((bit2int(init) == res).all())

    #def test_bits2str(self):
        #init = np.array([1,0,1,0,0,0,1,0,1,0,0,0])
        #res = np.array([ 2**7+2**5+2**1, 2**3])
        #self.assertTrue((bit2int(init) == res).all())

    #def test_int2bin_str_with_n_leading_zeros(self):
        #self.assertTrue((int2bin_str_with_n_leading_zeros(0, 5) == '101'))
        #self.assertTrue((int2bin_str_with_n_leading_zeros(1, 5) == '101'))
        #self.assertTrue((int2bin_str_with_n_leading_zeros(2, 5) == '101'))
        #self.assertTrue((int2bin_str_with_n_leading_zeros(3, 5) == '101'))
        #self.assertTrue((int2bin_str_with_n_leading_zeros(6, 5) == '000101'))
        #self.assertTrue((int2bin_str_with_n_leading_zeros(8, 5) == '00000101'))

    #def test_int2bit(self):
        #init = np.array([244, 0, 1])
        #res1 = np.array([1, 1, 1, 1, 0, 1, 0, 0,    0, 0, 0, 0, 0, 0, 0, 0,    1])
        #res2 = np.array([1, 1, 1, 1, 0, 1, 0, 0,    0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 1])
        #res3 = np.array([1, 1, 1, 1, 0, 1, 0, 0,    0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0, 1])
        #res4 = np.array([1, 1, 1, 1, 0, 1, 0, 0,    0, 0, 0, 0, 0, 0, 0, 0,    0, 0, 0, 0, 0, 0, 0, 1])
        #l1=2*8 + 1
        #l2=2*8 + 3
        #l3=2*8 + 5
        #l4=3*8
        #self.assertTrue((int2bit(init, l1) == res1).all())
        #self.assertTrue((int2bit(init, l2) == res2).all())
        #self.assertTrue((int2bit(init, l3) == res3).all())
        #self.assertTrue((int2bit(init, l4) == res4).all())

    #def test_conversions(self):
        ## go twice through all conversions.
        #bitseq = np.array([0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0])
        #leng = len(bitseq)

        #intseq = bit2int(bitseq)
        #bitseq_f = int2bit(intseq, leng)
        #intseq_f = bit2int(bitseq_f)
        #bitseq_ff = int2bit(intseq_f, leng)
        #intseq_ff = bit2int(bitseq_ff)

        #self.assertTrue((bitseq == bitseq_f).all())
        #self.assertTrue((intseq == intseq_f).all())

        #self.assertTrue((bitseq == bitseq_ff).all())
        #self.assertTrue((intseq == intseq_ff).all())
