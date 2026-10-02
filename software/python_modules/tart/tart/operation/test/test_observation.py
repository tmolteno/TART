import unittest
import datetime
import gzip
import os
import pickle
import pickletools
import tempfile

import numpy as np

from tart.operation.observation import Observation, Observation_Load
from tart.operation import settings


def _dumps_like_python2(obj):
    """Serialize obj the way a Python 2 writer would: a protocol 2 stream
    holding raw 8-bit byte strings (BINSTRING opcodes) instead of Python 3's
    byte-string opcodes.

    Python 3 never emits BINSTRING: at protocol >= 3 bytes become BINBYTES,
    at protocol 2 they are hidden behind ``_codecs.encode(str, 'latin1')``.
    Neither trips Python 3's default ASCII decoding, so we build the stream
    from a protocol-3 dump, rewrite BINBYTES to the py2 BINSTRING opcodes
    (identical framing) and downgrade PROTO to 2. Unpickling the result under
    Python 3 decodes the byte strings with pickle.load's `encoding` argument
    (ASCII unless overridden), which is what broke loading historic *.pkl
    files (issue #48).
    """
    blob = pickle.dumps(obj, protocol=3)
    buf = bytearray(blob)
    for opcode, _arg, pos in pickletools.genops(blob):
        if opcode.name == 'PROTO':
            buf[pos + 1] = 2                    # a py2 writer maxes out at protocol 2
        elif opcode.name == 'BINBYTES':         # py3-only: 4-byte length + payload
            buf[pos] = ord('T')                 # py2 BINSTRING, identical framing
        elif opcode.name == 'SHORT_BINBYTES':   # py3-only: 1-byte length + payload
            buf[pos] = ord('U')                 # py2 SHORT_BINSTRING, identical framing
    return bytes(buf)


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


    def test_load_py2_pickle_with_non_ascii(self):
        # Regression test for issue #48: *.pkl files written by Python 2
        # contain raw 8-bit byte strings. Observation_Load must decode them
        # with encoding="latin1" instead of raising
        # UnicodeDecodeError: 'ascii' codec can't decode byte 0xe4 ...
        config_dict = dict(self.config.Dict)
        # A Python-2 `str` holding non-ASCII (latin-1) bytes; 0xe4 sits at
        # position 1, exactly as in the traceback reported in issue #48.
        config_dict['site_name'] = 'M\xe4nchen-S\xfcd'.encode('latin1')

        payload = {
            'config': config_dict,
            'timestamp': self.obs.timestamp,
            'data': [np.packbits(np.asarray(row, dtype=np.uint8))
                     for row in self.data],
        }
        blob = _dumps_like_python2(payload)

        # Exercise both branches of Observation_Load: a plain (non-gzipped)
        # pickle and a gzipped one (what Observation.save produces).
        for gzipped in (False, True):
            with self.subTest(gzipped=gzipped):
                fd, path = tempfile.mkstemp(suffix='.pkl')
                os.close(fd)
                try:
                    if gzipped:
                        with gzip.open(path, 'wb') as save_ptr:
                            save_ptr.write(blob)
                    else:
                        with open(path, 'wb') as save_ptr:
                            save_ptr.write(blob)

                    nobs = Observation_Load(path)

                    self.assertEqual(nobs.config.Dict['site_name'],
                                     'M\xe4nchen-S\xfcd')
                    self.assertEqual(nobs.timestamp, self.obs.timestamp)
                    self.assertTrue((self.data == nobs.data).all())
                finally:
                    os.remove(path)


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
