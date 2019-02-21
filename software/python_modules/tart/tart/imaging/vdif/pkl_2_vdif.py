# http://www.vlbi.org/vdif/docs/VDIF%20specification%20Release%201.0%20ratified.pdf

import numpy as np
import datetime

#Data Frame Header
def invalid():
    if True:
        return 0
    else:
        return 1
def legacymode():
    return 0
def seconds_from_ref_epoch(date):
    res = np.binary_repr(int((date-ref_e()).total_seconds()))
    return '0'*(30-len(res))+res

def ref_e(): # quick and dirty
    return datetime.datetime(2013,7,1)

def unassigned():
    return '00'

def ref_epoch(date):
    start = datetime.datetime(2000,1,1)
    months = (date.year-start.year)*12 + date.month-start.month
    res = np.binary_repr((months/6)%64)
    return '0'*(6-len(res))+res

def data_frame_no_within_second(no):
    res = np.binary_repr(no)
    return '0'*(24-len(res))+res

def vdif_version_number():
    res = '000' # quick and dirty
    return res

def log2_number_of_channels_in_data_array(channels):
    res = np.binary_repr(np.log2(channels))
    return '0'*(5-len(res))+res

def data_frame_length():
#Data Frame length includes the Data Frame Header and must be a multiple of 8 bytes, with a maximum length of 2**27 bytes
    res = np.binary_repr(np.power(2,10))
    return '0'*(24-len(res))+res

def data_type():
    return '0' # 0 for real data, 1 for complex data

def bits_per_sample_minus1(bps):
    res = np.binary_repr(bps-1)
    return '0'*(5-len(res))+res

def thread_id(t_id):
    res = np.binary_repr(t_id)
    return '0'*(10-len(res))+res

import binascii
def station_id(s_id):
    res = np.binary_repr(s_id)
    return '0'*(16-len(res))+res



testdate=datetime.datetime.now()
testdate=datetime.datetime(2031,1,1)
testframeno = 14
testnumberofchannels= 4
testbitspersample=1
testthreadid=411
teststationid=123

w0 = str(invalid())+str(legacymode())+seconds_from_ref_epoch(testdate)
print(w0, len(w0) # Word 0)
w1 = unassigned()+ref_epoch(testdate)+data_frame_no_within_second(testframeno)
print(w1, len(w1) # Word 1)
w2 = vdif_version_number() + log2_number_of_channels_in_data_array(testnumberofchannels) + data_frame_length()
print(w2, len(w2) # Word 2)
w3 = data_type() + bits_per_sample_minus1(testbitspersample) + thread_id(testthreadid) + station_id(teststationid)
print(w3, len(w3) # Word 3)
