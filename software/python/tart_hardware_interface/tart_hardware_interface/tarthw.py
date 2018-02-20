import pkg_resources
from tart_hardware_interface.tartaxi import TartAXI
from tart_hardware_interface.tartspi import TartSPI

PERMUTE_FILE=pkg_resources.resource_filename('tart_hardware_interface','permute.txt')

def get_tart_hw():
  return TartAXI()

