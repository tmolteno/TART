'''
Usage: testbench.py [options] [test] [...]

Options:
  -h, --help       Show this message
  -v, --verbose    Verbose output
  -q, --quiet      Minimal output
  -f, --failfast   Stop on first failure
  -c, --catch      Catch control-C and display results
  -b, --buffer     Buffer stdout and stderr during test runs

Examples:
  testbench.py                               - run default set of tests
  testbench.py MyTestSuite                   - run suite 'MyTestSuite'
  testbench.py MyTestCase.testSomething      - run MyTestCase.testSomething
  testbench.py MyTestCase                    - run all 'test*' test methods
                                               in MyTestCase

  python testbench.py TestEphemeridesProxy.test_sp3_proxying
'''
import unittest
from tart.imaging.test.all import *
from tart.operation.test.all import *
from tart.simulation.test.all import *
from tart.util.test.all import *

if __name__ == '__main__':
    unittest.main()
