"""pytest configuration for the tart_web_api test-suite.

Puts the package root on sys.path so `import tart_web_api` works no matter
where pytest is invoked from (the package is not installed in the test
environment).
"""
import os
import sys

_PACKAGE_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
if _PACKAGE_ROOT not in sys.path:
    sys.path.insert(0, _PACKAGE_ROOT)
