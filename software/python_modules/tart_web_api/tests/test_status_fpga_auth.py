"""Regression tests for issue #27: GET /status/fpga must require a JWT.

The real ``tart_web_api.main`` module cannot be imported inside a test
process: at import time it creates a sqlite database, reads
``/config_data/*.json`` and spawns multiprocessing workers that drive the
SPI hardware (``tart_hardware_interface``).  We therefore install a
lightweight stand-in module under the name ``tart_web_api.main`` — with the
same Flask + flask-jwt-extended configuration as the real module (secret
key and ``JWT_HEADER_TYPE = 'JWT'``) — and then import the *real*
``tart_web_api.views`` and ``tart_web_api.views_auth``.  That way the route
and decorators under test are the genuine ones, and access tokens are minted
by the genuine ``POST /auth`` endpoint.
"""
import os
import sys
import types

import pytest
from flask import Flask
from flask_jwt_extended import JWTManager

import tart_web_api  # noqa: F401  (package root added to sys.path by conftest)


def _build_stub_main():
    """Stand-in for tart_web_api.main carrying the real module's JWT config."""
    app = Flask(__name__)
    app.secret_key = 'test-secret-key-0123456789abcdef0123456789ab'  # >= 32 bytes
    app.config['JWT_HEADER_TYPE'] = 'JWT'  # like main.py: app.config['JWT_HEADER_TYPE']
    jwt = JWTManager(app)

    app.config['CONFIG'] = {
        'hostname': 'tart-test-host',
        'mode': 'vis',
        'modes_available': ['off', 'diag', 'raw', 'vis', 'vis_save', 'cal',
                            'rt_syn_img'],
        'status': {
            'timestamp (UTC)': '2026-10-02T00:00:00',
            'AQ_STREAM': {'data': 0},
            # The registers issue #27 is about:
            'TC_STATUS': {'delta': 42, 'phase': 7},
            'TC_SYSTEM': {'enabled': 1, 'error': 0, 'locked': 1, 'source': 0},
        },
    }

    stub = types.ModuleType('tart_web_api.main')
    stub.app = app
    stub.jwt = jwt
    sys.modules['tart_web_api.main'] = stub
    tart_web_api.main = stub
    return stub


# Must happen before the views imports below (they do
# `from tart_web_api.main import app`).
_stub_main = _build_stub_main()

from tart_web_api import views  # noqa: E402  (registers /status/fpga et al.)
from tart_web_api import views_auth  # noqa: E402  (registers POST /auth)

app = _stub_main.app


@pytest.fixture()
def client():
    app.config['TESTING'] = True
    return app.test_client()


def _login(client, password=None):
    """Obtain an access token through the real POST /auth endpoint."""
    if password is None:
        password = os.environ.get('LOGIN_PW', 'password')  # see views_auth.py
    resp = client.post('/auth', json={'username': 'admin', 'password': password})
    assert resp.status_code == 200, resp.get_data(as_text=True)
    return resp.get_json()['access_token']


def _auth_header(token):
    # main.py sets JWT_HEADER_TYPE = 'JWT', not the 'Bearer' default.
    return {'Authorization': 'JWT {}'.format(token)}


def test_status_fpga_rejects_unauthenticated_request(client):
    """The gap issue #27 was filed for: anonymous GETs must now 401."""
    resp = client.get('/status/fpga')
    assert resp.status_code == 401

    # ...and with exactly the same error shape as the pre-existing
    # jwt-guarded routes (e.g. POST /mode/<mode> in views.py).
    assert client.post('/mode/vis').status_code == 401
    mode_resp = client.post('/mode/vis')
    assert resp.get_json() == mode_resp.get_json()
    assert 'msg' in resp.get_json()


def test_status_fpga_accepts_valid_token(client):
    token = _login(client)
    resp = client.get('/status/fpga', headers=_auth_header(token))
    assert resp.status_code == 200
    body = resp.get_json()
    assert body['hostname'] == 'tart-test-host'
    # The clock-recovery registers requested in issue #27 are served:
    assert body['TC_STATUS'] == {'delta': 42, 'phase': 7}
    assert body['TC_SYSTEM']['locked'] == 1


def test_status_fpga_rejects_bad_password_login(client):
    resp = client.post('/auth', json={'username': 'admin', 'password': 'nope'})
    assert resp.status_code == 401


def test_status_fpga_rejects_wrong_header_type(client):
    """Only the configured 'JWT <token>' header form is accepted."""
    token = _login(client)
    resp = client.get('/status/fpga',
                      headers={'Authorization': 'Bearer {}'.format(token)})
    assert resp.status_code == 401


def test_preexisting_guarded_route_works_with_token(client):
    """Sanity check that the harness matches the app's real JWT behaviour."""
    token = _login(client)
    resp = client.post('/mode/vis', headers=_auth_header(token))
    assert resp.status_code == 200
    assert resp.get_json() == {'mode': 'vis'}
