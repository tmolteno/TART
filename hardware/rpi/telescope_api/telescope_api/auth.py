import os
from werkzeug.security import safe_str_cmp

# Add authentication to app
class User(object):
    def __init__(self, id, username, password):
        self.id = id
        self.username = username
        self.password = password



if os.environ.has_key('LOGIN_PW'):
  pw = os.environ['LOGIN_PW']
else:
  pw = 'password'

Admin = User(1,'admin',pw)

def authenticate(username, password):
    if safe_str_cmp(Admin.password.encode('utf-8'), password.encode('utf-8')):
       return Admin

def identity(payload):
    return Admin
