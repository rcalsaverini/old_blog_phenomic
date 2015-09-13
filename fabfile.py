from fabric.api import *
import fabric.contrib.project as project
import os
import shutil
import sys
import SocketServer

from pelican.server import ComplexHTTPRequestHandler

# Local path configuration (can be absolute or relative to fabfile)
env.deploy_path = '.'
DEPLOY_PATH = env.deploy_path

# Port for `serve`
PORT = 8000

def build(theme):
    """Build local version of site"""
    local('pelican -s pelicanconf.py -t {theme} -o {output}'.format(theme=theme, output=env.deploy_path))

def rebuild():
    """`clean` then `build`"""
    clean()
    build()

def regenerate():
    """Automatically regenerate site upon file modification"""
    local('pelican -r -s pelicanconf.py -t {theme}'.format(theme=env.theme))

def serve():
    """Serve site at http://localhost:8000/"""
    os.chdir(env.deploy_path)

    class AddressReuseTCPServer(SocketServer.TCPServer):
        allow_reuse_address = True

    server = AddressReuseTCPServer(('', PORT), ComplexHTTPRequestHandler)

    sys.stderr.write('Serving on port {0} ...\n'.format(PORT))
    server.serve_forever()

def reserve(theme):
    """`build`, then `serve`"""
    build(theme)
    serve()
