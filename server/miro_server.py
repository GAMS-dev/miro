#!/usr/bin/env python3
from __future__ import print_function
import argparse

import os
import sys
import subprocess
import glob
import shutil
import zipfile
import random
import re
import platform
import string
import json
from distutils.dir_util import copy_tree

ZIP_IGNORE_FILES = ['.DS_Store']

def gen_password(length):
  return ''.join(random.choices(string.ascii_uppercase + string.ascii_lowercase + string.digits, 
    k=length))


def gen_env_file(env_path):
  with open(env_path, 'w') as f:
    f.write(f'GMS_MIRO_DATABASE_PWD={gen_password(40)}\nGMS_MIRO_ENGINE_ANONYMOUS_USER=miro_server_anonymous\nGMS_MIRO_ENGINE_ANONYMOUS_PWD={gen_password(40)}\n')


class MiroServer(object):
  def __init__(self):
    parser = argparse.ArgumentParser(prog='miro_server.py',
      usage='miro_server [-h] {build,up,down,push,release} [<args>]',
      description='GAMS MIRO Server build script')

    # Add the arguments
    parser.add_argument('command',
      type=str,
      help='Subcommand to run',
      choices=['build', 'up', 'down', 'push', 'release'])

    args = parser.parse_args(sys.argv[1:2])

    self.__compose_env = os.environ.copy()
    self.__compose_env['COMPOSE_PROJECT_NAME'] = 'miro_server'
    self.__compose_env['COMPOSE_IGNORE_ORPHANS'] = 'True'

    with open(os.path.join('..', 'package.json'), 'r') as f:
      self.__version_string = json.loads(f.read())['version'].strip()

    getattr(self, args.command)()


  def build(self):
    parser = argparse.ArgumentParser(
          description='Builds GAMS MIRO Server from source')
    parser.add_argument('--module',
      type=str,
      help='Module to build',
      choices=['dockerproxy', 'proxy'])

    parser.add_argument('--pull', help='Pull images from hub.gams.com', 
      action='store_true')

    args = parser.parse_args(sys.argv[2:])

    if not os.path.isfile('.env'):
      gen_env_file('.env')

    if args.pull:
      subprocess.check_call(['docker', 'login', 'hub.gams.com'])
      subprocess.check_call(['docker-compose', 'pull'], env=self.__compose_env)

    if args.module is None:
      subprocess.check_call(['docker-compose', 'build'], env=self.__compose_env)
    else:
      subprocess.check_call(['docker-compose', 'build', args.module], env=self.__compose_env)


  def up(self):
    parser = argparse.ArgumentParser(
          description='Launches GAMS MIRO Server')

    subprocess.check_call(['docker-compose', 'up', '-d'], env=self.__compose_env)


  def down(self):
    parser = argparse.ArgumentParser(
          description='Launches GAMS MIRO Server')
    parser.add_argument('-v', '--volumes', help='Removes volumes and networks', 
      action='store_true')

    args = parser.parse_args(sys.argv[2:])

    dc_args_miro = ['docker-compose', 'down']
    dc_args_engine = ['docker-compose', '-f', 'docker-compose.miro.yml', 'down']

    self.stop_proxies('hub.gams.com/gamsmiro-admin')
    self.stop_proxies('hub.gams.com/gamsmiro-ui')

    if args.volumes:
      dc_args_miro.append('-v')
    
    subprocess.check_call(dc_args_miro, env=self.__compose_env)


  def push(self):
    for image in [('gamsmiro-sproxy', 'gamsmiro-sproxy'),
                  ('gamsmiro-proxy', 'gamsmiro-proxy'),
                  ('gamsmiro-auth', 'gamsmiro-auth')]:
      self.push_image(*image)


  def release(self):
    parser = argparse.ArgumentParser(
          description='Releases GAMS MIRO Server')

    parser.add_argument('-f', '--force', help='Overwrite release if it exists', 
      action='store_true')

    args = parser.parse_args(sys.argv[2:])

    release_zip_filename = 'miro_server.zip'

    if os.path.isfile(release_zip_filename):
      if not args.force:
        print('Previous release exists and --force was not set.')
        exit(0)
      os.remove(release_zip_filename)

    if os.path.isdir('release'):
      shutil.rmtree('release')

    os.mkdir('release')

    python_binary = 'python3'

    if platform.system() == 'Windows':
      python_binary = 'python'

    copy_tree('release_data', 'release')

    shutil.copy('LICENSE', os.path.join('release', 'LICENSE'))

    release_zip_file = zipfile.ZipFile(release_zip_filename, 'w', zipfile.ZIP_DEFLATED)

    for root, dirs, files in os.walk('release'):
        for file in files:
            if file in ZIP_IGNORE_FILES:
                continue

            name_in_zip = os.path.join('miro_server',
                                       os.path.relpath(os.path.join(root, file), 'release'))
            if os.name == 'nt' and file == 'miro-compose':
                # we have to manually set executable bit of miro-compose on Windows
                upsh = zipfile.ZipInfo(name_in_zip)
                upsh.date_time = time.localtime()
                upsh.external_attr = 0o100755 << 16
                upsh.create_system = 3
                with open(os.path.join(root, file), 'rb') as f:
                    upsh_content = f.read()

                # make sure file has unix line endings
                upsh_content = upsh_content.replace(b'\r\n', b'\n')

                release_zip_file.writestr(upsh, upsh_content)
                continue

            release_zip_file.write(os.path.join(root, file),
                                   name_in_zip)

    release_zip_file.close()
    print(f"GAMS MIRO Server release file was written to: {os.path.join(os.getcwd(), release_zip_filename)}")


  def stop_proxies(self, image_name):
    active_admin_containers = subprocess.run(['docker', 'container', 'ls', '-f', 'network=miroserver-network', '-f', 
      f'ancestor={image_name}', '--format', '{{.ID}}'], capture_output=True).stdout.strip()

    if not active_admin_containers:
      return

    active_admin_containers = active_admin_containers.decode("utf-8").split('\n')

    if len(active_admin_containers) > 0:
      subprocess.check_call([*['docker', 'stop'], *active_admin_containers])
      subprocess.check_call([*['docker', 'rm'], *active_admin_containers])


  def push_image(self, image_name_local, image_name_hub):
    subprocess.check_call(['docker', 'tag', image_name_local, f'hub.gams.com/{image_name_hub}'])
    subprocess.check_call(['docker', 'tag', image_name_local, f'hub.gams.com/{image_name_hub}:{self.__version_string}'])
    subprocess.check_call(['docker', 'push', f'hub.gams.com/{image_name_hub}'])
    subprocess.check_call(['docker', 'push', f'hub.gams.com/{image_name_hub}:{self.__version_string}'])


if __name__ == '__main__':
    MiroServer()

