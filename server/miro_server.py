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
from distutils.dir_util import copy_tree

ZIP_IGNORE_FILES = ['.DS_Store']

def zipdir(path, ziph):
  for root, dirs, files in os.walk(path):
      for file in files:
          if file in ZIP_IGNORE_FILES:
            continue
          
          ziph.write(os.path.join(root, file),
            os.path.join('miro_server', os.path.relpath(os.path.join(root, file), path)))

def gen_password(length):
  return ''.join(random.choices(string.ascii_uppercase + string.ascii_lowercase + string.digits, 
    k=length))


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

    with open('version', 'r') as f:
      self.__version_string = f.read().strip()

    getattr(self, args.command)()


  def build(self):
    parser = argparse.ArgumentParser(
          description='Builds GAMS MIRO Server from source')
    parser.add_argument('--module',
      type=str,
      help='Module to build',
      choices=['dockerproxy', 'proxy'])

    parser.add_argument('--no-pull', help='Do not pull images from hub.gams.com', 
      action='store_true')
    parser.add_argument('--shinyproxy', '-sp', action='store_true', help='Build shinyproxy from source.')

    args = parser.parse_args(sys.argv[2:])

    if args.shinyproxy or len(glob.glob(os.path.join('proxy', 'shinyproxy-*.jar'))) == 0:
      mvn_executable = 'mvn'
      if platform.system() == 'Windows':
        mvn_executable = 'mvn.cmd'
      subprocess.check_call([mvn_executable, '-U', 'clean', 'install', '-f', os.path.join('shinyproxy', 'pom.xml')])
      artifact_path = glob.glob(os.path.join('shinyproxy', 'target', 'shinyproxy-*.jar'))

      if len(artifact_path) == 0:
        print("Something went wrong building shinyproxy artifact. Check whether Maven is correctly installed.")
        exit(1)

      os.rename(artifact_path[0], os.path.join('proxy', os.path.basename(artifact_path[0])))

    if not os.path.isfile('.env'):
      with open('.env', 'w') as f:
        f.write(f'GMS_MIRO_DATABASE_PWD={gen_password(30)}\nGMS_MIRO_SA_PWD={gen_password(20)}\n')

    if not args.no_pull:
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
    parser = argparse.ArgumentParser(
        description='Pushes GAMS MIRO Server images to hub.gams.com')
    parser.add_argument('--password', '-p', help='Password')
    parser.add_argument('--username', '-u', help='Username')

    args = parser.parse_args(sys.argv[2:])

    docker_login_args = ['docker', 'login', 'hub.gams.com']

    if args.username is not None:
      if args.password is None:
        print('You must specify both username and password!')
        exit(1)

      docker_login_args.extend(['-u', args.username, '-p', args.password])


    subprocess.check_call(docker_login_args)

    for image in [('gamsmiro-sproxy', 'gamsmiro-sproxy'),
                  ('gamsmiro-proxy', 'gamsmiro-proxy')]:
      self.push_image(*image)


  def release(self):
    parser = argparse.ArgumentParser(
          description='Releases GAMS MIRO Server')

    args = parser.parse_args(sys.argv[2:])

    release_zip_filename = 'miro_server.zip'

    if os.path.isfile(release_zip_filename):
      remove_previous_release = input('Release file already exists. Remove it? [y/N]\n')
      if remove_previous_release != 'y':
        print('Release was interrupted.')
        exit(0)
      os.remove(release_zip_filename)

    if os.path.isdir('release'):
      shutil.rmtree('release')

    os.mkdir('release')

    python_binary = 'python3'

    if platform.system() == 'Windows':
      python_binary = 'python'

    copy_tree('release_data', 'release')
    
    env_file_content = f'GMS_MIRO_DATABASE_PWD={gen_password(40)}\nGMS_MIRO_SA_PWD={gen_password(30)}\n'

    with open(os.path.join('release', '.env'), 'w') as f:
      f.write(env_file_content)


    answers = ['y', 'Y', 'n', 'N', '']
    yes_answers = ['y', 'Y']

    release_windows = input('Do you want to release MIRO Server for a Windows Server? [y/N]: ').strip()
    while release_windows not in answers:
        release_windows = input('Do you want to release MIRO Server for a Windows Server? [y/N]: ').strip()

    if release_windows in yes_answers:
      os.remove(os.path.join('release', 'miro-compose'))
    else:
      os.remove(os.path.join('release', 'miro-compose.ps1'))

    shutil.copy('LICENSE', os.path.join('release', 'LICENSE'))

    release_zip_file = zipfile.ZipFile(release_zip_filename, 'w', zipfile.ZIP_DEFLATED)
    zipdir('release', release_zip_file)
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
