#!/usr/bin/env python3
from __future__ import print_function
import argparse
import time
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

ZIP_IGNORE_FILES = ['.DS_Store']


def gen_password(length):
    return ''.join(random.choices(string.ascii_uppercase + string.ascii_lowercase + string.digits,
                                  k=length))


def gen_env_file(env_path):
    with open(env_path, 'w', encoding='utf-8') as f:
        f.write(
            f'GMS_MIRO_DATABASE_PWD={gen_password(40)}\nGMS_MIRO_ENGINE_ANONYMOUS_USER=miro_server_anonymous\nGMS_MIRO_ENGINE_ANONYMOUS_PWD={gen_password(40)}\n')


DOCKERHUB_IMAGE_CONFIG = {
    'miro-ui': {'short_desc': 'GAMS MIRO Server UI',
                'readme_loc': f'{os.getcwd()}/image-docs/miro-ui'},
    'miro-admin': {'short_desc': 'GAMS MIRO Server Admin Panel',
                   'readme_loc': f'{os.getcwd()}/image-docs/miro-admin'},
    'miro-auth': {'short_desc': 'GAMS MIRO Server Authentication Module',
                  'readme_loc': f'{os.getcwd()}/image-docs/miro-auth'},
    'miro-sproxy': {'short_desc': 'GAMS MIRO Server Docker Socket Proxy',
                    'readme_loc': f'{os.getcwd()}/image-docs/miro-sproxy'},
    'miro-proxy': {'short_desc': 'GAMS MIRO Server Container Proxy',
                   'readme_loc': f'{os.getcwd()}/image-docs/miro-proxy'}
}


class MiroServer(object):
    def __init__(self):
        parser = argparse.ArgumentParser(prog='miro_server.py',
                                         usage='miro_server [-h] {build,up,down,scan,download,push,update_readmes,release,dump_schema,fix_coverage_paths} [<args>]',
                                         description='GAMS MIRO Server build script')

        # Add the arguments
        parser.add_argument('command',
                            type=str,
                            help='Subcommand to run',
                            choices=['build', 'up', 'down', 'scan', 'download', 'push', 'update_readmes', 'release', 'dump_schema', 'fix_coverage_paths'])

        args = parser.parse_args(sys.argv[1:2])

        with open(os.path.join('..', 'build-config.json'), 'r', encoding='utf-8') as f:
            build_config = json.loads(f.read())

        self.__compose_env = os.environ.copy()
        self.__compose_env['COMPOSE_PROJECT_NAME'] = 'miro_server'
        self.__compose_env['COMPOSE_IGNORE_ORPHANS'] = 'True'
        self.__compose_env['R_BASE_VERSION'] = build_config['rVersion'].strip()
        self.__compose_env['OPENSSL_VERSION'] = build_config['opensslVersion'].strip(
        )
        self.__compose_env['CURL_VERSION'] = build_config['curlVersion'].strip()
        self.__compose_env['PQ_VERSION'] = build_config['pqVersion'].strip()

        getattr(self, args.command)()

    def build(self):
        images_to_build = ['dockerproxy', 'proxy',
                           'ui', 'admin', 'auth', 'auth_test']
        parser = argparse.ArgumentParser(
            description='Builds GAMS MIRO Server from source')
        parser.add_argument('--module',
                            type=str,
                            help='Module to build',
                            choices=images_to_build)

        parser.add_argument('--no-prep', help='Skips downloading required R packages (e.g. because they are already downloaded)',
                            action='store_true')

        args = parser.parse_args(sys.argv[2:])

        if not os.path.isfile('.env'):
            gen_env_file('.env')

        if args.module is None:
            if not args.no_prep:
                subprocess.check_call(['yarn', 'docker-prepare'], cwd='..')
            for image_to_build in images_to_build:
                if image_to_build == 'auth_test':
                    continue
                subprocess.check_call(
                    ['docker', 'compose', 'build', image_to_build], env=self.__compose_env)
            subprocess.check_call(
                ['docker', 'compose', '-f', 'docker-compose.test.yml', 'build', 'auth_test'], env=self.__compose_env)
        else:
            if args.module == 'ui' and not args.no_prep:
                subprocess.check_call(['yarn', 'docker-prepare'], cwd='..')
            if args.module == 'auth_test':
                subprocess.check_call(
                    ['docker', 'compose', '-f', 'docker-compose.test.yml', 'build', 'auth_test'], env=self.__compose_env)
            else:
                subprocess.check_call(
                    ['docker', 'compose', 'build', args.module], env=self.__compose_env)

    def up(self):
        parser = argparse.ArgumentParser(
            description='Starts GAMS MIRO Server')

        subprocess.check_call(
            ['docker', 'compose', 'up', '-d'], env=self.__compose_env)

    def down(self):
        parser = argparse.ArgumentParser(
            description='Stops GAMS MIRO Server')
        parser.add_argument('-v', '--volumes', help='Removes volumes and networks',
                            action='store_true')

        args = parser.parse_args(sys.argv[2:])

        dc_args_miro = ['docker', 'compose', 'down']

        self.stop_proxies('registry.gams.com/miro/miro/miro-admin')
        self.stop_proxies('registry.gams.com/miro/miro/miro-ui')
        self.stop_proxies('gams/miro-admin')
        self.stop_proxies('gams/miro-ui')
        self.stop_proxies('miro-admin')
        self.stop_proxies('miro-ui')

        if args.volumes:
            dc_args_miro.append('-v')

        subprocess.check_call(dc_args_miro, env=self.__compose_env)

    def scan(self):
        for image_name in DOCKERHUB_IMAGE_CONFIG:
            print(f"Scanning image: {image_name}:latest")
            subprocess.check_call(['docker', 'scan', f'{image_name}:latest'])

    def download(self):
        parser = argparse.ArgumentParser(
            description='Downloads GAMS MIRO Server Docker images')
        parser.add_argument('registry', type=str,
                            help='Registry to download images from')
        parser.add_argument('--image-tag', type=str, default='latest',
                            help='The tag of the images')
        parser.add_argument('--tag-as-released', action='store_true',
                            help='Tag images as if they were released ("gams/" prefix)')

        args = parser.parse_args(sys.argv[2:])

        for image in [('miro-sproxy', 'miro-sproxy'),
                      ('miro-proxy', 'miro-proxy'),
                      ('miro-auth', 'miro-auth'),
                      ('miro-admin', 'miro-admin'),
                      ('miro-ui', 'miro-ui')]:
            self.download_image(*image, image_tag=args.image_tag,
                                image_server=args.registry,
                                tag_as_released=args.tag_as_released)

    def push(self):
        parser = argparse.ArgumentParser(
            description='Publishes GAMS MIRO Server Docker images')
        parser.add_argument('--unstable',
                            help='Unstable build',
                            action='store_true')
        parser.add_argument('--registry',
                            help='Container registry host')
        parser.add_argument('--custom-tag',
                            help='Custom image tag')

        args = parser.parse_args(sys.argv[2:])

        registry_host = args.registry

        if registry_host is None:
            if args.unstable:
                raise ValueError(
                    'registry_host must not be None if --unstable argument is set')
            registry_host = 'gams'

        for image in [('miro-sproxy', 'miro-sproxy'),
                      ('miro-proxy', 'miro-proxy'),
                      ('miro-auth', 'miro-auth'),
                      ('miro-admin', 'miro-admin'),
                      ('miro-ui', 'miro-ui')]:
            self.push_image(registry_host, *image, unstable=args.unstable,
                            custom_tag=args.custom_tag)

        if args.unstable:
            self.push_image(registry_host, 'miro-auth-test', 'miro-auth-test',
                            unstable=args.unstable)

    def update_readmes(self):
        parser = argparse.ArgumentParser(
            description='Appends specified version to README files of GAMS MIRO Server images')
        parser.add_argument('version', type=str,
                            help='MIRO Server version to append')

        args = parser.parse_args(sys.argv[2:])

        for image_name in DOCKERHUB_IMAGE_CONFIG:
            self.append_tag_readme(os.path.join(DOCKERHUB_IMAGE_CONFIG[image_name]['readme_loc'], 'README.md'),
                                   args.version)

    def release(self):
        parser = argparse.ArgumentParser(
            description='Releases GAMS MIRO Server')

        parser.add_argument('-f', '--force', help='Overwrite release if it exists',
                            action='store_true')
        parser.add_argument('--k8s', help='Release k8s version (helm chart)',
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

        python_binary = 'python3'

        if platform.system() == 'Windows':
            python_binary = 'python'

        if args.k8s:
            shutil.copytree('kubernetes/gams-miro-server',
                            'release/gams-miro-server')
        else:
            shutil.copytree('release_data', 'release')

        shutil.copy('LICENSE', os.path.join('release', 'LICENSE'))

        release_zip_file = zipfile.ZipFile(
            release_zip_filename, 'w', zipfile.ZIP_DEFLATED)

        for root, dirs, files in os.walk('release'):
            for file in files:
                if file in ZIP_IGNORE_FILES:
                    continue

                name_in_zip = os.path.join('miro_server',
                                           os.path.relpath(os.path.join(root, file), 'release'))
                if os.name == 'nt' and file == 'miro-server':
                    # we have to manually set executable bit of miro-server on Windows
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
        print(
            f"GAMS MIRO Server release file was written to: {os.path.join(os.getcwd(), release_zip_filename)}")

    def stop_proxies(self, image_name):
        active_admin_containers = subprocess.run(['docker', 'container', 'ls', '-f', 'network=miroserver-network', '-f',
                                                  f'ancestor={image_name}', '--format', '{{.ID}}'], capture_output=True).stdout.strip()

        if not active_admin_containers:
            return

        active_admin_containers = active_admin_containers.decode(
            "utf-8").split('\n')

        if len(active_admin_containers) > 0:
            subprocess.check_call(
                [*['docker', 'stop'], *active_admin_containers])
            subprocess.check_call(
                [*['docker', 'rm'], *active_admin_containers])

    def append_tag_readme(self, file_name, tag):
        with open(file_name, 'r', encoding='utf-8') as f:
            content = f.readlines()
            for i, line in enumerate(content):
                if '[`latest`' in line and not f'[`latest`, `{tag}`' in line:
                    index = line.index('[') + 9
                    content[i] = content[i][0:index] + \
                        f', `{tag}`' + content[i][index:]

        with open(file_name, 'w', encoding='utf-8') as f:
            f.writelines(content)

    def download_image(self, image_name_local, image_name_hub,
                       image_tag, image_server, tag_as_released=False):
        '''Downloads specified GAMS MIRO Server image from specified server'''
        subprocess.check_call(
            ['docker', 'pull', f'{image_server}/{image_name_hub}:{image_tag}'])
        if tag_as_released:
            local_image_tag = f"gams/{image_name_local}"
        else:
            local_image_tag = image_name_local
        subprocess.check_call(
            ['docker', 'tag', f'{image_server}/{image_name_hub}:{image_tag}', local_image_tag])

    def push_image(self, registry_host, image_name_local, image_name_hub, unstable=False, custom_tag=None):
        if unstable:
            if custom_tag:
                version_string = custom_tag
            else:
                version_string = 'unstable'
        else:
            with open(os.path.join('..', 'package.json'), 'r', encoding='utf-8') as f:
                version_string = json.loads(f.read())['version'].strip()

        if not unstable:
            subprocess.check_call(
                ['docker', 'tag', image_name_local, f'{registry_host}/{image_name_hub}'])

        subprocess.check_call(
            ['docker', 'tag', image_name_local, f'{registry_host}/{image_name_hub}:{version_string}'])
        if not unstable:
            subprocess.check_call(
                ['docker', 'push', f'{registry_host}/{image_name_hub}'])

        subprocess.check_call(
            ['docker', 'push', f'{registry_host}/{image_name_hub}:{version_string}'])

        # publish README
        if (not unstable and image_name_hub in DOCKERHUB_IMAGE_CONFIG and
                os.path.isfile(os.path.join(DOCKERHUB_IMAGE_CONFIG[image_name_hub]['readme_loc'], 'README.md'))):
            self.append_tag_readme(os.path.join(DOCKERHUB_IMAGE_CONFIG[image_name_hub]['readme_loc'], 'README.md'),
                                   version_string)
            subprocess.check_call(['docker',
                                   'run',
                                   '--rm',
                                   '-v',
                                   f'{DOCKERHUB_IMAGE_CONFIG[image_name_hub]["readme_loc"]}:/myvol',
                                   '-e',
                                   f'DOCKER_USER={os.getenv("DOCKERHUB_USER")}',
                                   '-e',
                                   f'DOCKER_PASS={os.getenv("DOCKERHUB_PASS")}',
                                   'chko/docker-pushrm:1',
                                   '--file',
                                   'myvol/README.md',
                                   '--short',
                                   f'gams/{DOCKERHUB_IMAGE_CONFIG[image_name_hub]["short_desc"]}',
                                   '--debug',
                                   f'gams/{image_name_hub}'])

    def dump_schema(self):
        parser = argparse.ArgumentParser(
            description='Dumps MIRO Server OpenAPI schema')

        parser.add_argument('path', help='Path where to dump schema')

        args = parser.parse_args(sys.argv[2:])

        dump_result = subprocess.run(
            [
                'docker',
                'compose',
                'run',
                '--no-deps',
                '--rm',
                '-v',
                f'{os.getcwd()}/auth/utils/dump_api.py:/app/dump_api.py',
                'auth',
                'python3',
                '/app/dump_api.py'
            ],
            check=True, stdout=subprocess.PIPE
        )

        with open(args.path, 'w', newline='\n', encoding='utf-8') as oai_schema:
            parsed = json.loads(dump_result.stdout.decode())
            json.dump(parsed, oai_schema, indent=4)
            oai_schema.write('\n')

    def fix_coverage_paths(self):
        parser = argparse.ArgumentParser(
            description='Make absolute paths in cobertura XML code coverage report relative')

        parser.add_argument('path', help='Filepath to coverage report file')
        parser.add_argument('-b', '--base-dir', help='Base directory of scanned component in repository',
                            default='')

        args = parser.parse_args(sys.argv[2:])
        with open(args.path, 'r', encoding='utf-8') as f:
            content = f.readlines()

        for idx, line in enumerate(content):
            if 'filename="' in line:
                content[idx] = content[idx].replace('filename="', f'filename="{args.base_dir}')

        with open(args.path, 'w', encoding='utf-8') as f:
            f.writelines(content)


if __name__ == '__main__':
    MiroServer()
