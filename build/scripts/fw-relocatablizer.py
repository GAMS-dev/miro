# encoding: utf-8
#
# Copyright 2018 Greg Neagle.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""Functions to make the Python framework relocatable"""

from __future__ import print_function

import os
import subprocess
import sys

CHMOD = "/bin/chmod"
OTOOL = "/usr/bin/otool"
INSTALL_NAME_TOOL = "/usr/bin/install_name_tool"
FILETOOL = "/usr/bin/file"


def run(cmd):
    """Prints and executes cmd"""
    print(" ".join(cmd))
    subprocess.check_call(cmd)


def fix_modes(framework_dir):
    """Make sure all files are set so owner can read/write and everyone else
       can only read"""
    cmd = [CHMOD, "-R", "u+rw,g+r,g-w,o+r,o-w", framework_dir]
    print("Ensuring correct modes for files in %s..." % framework_dir)
    subprocess.check_call(cmd)


def framework_dir(some_file):
    """Return parent path to framework dir"""
    temp_path = some_file
    while len(temp_path) > 1:
        if temp_path.endswith("/r"):
            return temp_path
        temp_path = os.path.dirname(temp_path)
    return ""


def framework_name(some_file):
    """Return framework name"""
    temp_path = some_file
    while len(temp_path) > 1:
        if temp_path.endswith(".framework"):
            return os.path.basename(temp_path)
        temp_path = os.path.dirname(temp_path)
    return ""


def framework_lib_name(some_file):
    """Return framework lib name"""
    return os.path.splitext(framework_name(some_file))[0]


def relativize_install_name(some_file):
    """Replaces original install name with an rpath; returns new
    install_name"""
    original_install_name = get_install_name(some_file)
    if original_install_name and not original_install_name.startswith("@"):
        framework_loc = framework_dir(some_file)
        new_install_name = os.path.join(
            "@rpath", os.path.relpath(some_file, framework_loc)
        )
        cmd = [INSTALL_NAME_TOOL, "-id", new_install_name, some_file]
        run(cmd)
        return new_install_name
    return original_install_name


def fix_dep(some_file, old_install_name, new_install_name):
    """Updates old_install_name to new_install_name inside some file"""
    cmd = [
        INSTALL_NAME_TOOL,
        "-change",
        old_install_name,
        new_install_name,
        some_file,
    ]
    run(cmd)


def get_rpaths(some_file):
    """returns rpaths stored in an executable"""
    cmd = [OTOOL, "-l", some_file]
    output_lines = subprocess.check_output(cmd).decode("utf-8").splitlines()
    rpaths = []
    for (index, line) in enumerate(output_lines):
        if "cmd LC_RPATH" in line and index + 2 <= len(output_lines):
            rpath_line = output_lines[index + 2]
            rpath_line = rpath_line.lstrip()
            if rpath_line.startswith("path "):
                rpath_line = rpath_line[5:]
            tail = rpath_line.find(" (offset ")
            if tail != -1:
                rpath_line = rpath_line[0:tail]
            rpaths.append(rpath_line)
    return rpaths


def add_rpath(some_file):
    """adds an rpath to the file"""
    framework_loc = framework_dir(some_file)
    rpath = (
        os.path.join(
            "@executable_path",
            os.path.relpath(framework_loc, os.path.dirname(some_file)),
        )
        + "/"
    )
    if rpath not in get_rpaths(some_file):
        cmd = [INSTALL_NAME_TOOL, "-add_rpath", rpath, some_file]
        run(cmd)


def get_deps(some_file):
    """Return a list of dependencies for some_file"""
    cmd = [OTOOL, "-L", some_file]
    output_lines = subprocess.check_output(cmd).decode("utf-8").splitlines()
    deps = []
    if len(output_lines) > 1:
        for line in output_lines[1:]:
            line = line.lstrip()
            tail = line.find(" (compatibility")
            if tail != -1:
                line = line[0:tail]
            deps.append(line)
    return deps


def get_install_name(some_file):
    """Returns the install_name of a shared library"""
    cmd = [OTOOL, "-D", some_file]
    output_lines = subprocess.check_output(cmd).decode("utf-8").splitlines()
    if len(output_lines) > 1:
        return output_lines[1]
    return ""


def make_info(some_file):
    """Return a dict containing info about the file"""
    info = {}
    info["path"] = some_file
    install_name = get_install_name(some_file)
    if install_name:
        info["install_name"] = install_name
        info["dependencies"] = get_deps(some_file)[1:]
    else:
        info["dependencies"] = get_deps(some_file)
    return info


def deps_contain_prefix(info_item, prefix):
    """Do the deps or install_name contain the prefix?"""
    matching_dep_items = (
        len(
            [
                dep_item
                for dep_item in info_item.get("dependencies", [])
                if dep_item.startswith(prefix)
            ]
        )
        > 0
    )
    matching_install_name = info_item.get("install_name", "").startswith(prefix)
    return matching_dep_items or matching_install_name


def base_install_name(full_framework_path):
    """Generates a base install name for the framework"""
    dylib_name = os.path.join(full_framework_path, "R")
    if os.path.exists(dylib_name):
        install_name = get_install_name(dylib_name)
        if not install_name.startswith("@"):
            return framework_dir(install_name)
    return ""


def analyze(some_dir):
    """Finds files we need to tweak"""
    print("Analyzing %s..." % some_dir)
    prefix = base_install_name(some_dir)
    data = {}
    data["executables"] = []
    data["dylibs"] = []
    data["so_files"] = []
    count = 0
    for dirpath, _dirs, files in os.walk(some_dir):
        for some_file in files:
            count += 1
            if count % 100 == 0:
                sys.stdout.write(".")
                sys.stdout.flush()
            filepath = os.path.join(dirpath, some_file)
            if os.path.islink(filepath):
                continue
            ext = os.path.splitext(filepath)[1]
            if ext == ".so":
                info = make_info(filepath)
                if deps_contain_prefix(info, prefix):
                    data["so_files"].append(info)
            elif ext == ".dylib":
                info = make_info(filepath)
                if deps_contain_prefix(info, prefix):
                    data["dylibs"].append(info)
            else:
                cmd = [FILETOOL, "-b", filepath]
                try:
                    output = subprocess.check_output(cmd).decode("utf-8")
                except:
                    continue

                if "Mach-O 64-bit executable" in output:
                    info = make_info(filepath)
                    if deps_contain_prefix(info, prefix):
                        data["executables"].append(info)
                if "Mach-O 64-bit dynamically linked shared library" in output:
                    info = make_info(filepath)
                    if deps_contain_prefix(info, prefix):
                        data["dylibs"].append(info)
    sys.stdout.write("\n")
    return data


def relocatablize(framework_path):
    """Changes install names and rpaths inside a (Python) framework to make
    it relocatable. Might work with non-Python frameworks..."""
    full_framework_path = os.path.abspath(
        os.path.normpath(os.path.expanduser(framework_path))
    )
    fix_modes(full_framework_path)
    framework_data = analyze(full_framework_path)
    for dylib in framework_data["dylibs"]:
        old_install_name = dylib["install_name"]
        new_install_name = relativize_install_name(dylib["path"])
        # update other files with new install_name
        if old_install_name != new_install_name:
            files = (
                framework_data["executables"]
                + framework_data["dylibs"]
                + framework_data["so_files"]
            )
            for item in files:
                if old_install_name in item["dependencies"]:
                    fix_dep(item["path"], old_install_name, new_install_name)
        print()
    # add rpaths to executables
    for item in framework_data["executables"]:
        add_rpath(item["path"])



def make_r_portable(exec_path):
    # change fixed paths to make R portable
    # taken from: https://github.com/chasemc/electricShine
    # Copyright (c) 2019 Chase Clark
    #
    # Permission is hereby granted, free of charge, to any person obtaining a copy 
    # of this software and associated documentation files (the "Software"), to deal 
    # in the Software without restriction, including without limitation the rights 
    # to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
    # of the Software, and to permit persons to whom the Software is furnished to do so, 
    # subject to the following conditions:
    #
    # The above copyright notice and this permission notice shall be included in all 
    # copies or substantial portions of the Software.
    #
    # THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
    # INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
    # PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE 
    # FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR 
    # OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
    # DEALINGS IN THE SOFTWARE.
    modification = r'''#!/bin/sh
# Shell wrapper for R executable.
export R_HOME="${R_HOME_DIR}"
#R_SHARE_DIR=/Library/Frameworks/R.framework/Resources/share
export R_SHARE_DIR
#R_INCLUDE_DIR=/Library/Frameworks/R.framework/Resources/include
export R_INCLUDE_DIR
#R_DOC_DIR=/Library/Frameworks/R.framework/Resources/doc
R_DOC_DIR=${R_HOME_DIR}/doc
export R_DOC_DIR
# Since this script can be called recursively, we allow R_ARCH to
# be overridden from the environment.
# This script is shared by parallel installs, so nothing in it should
# depend on the sub-architecture except the default here.
: ${R_ARCH=""}

'''

    with open(exec_path, 'r') as f:
        executable_contents = f.readlines()

    executable_contents = executable_contents[[i for (i,line) in enumerate(executable_contents) if "usage=" in line][0]:]

    with open(exec_path, 'w') as f:
        f.write(modification + ''.join(executable_contents))


def copy_and_overwrite(from_path, to_path):
        if os.path.exists(to_path):
            shutil.rmtree(to_path)
        shutil.copytree(from_path, to_path)

if __name__ == "__main__":
    import os
    import sys
    import shutil

    if len(sys.argv) < 2:
        print("Please provide path to R framework as first argument")
        sys.exit(1)

    framework_path = sys.argv[1]
    exec_path = os.path.join(framework_path, 'R')

    # workaround as fixing rpaths somehow modifies original files as well, which leads to failing tests
    copy_and_overwrite(os.path.join('.', 'r', 'library'), os.path.join('.', 'library'))

    relocatablize(framework_path)
    make_r_portable(exec_path)


