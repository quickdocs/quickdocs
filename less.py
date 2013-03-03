#!/usr/bin/env python
import os
import re
import shutil
import sys
import time
import glob

from watchdog.observers import Observer
from watchdog.tricks import Trick


class FileCompileHandler(Trick):

    def output_path(self, path):
        return re.sub(r"([^\.]+)(\..+?)?$", r"\1-compiled\2", path)

    def compile_file(self, file):
        pass

    def compile_path(self, path):
        if os.path.isdir(path):
            for file in os.listdir(path):
                self.compile_path(file)
        else:
            self.compile_file(path)

    def compile(self, *files):
        for file in files:
            self.compile_path(file)

    def on_created(self, event):
        if not event.is_directory:
            self.compile(event.src_path)
        else:
            os.mkdir(self.output_path(event.src_path))

    def on_modified(self, event):
        if not event.is_directory:
            self.compile(event.src_path)

    def on_moved(self, event):
        self.on_deleted(event)
        self.compile(event.dest_path)

    def on_deleted(self, event):
        to_delete_path = self.output_path(event.src_path)
        if event.is_directory:
            shutil.rmtree(to_delete_path)
        else:
            os.remove(to_delete_path)


class LESSCompileHandler(FileCompileHandler):

    def __init__(self,
                 patterns=['*.less'],
                 source_directory='.',
                 destination_directory=None,
                 import_directory='lib',
                 tmp_directory='tmp/less',
                 lessc='lessc'):
        super(LESSCompileHandler, self).__init__(patterns=patterns)

        self.source_directory = os.path.abspath(source_directory)
        if destination_directory is None:
            self.destination_directory = self.source_directory
        else:
            self.destination_directory = os.path.abspath(destination_directory)

        if import_directory is not None:
            self.import_directory = os.path.join(self.source_directory,
                                                 import_directory)

        self.lessc = lessc

        self.tmp_directory = os.path.abspath(tmp_directory)
        if os.path.exists(self.tmp_directory):
            shutil.rmtree(self.tmp_directory)

    def output_path(self, path):
        return re.sub(
            r"\.less$", '.css',
            path.replace(self.source_directory, self.destination_directory))

    def compile(self, *files):
        for file in self.resolve(*files):
            self.compile_path(file)

    def compile_file(self, file):
        output_file = self.output_path(file)

        args = ['node', self.lessc,
                ('-I' + self.import_directory)]

        depsfile = self.depspath(file)
        if not os.path.exists(os.path.dirname(depsfile)):
            os.makedirs(os.path.dirname(depsfile))

        open(depsfile, 'w').close()
        args.append('-MF=' + depsfile)

        args.extend([file, output_file])

        os.system(' '.join(args))
        print '[LESS] Compiled: %s -> %s' % (file, output_file)

    def depspath(self, path):
        tmpdir = path.replace(self.source_directory,
                              self.tmp_directory)
        return tmpdir if os.path.isdir(path) else tmpdir + '.dep'

    def resolve(self, *files):
        dependencies = []
        for depfile in glob.glob(self.tmp_directory + '/*.dep'):
            f = open(depfile)
            for line in f:
                line = line.rstrip()
                [filename, deps] = re.split(r"\s*:\s*", line)
                if next((d for d in re.split(r"\s+", deps)
                         if d in files), None):
                    dependencies.append(filename)

        return list(files) + list(set(dependencies))

    def on_deleted(self, event):
        super(LESSCompileHandler, self).on_deleted(event)
        depspath = self.depspath(event.src_path)
        if event.is_directory:
            shutil.rmtree(depspath)
        else:
            os.remove(depspath)


def watch_and_compile(source_directory="static/less",
                      destination_directory="static/css"):
    event_handler = LESSCompileHandler(
        source_directory=source_directory,
        destination_directory=destination_directory,
        lessc='modules/less.js/bin/lessc')

    for file in glob.glob(source_directory + "/**/*.less") + \
            glob.glob(source_directory + "/*.less"):
        event_handler.compile_file(os.path.abspath(file))

    observer = Observer()
    observer.schedule(
        event_handler,
        path=event_handler.source_directory,
        recursive=True)
    observer.start()
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        observer.stop()
    observer.join()


if __name__ == "__main__":
    watch_and_compile(*sys.argv[1:])
