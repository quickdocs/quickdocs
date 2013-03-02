#!/usr/bin/env python
import os
import re
import shutil
import sys
import time

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

    def on_created(self, event):
        if not event.is_directory:
            self.compile_path(event.src_path)
        else:
            os.mkdir(self.output_path(event.src_path))

    def on_modified(self, event):
        if not event.is_directory:
            self.compile_path(event.src_path)

    def on_moved(self, event):
        self.on_deleted(event)
        self.compile_path(event.dest_path)

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
                 lessc='lessc'):
        super(LESSCompileHandler, self).__init__(patterns=patterns)

        self.source_directory = os.path.abspath(source_directory)
        if destination_directory is not None:
            self.destination_directory = os.path.abspath(destination_directory)
        else:
            self.destination_directory = self.source_directory

        self.lessc = lessc

    def output_path(self, path):
        return re.sub(
            r"\.less$", '.css',
            path.replace(self.source_directory, self.destination_directory))

    def compile_file(self, file):
        output_file = self.output_path(file)
        os.system(' '.join([self.lessc, file, output_file]))
        print '[LESS] Compiled: %s -> %s' % (file, output_file)


def watch_and_compile(source_directory="static/less",
                      destination_directory="static/css"):
    event_handler = LESSCompileHandler(
        source_directory=source_directory,
        destination_directory=destination_directory)

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
