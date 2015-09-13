# -*- coding: utf-8 -*-
"""
Scholdoc Reader
===============
This plugin allows you to use Scholarly Markdown to write your posts. 
File extension should be ``.smkd`` or ``.scholdoc``.
"""

import subprocess
from pelican import signals
from pelican.readers import BaseReader
from pelican.utils import pelican_open


class ScholdocReader(BaseReader):
    """Reader for Scholarly Markdown files."""

    enabled = True
    file_extensions = ['smkd', 'scholdoc', 'scholmd']

    def extract_metadata(self, text):
        metadata_text, content_text = self.split_metadata(text)
        metadata_lines = metadata_text.splitlines()
        metadata = {}
        for line in metadata_lines:
            if ':' in line:
                key, value = line.split(':', 1)
                name = key.lower().strip()
                metadata[name] = self.process_metadata(name, value.strip())
        return metadata, content_text

    def split_metadata(self, text):
        try:
            delimiter = '---'
            start = text.index(delimiter) + len(delimiter)
            end = text.index(delimiter, start)
            return text[start:end], text[end:]
        except ValueError:
            raise MetadataException("Can't find metadata")


    def read(self, source_path):
        with pelican_open(source_path) as source:
            text = source

        metadata, content = self.extract_metadata(text)
        proc = subprocess.Popen(
            ["scholdoc"],
            stdin = subprocess.PIPE,
            stdout = subprocess.PIPE
        )

        output = proc.communicate(content.encode('utf-8'))[0].decode('utf-8')
        status = proc.wait()
        if status:
            raise subprocess.CalledProcessError(status, scholdoc_cmd)

        return output, metadata

def add_reader(readers):
    for ext in ScholdocReader.file_extensions:
        readers.reader_classes[ext] = ScholdocReader

def register():
    signals.readers_init.connect(add_reader)