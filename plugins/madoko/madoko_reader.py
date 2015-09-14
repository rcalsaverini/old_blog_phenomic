# -*- coding: utf-8 -*-
"""
Madoko Reader
===============
This plugin allows you to use Madoko to write your posts. 
File extension should be ``.mdk`` or ``.madoko``.
"""

import bond
from pelican import signals
from pelican.readers import BaseReader
from pelican.utils import pelican_open

js = bond.make_bond("JavaScript")

madoko = js.callable("""
    function(content) {
        var madoko = require('madoko');
        madoko.initialOptions0.full = false;
        return madoko.markdownNormal(content);
    }
""")


class MadokoReader(BaseReader):
    """Reader for Madoko files."""

    enabled = True
    file_extensions = ['mdk', 'madoko']

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
        output = madoko(content)
        return output, metadata

def add_reader(readers):
    for ext in MadokoReader.file_extensions:
        readers.reader_classes[ext] = MadokoReader

def register():
    signals.readers_init.connect(add_reader)


if __name__ == "__main__":
    content = (
        "#Introduction\n"
        "This is a test of running madoko in Python "
        "without any monkey business. As Phillip, the Arab "
        "once said:\n"
        "> Poor is the man that can't communicate processes.\n"
        "\n"
        "Let's just not get too excited."
    )
    print madoko(content)