#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = u'Rafael S. Calsaverini'
SITENAME = u'Entropy maximizer'
SITEURL = ''
SITESUBTITLE = u"Trapped at a local maximum."
SITEURL = u'http://rcalsaverini.github.io'
RELATIVE_URLS = True
SHOW_ARTICLE_AUTHOR = True

PATH = 'content'

TIMEZONE = 'America/Sao_Paulo'

DEFAULT_LANG = u'en'

# Feed generation is usually not desired when developing

FEED_RSS = "atom.rss"

EMAIL_ADDRESSS = "mailto:rafael.calsaverini+pelican@gmail.com"
GITHUB_ADDRESS = "http://github.com/rcalsaverini"
SO_ADDRESS = "http://stackoverflow.com/users/114388/rafael-s-calsaverini"
TWITTER_ADDRESS = "http://twitter.com/rcalsaverini"
FB_ADDRESS = "https://www.facebook.com/rafael.calsaverini/"
PROFILE_IMAGE_URL = "https://www.gravatar.com/avatar/9ed01afca2cc6c4d7d0c4d134fe5bde4?s=328&d=identicon&r=PG"
LICENSE_URL = "https://creativecommons.org/licenses/by-sa/3.0/"
LICENSE_NAME = "Creative Commons BY-SA 3.0"
DEFAULT_PAGINATION = 10

# Uncomment following line if you want document-relative URLs when developing
RELATIVE_URLS = True
PLUGIN_PATHS = ["plugins", "./plugins"]
PLUGINS = ['scholdoc_reader']
DISQUS_SITENAME = "entropic"
