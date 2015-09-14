#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = u'Rafael S. Calsaverini'
SITENAME = u'Entropy maximizer'
SITEURL = ''
SITESUBTITLE = u"Trapped in a local maximum."
SITEURL = u'http://rcalsaverini.github.io'
RELATIVE_URLS = True
SHOW_ARTICLE_AUTHOR = True

PATH = 'content'
TIMEZONE = 'America/Sao_Paulo'
DEFAULT_LANG = u'en'

# Feed generation is usually not desired when developing

FEED_RSS = "rss.xml"
FEED_ATOM = "atom.xml"
DISPLAY_PAGES_ON_MENU = True

COLOPHON = True
COLOPHON_TITLE = u"Trapped in a local maximum"
COLOPHON_CONTENT = u"Bayesian hierarchical modeling is a statistical model written in multiple levels (hierarchical form) that estimates the parameters of the posterior distribution using the Bayesian method.[1] The sub-models combine to form the hierarchical model, and the Bayes’ theorem is used to integrate them with the observed data, and account."

GITHUB_ADDRESS = "http://github.com/rcalsaverini"
TWITTER_ADDRESS = "http://twitter.com/rcalsaverini"
FB_ADDRESS = "https://www.facebook.com/rafael.calsaverini/"

LICENSE_URL = "https://creativecommons.org/licenses/by-sa/3.0/"
LICENSE_NAME = "Creative Commons BY-SA 3.0"
DEFAULT_PAGINATION = 2

# Uncomment following line if you want document-relative URLs when developing
RELATIVE_URLS = True
PLUGIN_PATHS = ["plugins", "./plugins"]
PLUGINS = ['madoko']
DISQUS_SITENAME = "entropic"

