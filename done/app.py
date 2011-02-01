#!/usr/bin/env python
from tofuroll import *

class app(TofuApp):
    @option
    def name(self):
        return {
            'name' : 'name',
            'help' : 'A name'
        }

    @command
    def hello(self, options, *args):
        name = options.name or "there"
        print "Hi %s" % name

if __name__ == '__main__':
    app().run()
