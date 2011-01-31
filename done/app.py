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
#        name = options.name or "there"
#        print "Hi %s" % name
        print "hi there"
        #print options
        #print args

if __name__ == '__main__':
    app().run()
