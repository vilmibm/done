from tofuroll import *

class app(TofuApp):
    @command
    def hello(self, options, *args):
        print options
        print args

if __name__ == '__main__':
    app().run()
