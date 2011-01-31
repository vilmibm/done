from optparse import OptionParser
import types
import sys

def command(fun):
    def preload_args(self, options, *args):
        print "called"
        print dir(self)
        import pdb; pdb.set_trace()
        def member_is_option(fname):
            member = getattr(self, fname)
            if type(member) == types.MethodType:
                return member.__name__.startswith('option_')

        fnames = [member for member in dir(self) if member_is_option(member)]
        for opt_method in fnames:
            opt_method = getattr(self, opt_method)
            option     = opt_method()
            name       = option['name']
            help       = option['help']
            long_name  = "--%s" % name
            short_name = "-%s" % name[0]
            self.op.add_option(short_name, long_name, dest=name, action="store", help=help)
        print "called2"
        fun(options, *args)

    preload_args.__name__ = "command_%s" % fun.__name__
    return preload_args

def option(fun):
    fun.__name__ = "option_%s" % fun.__name__
    return fun

class TofuApp:
    def __init__(self):
        self.op = OptionParser()

    def match_command(self, command):
        def member_is_command(fname):
            member = getattr(self, fname)
            if type(member) == types.MethodType:
                return member.__name__.startswith('command_')

        fnames = [member for member in dir(self) if member_is_command(member)]
        build = ""
        possible = fnames
        for letter in command:
            build += letter
            possible = filter(lambda w: w.startswith(build), possible)

        if len(possible) == 0:
            raise ValueError("Command invalid %s" % command)

        if len(possible) > 1:
            raise ValueError("Ambiguous command: %s" % command)

        return getattr(self, possible.pop())

    def run(self):
        (options, args) = self.op.parse_args()

        if len(args) == 0:
            self.op.print_help()
            return 1

        try:
            command = self.match_command(args.pop(0))
            command(options, args)
        except Exception, e:
            print "Exception: %s" % e
            self.op.print_help()
            return 1

    def __call__(self):
        sys.exit(self.run())
