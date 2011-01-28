from optparse import OptionParser
import types
import sys

def command(fun):
    fun.__name__ = "command_%s" % fun.__name__
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