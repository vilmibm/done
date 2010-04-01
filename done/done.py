#!/usr/bin/python

# who  nate smith
# when march 2010
# why  the done tool
# where midwest usa

import sys
from optparse import OptionParser

import Commands

def main():
    op = OptionParser(usage="usage: %prog [options] ladb [arg1...]")
    op.add_option("-d", "--due", dest="due",  help="specify a due date")
    op.add_option("-s", "--sort",dest="sort", default="due", help="sort by created, alpha, pri, due date")

    (options, args) = op.parse_args()

    if len(args) == 0:
        return handle_error("Must supply a command", op)

    command = args.pop(0)
    
    try:
        command = Commands.match(command, options, args)
    except Commands.ArgError as e:
        return handle_error(e.value, op)

    Commands.run(command, options, args)

    return 0

def handle_error(error, op):
    print error
    op.print_help()
    return 1

if __name__ == '__main__':
    sys.exit(main())
