#!/usr/bin/python

# who  nate smith
# when march 2010
# why  the done tool
# where midwest usa

import os
import sys
import sqlite3
from optparse import OptionParser

import Config
import Commands

def main():
    maybe_init_db()
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

def maybe_init_db():
    db_path = Config.db_path
    if not os.path.isfile(db_path):
        db = sqlite3.connect(db_path)
        c  = db.cursor()

        c.execute(" \
            CREATE TABLE tasks ( \
                id integer primary key, \
                desc text, \
                due integer, \
                created itneger, \
                done boolean default 0 \
            ) \
        ")

if __name__ == '__main__':
    sys.exit(main())
