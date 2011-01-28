#!/usr/bin/env python
# experimental new version

import sys
import types

from optparse import OptionParser
import peewee
from peewee import database
DATABASE = 'done2.db'
database = peewee.Database(DATABASE)

class Task(peewee.Model):
    desc      = peewee.CharField()
    due_date  = peewee.DateTimeField()
    done      = peewee.BooleanField()

    class Meta:
        database = database

    def __unicode__(self):
        ustr = u'desc'
        if self.due_date:
            ustr += ( u' (due: %s)' % self.due_date)

        return ustr

class Commands:
    def __init__(self):
        self.op = OptionParser(usage='usage: %prog [la] arg')

    def run(self):
        (options, args) = self.op.parse_args()

        if len(args) == 0:
            self.op.print_help()
            return 1

        try:
            command = self.match_command(args.pop())
            getattr(self, command)()
        except Exception, e:
            print "Exception: %s" % e
            self.op.print_help()
            return 1

    def match_command(self, command):
        funs = [getattr(self,f) for f in dir(self) if type(getattr(self,f)) == types.MethodType]
        funs = [fname.__name__ for fname in funs if fname.__name__.startswith('command_')]
        build = "command_"
        possible = funs
        for letter in command:
            build += letter
            possible = filter(lambda w: w.startswith(build), possible)

        if len(possible) == 0:
            raise ValueError("Command invalid %s" % command)

        if len(possible) > 1:
            raise ValueError("Ambiguous command: %s" % command)

        return possible.pop().split('command_')[1]

    # filter
    def command(fun):
        fun.__name__ = "command_%s" % fun.__name__
        return fun

    @property
    def opt_parser(self):
        return self.op

    @command
    def add(self, desc, due_date=None):
        #t = Task(desc=desc, due_date=due_date)
        #t.save()
        #print "Added task: %s" % t
        print "add"

    @command
    def list(self):
        #tasks = Task.select().where(done=False).order_by('due_date')
        #for t in tasks:
        #    print t
        print "list"

def main():
    database.connect()
    Task.create_table()

    return Commands().run()

if __name__ == '__main__':
    sys.exit(main())
