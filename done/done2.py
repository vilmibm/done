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
            self.match_command(args.pop(0))()
        except Exception, e:
            print "Exception: %s" % e
            self.op.print_help()
            return 1

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

    # decorator
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
