#!/usr/bin/env python
# experimental new version

import sys

import peewee
from peewee import database

class Task:
    desc      = peewee.CharField()
    due_date  = peewee.DateTimeField()
    done      = peewee.BooleanField()

    def __unicode__(self):
        ustr = u'desc'
        if self.due_date:
            ustr += ( u' (due: %s)' % self.due_date)

        return ustr

class Commands:
    def __init__(self):
        op = OptionParser(usage='usage: %prog [la] arg')

    def run(self):
        (options, args) = self.op.parse_args()

        if len(args) == 0:
            self.op.print_help()
            return 1

        try:
            command = self.match_command(args.pop())
            getattr(self, command)()
        except:
            self.op.print_help()
            return 1

    def match_command(self):
        funs = [fname for fname in dir(self) if fname.startswith('command_')]
        # XXX

    def command(fun):
        fun.__name__ = "command_%s" % fun.__name__
        return fun

    @property
    def opt_parser(self):
        return self.op

    @command
    def add(self, desc, due_date=None):
        t = Task(desc=desc, due_date=due_date)
        t.save()
        print "Added task: %s" % t

    @command
    def list(self):
        tasks = Task.select().where(done=False).order_by('due_date')
        for t in tasks:
            print t

def main():
    database.connect()
    Task.create_table()

    return Commands().run()

if __name__ == '__main__':
    sys.exit(main())


