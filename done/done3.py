#!/usr/bin/env python

import sys

import peewee

from tofuroll import *
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

class done(TofuApp):
    @command
    def add(self, options, *args):
        print "adding a task"

    @command
    def list(self, options, *args):
        print "listing tasks"

    @option
    def due_date(self):
        return {
            'name' : 'due_date',
            'help' : 'a due date. Accepts natural language input (ie, tomorrow)'
        }

if __name__ == '__main__':
    sys.exit( app().run() )
