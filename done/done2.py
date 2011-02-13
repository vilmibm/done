#!/usr/bin/env python
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
    database.connect()
    Task.create_table()
    sys.exit( app().run() )
