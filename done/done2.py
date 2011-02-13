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

from tofuroll import *
import peewee

import config

database = peewee.Database(peewee.SqliteAdapter(), config.db_path)

class Task(peewee.Model):
    desc      = peewee.CharField()
    due_date  = peewee.DateTimeField(null=True)
    done      = peewee.BooleanField()

    class Meta:
        database = database

    def __unicode__(self):
        ustr = unicode(self.desc)
        if self.due_date:
            ustr += ( u' (due: %s)' % self.due_date)

        return ustr

    def __str__(self):
        return unicode(self)

class done(TofuApp):
    @command
    def add(self, options, args):
        try:
            desc = args.pop()
        except:
            raise ValueError("add command needs exactly 1 argument")

        t = Task(desc=desc, due_date=None, done=False)
        t.save()
        print "added '%s'" % t

    @command
    def list(self, options, args):
        tasks = Task.select().where(done=False)
        for t in tasks:
            print t

    @command
    def done(self, options, args):
        tasks = Task.select().where(done=False)
        for t in tasks:
            answer = raw_input("done with '%s'? [yN]: " % t)
            if answer.lower().startswith('y'):
                t.done = True
                t.save()

    @command
    def remove(self, options, args):
        tasks = Task.select().where(done=False)
        for t in tasks:
            answer = raw_input("remove '%s'? [yN]: " % t)
            if answer.lower().startswith('y'):
                Task.delete().where(desc=t.id)
                t.done = True
                t.save()

    @option
    def due_date(self):
        return {
            'name' : 'due_date',
            'help' : 'a due date. Accepts natural language input (ie, tomorrow)'
        }

if __name__ == '__main__':
    database.connect()
    # This could be more elegant. Works for now.
    try:
        Task.create_table()
    except:
        pass
    sys.exit( done().run() )
