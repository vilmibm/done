# who  nate smith
# when march 2010
# why  the done tool
# where midwest usa

import sys
from time import mktime, localtime, time, strftime
from datetime import date

import sqlite3
from termcolor import colored

import sql_interp.sql_interp as si

from Config import db_path

class Task:
    def __init__(self, desc, due):
        self.due    = due
        self.desc   = desc
        self.si     = si.SQLInterp()
        self.db     = sqlite3.connect(db_path)
        self.c      = self.db.cursor()

    def add(self):
        insert = {
            "desc"    : self.desc,
            "created" : time()
        }
        if self.due:
            insert["due"] = self.due

        interped = self.si.interp("INSERT INTO tasks", insert)

        self.c.execute(interped[0], interped[1])
        self.db.commit()

        print "\t *", self

    def done(self):
        sys.stdout.write("\t * " + str(self) + "? ")
        answer = raw_input("[dN]:") 
        if answer == "d":
            self.finish()
            return True

        return False

    def finish(self):
        where = { "desc":self.desc }
#        if self.due:
#            where["due"] = mktime(self.dp.parse(self.due)[0])

        interped = self.si.interp("UPDATE tasks SET done=1 WHERE", where)

        self.c.execute(interped[0], interped[1])
        self.db.commit()

    def pretty_due(self):
        if not self.due:
            return ""

        diff = abs(time() - self.due)

        color = "green"
        if diff / 60.0 / 60.0 < 12:
            color = "red"
        elif diff / 60.0 / 60.0 < 24:
            color = "yellow"

        return colored("due: %s" % strftime("%a, %Y-%m-%d %X", localtime(self.due)), color)

    def __str__(self):
        due_string = self.pretty_due()
        return "%s %s" % (self.desc, due_string)

