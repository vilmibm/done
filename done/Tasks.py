# who  nate smith
# when march 2010
# why  the done tool
# where midwest usa

import sys
from time import mktime, time
from datetime import datetime

import sqlite3
from termcolor import colored

import sql_interp.sql_interp as si

from Config import db_path

class Task:
    def __init__(self, desc, due):
        self.desc   = desc
        self.si     = si.SQLInterp()
        self.db     = sqlite3.connect(db_path)
        self.c      = self.db.cursor()

        self.due = datetime.fromtimestamp(due) if due else None

    def add(self):
        insert = {
            "desc"    : self.desc,
            "created" : time()
        }
        if self.due:
            insert["due"] = mktime(self.due.timetuple())

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
        
    def remove(self):
        sys.stdout.write("\t * " + str(self) + "? ")
        answer = raw_input("[rmN]:") 
        if answer == "rm":
            self.delete()
            return True
        return False

    def finish(self):
        where = { "desc":self.desc }

        interped = self.si.interp("UPDATE tasks SET done=1 WHERE", where)

        self.c.execute(interped[0], interped[1])
        self.db.commit()
    
    def delete(self):
        where = { "desc":self.desc }
    
        interped = self.si.interp("DELETE FROM tasks WHERE", where)
    
        self.c.execute(interped[0], interped[1])
        self.db.commit()

    def pretty_due(self):
        if not self.due:
            return ""

        due_string = self.due.strftime("%a, %Y-%m-%d %X")

        overdue = lambda s: colored(s, "white", "on_red")
        red     = lambda s: colored(s, "red")
        yellow  = lambda s: colored(s, "yellow")
        green   = lambda s: colored(s, "green")

        now = datetime.now()

        delta = self.due - now

        if delta.days < 0: # overdue
            due_string = overdue(due_string)
        
        if delta.days == 0:
            due_string = red(due_string)
        
        if delta.days == 1:
            due_string = yellow(due_string)
        
        if delta.days > 1:
            due_string = green(due_string)
            
        return due_string

    def __str__(self):
        due_string = self.pretty_due()
        return "%s %s" % (self.desc, due_string)

