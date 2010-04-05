# who  nate smith
# when march 2010
# why  the done tool
# where midwest usa

import sqlite3
import parsedatetime.parsedatetime as pdt
import sql_interp.sql_interp as si
from time import mktime, localtime, strftime
from Config import db_path

class Query:
    def __init__(self, filters, due, sort):
        self.filters = filters
        self.due     = due
        self.sort    = sort
        self.dp      = pdt.Calendar()
        self.si      = si.SQLInterp()
        self.db      = sqlite3.connect(db_path)
        self.c       = self.db.cursor()

    def find(self):
        criteria = { "done":0 }
        if self.due:
            criteria["due"] = mktime(self.dp.parse(self.due)[0])

        interped = self.si.interp("SELECT desc, due FROM tasks WHERE", criteria, "ORDER BY", self.sort, "DESC")

        self.c.execute(interped[0], interped[1])

        rows = []
        for row in self.c:
            rows.append({"desc":row[0], "due":row[1]})
        
        if not self.filters:
            return rows

        filtset = set(self.filters)

        matches = filter(lambda r: set(r["desc"].split()) >= filtset, rows)

        return matches
