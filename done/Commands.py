# who  nate smith
# when march 2010
# why  the done tool
# where midwest usa

import subprocess
from time import mktime

import parsedatetime.parsedatetime as pdt

from Tasks   import Task
from Queries import Query
from Config  import db_path

commands = ['list', 'done', 'add', 'backend']

num_args = {
    "list"    : lambda n: n >= 0,
    "done"    : lambda n: n >= 0,
    "add"     : lambda n: n >= 1,
    "backend" : lambda n: n == 0
}

def run(command, options, args):
    """Run the requested command. args is either a list of descriptions or a list of strings to filter by"""

    if command == "backend":
        subprocess.call(("sqlite3", db_path))

    if command == "add":
        dp = pdt.Calendar()
        due = mktime(dp.parse(options.due)[0]) if options.due else None
        print "added tasks..."
        [Task(desc, due).add() for desc in args]
        return

    filters = args if len(args) else None
    rows    = Query(filters, options.due, options.sort).find()
    tasks   = [Task(r["desc"], r["due"]) for r in rows]

    if command == "list":
        for t in tasks:
            print "\t *", t

    if command == "done":
        print "done with..."
        finished_tasks = []
        for t in tasks: 
            finished = t.done()
            if finished:
                finished_tasks.append(t)

        if not finished_tasks:
            return

        print ""
        print "finished tasks:"
        for t in finished_tasks:
            print "\t X", t

class ArgError(Exception):
    def __str__(self):
        return repr(self.value)

def match(command, options, args):
    """disambiguate a command (expanding, for eg, lis into list) and validate the number of args passed for it"""
    build = ""
    possible = commands
    for l in command:
        build += l
        possible = filter(lambda w: w.startswith(build), possible)
    
    if len(possible) == 0:
        raise ArgError("Command invalid: %s" % command)
    
    if len(possible) > 1:
        raise ArgError("Ambiguous command: %s" % command)

    command = possible.pop()

    if not num_args[command](len(args)):
        raise ArgError("Bad number of args for command %s" % command)

    return command
