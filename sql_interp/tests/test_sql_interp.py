from nose.tools import eq_, ok_
from sql_interp import SQLInterp
from esc_types import Esc, ListEsc, DictEsc

### Test SQLInterp.esc() method.

def test_esc_int():
    sqli = SQLInterp()
    obj = sqli.esc(1)
    eq_(type(obj), Esc)

def test_esc_str():
    sqli = SQLInterp()
    obj = sqli.esc('')
    eq_(type(obj), Esc)

def test_esc_dict():
    sqli = SQLInterp()
    obj = sqli.esc({})
    eq_(type(obj), DictEsc)

def test_esc_list():
    sqli = SQLInterp()
    obj = sqli.esc([])
    eq_(type(obj), ListEsc)

def test_esc_tuple():
    # Tuples are treated identically to lists.
    sqli = SQLInterp()
    obj = sqli.esc((1,))
    eq_(type(obj), ListEsc)

### Test SQLInterp.add_types() method.

def test_add_types_custom():
    sqli = SQLInterp()

    class MyClass(object): pass
    class MyClassEsc(Esc): pass

    sqli.add_types({ MyClass: MyClassEsc })
    
    obj = sqli.esc(MyClass())
    eq_(type(obj), MyClassEsc)

def test_add_types_custom_constructor():
    class MyClass(object): pass
    class MyClassEsc(Esc): pass

    sqli = SQLInterp({ MyClass: MyClassEsc })

    obj = sqli.esc(MyClass())
    eq_(type(obj), MyClassEsc)

### Test SQLInterp.interp() method.

def test_sql_interp_no_whitespace():
    # Whitespace should be added between arguments.
    sqli = SQLInterp()

    sql, bind = sqli.interp("SELECT", "*", "FROM table")
    eq_(sql, "SELECT * FROM table")
    eq_(bind, ())

    cols = ['one', 'two', 'three']
    sql, bind = sqli.interp("SELECT", cols, "FROM table")
    eq_(sql, "SELECT (?, ?, ?) FROM table")
    eq_(bind, ('one', 'two', 'three'))

def test_sql_interp_extra_whitespace():
    # Excess whitespace is fine.
    sqli = SQLInterp()

    sql, bind = sqli.interp("SELECT ", " *", "   FROM table")
    eq_(sql, "SELECT  *    FROM table")
    eq_(bind, ())

def test_sql_interp_dict():
    sqli = SQLInterp()

    where = { 'first_name': 'John', 'last_name': 'Doe' }
    sql, bind = sqli.interp("SELECT * FROM users WHERE", where)
    eq_(sql, "SELECT * FROM users WHERE first_name = ? AND last_name = ?")
    eq_(bind, ('John', 'Doe'))

    where = { 'first_name': ['John', 'Jane'], 'last_name': 'Doe' }
    sql, bind = sqli.interp("SELECT * FROM users WHERE", where)
    eq_(sql, "SELECT * FROM users WHERE first_name IN (?, ?) AND last_name = ?")
    eq_(bind, ('John', 'Jane', 'Doe'))

def test_sql_interp_dict_none():
    sqli = SQLInterp()

    where = { 'first_name': None, 'last_name': 'Doe' }
    sql, bind = sqli.interp("SELECT * FROM users WHERE", where)
    eq_(sql, "SELECT * FROM users WHERE first_name IS NULL AND last_name = ?")
    eq_(bind, ('Doe',))

def test_sql_interp_tuple():
    # Tuples are treated identically to lists.
    sqli = SQLInterp()

    cols = ('one', 'two', 'three')
    sql, bind = sqli.interp("SELECT", cols, "FROM table")
    eq_(sql, "SELECT (?, ?, ?) FROM table")
    eq_(bind, ('one', 'two', 'three'))

def test_sql_interp_string():
    sqli = SQLInterp()

    full_name = 'John Doe'
    sql, bind = sqli.interp("SELECT * FROM table WHERE full_name =", sqli.esc(full_name))
    eq_(sql, "SELECT * FROM table WHERE full_name = ?")
    eq_(bind, ('John Doe',))
