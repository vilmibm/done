import re

# Regular expressions to determine context.
# Borrowed directly from SQL::Interp.
RE_FLAGS = re.S | re.I
NOT_IN_RE = re.compile(r'\b(?:NOT\s+)?IN\s*$', RE_FLAGS)
SET_UPDATE_RE = re.compile(r'\b(?:ON\s+DUPLICATE\s+KEY\s+UPDATE|SET)\s*$', RE_FLAGS)
INSERT_INTO_RE = re.compile(r'\bINSERT[\w\s]*\sINTO\s*[a-zA-Z_][a-zA-Z0-9_\$\.]*\s*$', RE_FLAGS)
FROM_JOIN_RE = re.compile(r'(?:\bFROM|JOIN)\s*$', RE_FLAGS)

class UnknownContextError(Exception): pass

class Esc(object):
    def __init__(self, val):
        self.val = val

    def to_string(self, sql):
        if NOT_IN_RE.search(sql):
            return self.in_ctxt()
        elif SET_UPDATE_RE.search(sql):
            return self.set_update_ctxt()
        elif INSERT_INTO_RE.search(sql):
            return self.insert_into_ctxt()
        elif FROM_JOIN_RE.search(sql):
            return self.from_join_ctxt()
        else:
            return self.default_ctxt()

    def in_ctxt(self):
        raise UnknownContextError("IN context not implemented for this type")

    def set_update_ctxt(self):
        raise UnknownContextError("SET/UPDATE context not implemented for this type")

    def insert_into_ctxt(self):
        raise UnknownContextError("INSERT context not implemented for this type")

    def from_join_ctxt(self):
        raise UnknownContextError("FROM/JOIN context not implemented for this type")

    def default_ctxt(self):
        if type(self.val) is str:
            bind = (self.val,)
        else:
            try:
                bind = tuple(self.val)
            except TypeError:
                bind = (self.val,)

        return '?', bind

class ListEsc(Esc):
    """
    >>> e = ListEsc(['one', 'two', 'three'])
    >>> e.in_ctxt()
    ('(?, ?, ?)', ('one', 'two', 'three'))

    >>> e.set_update_ctxt()
    Traceback (most recent call last):
    ...
    UnknownContextError: SET/UPDATE context not implemented for this type

    >>> e.insert_into_ctxt()
    ('VALUES (?, ?, ?)', ('one', 'two', 'three'))

    >>> e.from_join_ctxt()
    Traceback (most recent call last):
    ...
    UnknownContextError: FROM/JOIN context not implemented for this type

    >>> e.default_ctxt()
    ('(?, ?, ?)', ('one', 'two', 'three'))
    """
    def __init__(self, val):
        self.val = tuple(val)

    def in_ctxt(self):
        return self.default_ctxt()

    def insert_into_ctxt(self):
        sql = 'VALUES (' + ', '.join(['?' for _ in self.val]) + ')'
        return sql, self.val

    def default_ctxt(self):
        sql = '(' + ', '.join(['?' for _ in self.val]) + ')'
        return sql, self.val

class DictEsc(Esc):
    """
    >>> e = DictEsc({'one' : 1, 'two' : 2, 'three' : 3})

    >>> e.in_ctxt()
    Traceback (most recent call last):
    ...
    UnknownContextError: IN context not implemented for this type

    >>> e.set_update_ctxt()
    ('one = ?, three = ?, two = ?', (1, 3, 2))

    >>> e.insert_into_ctxt()
    ('(one, three, two) VALUES (?, ?, ?)', (1, 3, 2))

    >>> e.from_join_ctxt()
    Traceback (most recent call last):
    ...
    UnknownContextError: FROM/JOIN context not implemented for this type

    >>> e.default_ctxt()
    ('one = ? AND three = ? AND two = ?', (1, 3, 2))
    """
    def __init__(self, val):
        self.val = val

    def set_update_ctxt(self):
        sorted_keys = sorted(self.val.keys())

        sql = " = ?, ".join(sorted_keys) + " = ?"
        bind = [self.val[key] for key in sorted_keys]

        return sql, tuple(bind)

    def insert_into_ctxt(self):
        sorted_keys = sorted(self.val.keys())

        sql = "(" + ", ".join(sorted_keys) + ") VALUES (" \
            + ", ".join('?' for _ in sorted_keys) + ")"
        bind = [self.val[key] for key in sorted_keys]

        return sql, tuple(bind)

    def default_ctxt(self):
        sorted_keys = sorted(self.val.keys())

        sql_bits = []
        bind = []

        for key in sorted_keys:
            val = self.val[key]
            if val is None:
                sql_bits.append(key + " IS NULL")
            elif type(val) is list:
                # i.e. key IN (?, ?)
                val_sql, val_bind = ListEsc(val).default_ctxt()
                sql_bits.append(key + " IN " + val_sql)
                for v in val_bind: bind.append(v)
            else:
                sql_bits.append(key + " = ?")
                bind.append(val)

        sql = " AND ".join(sql_bits)

        return sql, tuple(bind)
