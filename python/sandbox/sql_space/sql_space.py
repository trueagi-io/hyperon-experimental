import psycopg2
from hyperon import *
from hyperon.ext import register_atoms
import re


def results2bindings(vars, values):
    new_bindings_set = BindingsSet.empty()
    if len(values) == 0 or len(vars) != len(values[0]):
        return new_bindings_set

    for value in values:
        bindings = Bindings()
        for i in range(len(vars)):
           bindings.add_var_binding(vars[i], ValueAtom(str(value[i])))
        new_bindings_set.push(bindings)
        
    return new_bindings_set

class SqlHelper:
    def __init__(self):
        self.conn = psycopg2.connect(database="musicbrainz_db",
                                host="localhost",
                                user="musicbrainz",
                                password="musicbrainz",
                                port="5432")
        self.cursor = self.conn.cursor()

    def select(self, fields, table,  condition="", limit=""):
        query = "SELECT " + fields + " FROM " + table + " WHERE " + condition
        if str(limit).isdigit():
            query = query + " LIMIT " + str(limit)
        self.cursor.execute(query)
        return self.cursor.fetchall()

class SqlSpace(GroundingSpace):
    def __init__(self, conn, cursor):
        self.conn = conn
        self.cursor =cursor
        self.colums_word = "ColumnNames"

    def get_variables(self, query):
        left = query.rfind(self.colums_word)
        if left == -1:
            return ""
        left = left + len(self.colums_word)
        i = left
        start = left
        while i < len(query) and query[i] != ")":
            if query[i] == "(":
                start = i + 1
            i = i + 1
        return query[start:i] if i < len(query) else ""


    def query(self, query_atom):
        query = repr(query_atom)
        vars_names = self.get_variables(query)
        new_bindings_set = BindingsSet.empty()
        if len(vars_names) > 0:
            vars = vars_names.replace("$", "").split(" ")
            str_for_query = ", ".join(vars)
            query = query.replace(self.colums_word, "").replace("(" + vars_names + ")", str_for_query)
            self.cursor.execute(query)
            values = self.cursor.fetchall()
            return results2bindings(vars, values)

        return new_bindings_set

def wrapsqlop(func):
    def wrapper(*args):
        a = [repr(arg) if isinstance(arg, SymbolAtom) else arg.get_object().value for arg in args]
        res = func(*a)
        return [ValueAtom(val) for val in res]
    return wrapper
    

@register_atoms
def sql_space_atoms():
    helper = SqlHelper()
    newNSpaceAtom = OperationAtom('new-sql-space', lambda: [G(SpaceRef(SqlSpace(helper.conn, helper.cursor)))], unwrap=False)
    sqlSelect = G(OperationObject('sql.select', wrapsqlop(helper.select), unwrap=False))
    return {
        r"new-sql-space": newNSpaceAtom,
        r"sql.select": sqlSelect
    }
