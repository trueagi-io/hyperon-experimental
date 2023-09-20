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
    colums_word = "ColumnNames"

    def select(self, space, fields, table, condition="", limit=""):
        # if no fields provided get them from information_schema.columns
        if "*" in fields:
            query = ''
            if '.' in table:
                table_info = table.split('.')
                if len(table_info) == 2:
                    query = f'''SELECT column_name  FROM information_schema.columns  WHERE table_schema = '{table_info[0]}'
                            AND table_name = '{table_info[1]}' '''
            else:
                query = f'''SELECT column_name  FROM information_schema.columns  WHERE table_name = {table} '''
            fields = space.query(E(S(query)))
            temp = []
            for val in fields:
                temp.extend(repr(field)[1:-1] for field in val.values())
            fields = " ".join(temp)

        fields = f"{self.colums_word}({fields.replace(',', '')})"

        query = f"SELECT {fields} FROM {table} WHERE {condition}"
        if str(limit).isdigit():
            query = query + " LIMIT " + str(limit)
        res = space.query(E(S(query)))
        return res

    def insert(self, space, table, fields, values, returning):
        query = f'''INSERT INTO {table} ({fields})	VALUES ({values}) RETURNING {returning};'''
        res = space.query(E(S(query)))
        return res


class SqlSpace(GroundingSpace):
    def __init__(self, database, host, user, password, port):
        super().__init__()
        self.conn = psycopg2.connect(database=database,
                                     host=host,
                                     user=user,
                                     password=password,
                                     port=port)
        self.cursor = self.conn.cursor()

    def from_space(self, cspace):
        self.gspace = GroundingSpaceRef(cspace)

    def get_variables(self, query):
        left = query.rfind(SqlHelper.colums_word)
        if left == -1:
            return ""
        left = left + len(SqlHelper.colums_word)
        i = left
        start = left
        while i < len(query) and query[i] != ")":
            if query[i] == "(":
                start = i + 1
            i = i + 1
        return query[start:i] if i < len(query) else ""

    def query(self, query_atom):
        try:
            query = repr(query_atom)
            vars_names = self.get_variables(query)
            new_bindings_set = BindingsSet.empty()
            vars = []
            if len(vars_names) > 0:
                vars = vars_names.replace("$", "").split(" ")
                str_for_query = ", ".join(vars)
                query = query.replace(SqlHelper.colums_word, "").replace("(" + vars_names + ")", str_for_query)

            self.cursor.execute(query[1:-1])
            values = self.cursor.fetchall()
            if len(vars_names) == 0 and len(values) > 0:
                vars = [f"var{i + 1}" for i in range(len(values[0]))]
            if len(vars) > 0:
                return results2bindings(vars, values)
            if query.strip().startswith("(INSERT"):
                self.conn.commit()
            return new_bindings_set
        except (Exception, psycopg2.DatabaseError) as error:
            print(error)
            if self.conn is not None:
                self.conn.close()


def wrapsqlop(func):
    def wrapper(*args):
        if len(args) > 0 and isinstance(args[0], GroundedAtom) and isinstance(args[0].get_object(), SpaceRef):
            space = args[0].get_object()
            args = args[1:]
            a = [repr(arg) if isinstance(arg, SymbolAtom) else arg.get_object().value for arg in args]
            res = func(space, *a)
            return [ValueAtom(val) for val in res]
        return []

    return wrapper


@register_atoms
def sql_space_atoms():
    helper = SqlHelper()
    newNSpaceAtom = OperationAtom('new-sql-space', lambda database, host, user, password, port: [
        G(SpaceRef(SqlSpace(database, host, user, password, port)))], unwrap=False)
    sqlSelect = G(OperationObject('sql.select', wrapsqlop(helper.select), unwrap=False))
    sqlInsert = G(OperationObject('sql.insert', wrapsqlop(helper.insert), unwrap=False))
    return {
        r"new-sql-space": newNSpaceAtom,
        r"sql.select": sqlSelect,
        r"sql.insert": sqlInsert
    }
