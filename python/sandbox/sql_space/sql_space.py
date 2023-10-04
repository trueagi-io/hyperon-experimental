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
    insert_command_sql = "INSERT INTO"

    @staticmethod
    def get_query_atoms(query_atom):
        children = query_atom.get_children()
        new_query_atoms = []
        for ch in children:
            if 'limit' not in repr(ch).lower():
                new_query_atoms.append(ch)
        return new_query_atoms

    @staticmethod
    def get_fields_and_conditions(query_atom):
        ''' parse sql query and get columns to select and conditions for filtering '''
        atoms = query_atom.get_children()
        fields = {}
        conditions = {}
        limit = ""
        vars_map = {}
        for atom in atoms:
            if isinstance(atom, ExpressionAtom):
                items = atom.get_children()
                if len(items) == 3:
                    id_fields = items[1].get_children()
                    current_field_info = items[2].get_children()
                    if len(id_fields) != 2 or len(current_field_info) != 2:
                        raise SyntaxError("Incorrect number of arguments")
                    # (musicbrainz.artist (id $id) (name $name))
                    # identification field
                    id_name = repr(id_fields[0])
                    vars_map[id_name] = repr(id_fields[1])
                    # field to select
                    field_name = repr(current_field_info[0])
                    vars_map[field_name] = repr(current_field_info[1])
                    # table
                    table = repr(items[0])
                    if table not in fields:
                        fields[table] = set()
                    if table not in conditions:
                        conditions[table] = set()
                    # add id field to corresponding category (filed/condition)
                    if isinstance(id_fields[1], VariableAtom):
                        fields[table].add(id_name)
                    else:
                        conditions[table].add(id_name)
                    # add selected field to corresponding category (filed/condition)
                    if isinstance(current_field_info[1], VariableAtom):
                        fields[table].add(field_name)
                    else:
                        conditions[table].add(field_name)

                if len(items) == 2 and ("limit" in repr(items[0]).lower()):
                    limit = repr(items[1])
        return fields, conditions, limit, vars_map

    @staticmethod
    def get_fields_and_values(query_atom):
        ''' parse sql query and get columns to select and conditions for filtering '''
        atoms = query_atom.get_children()
        fields = {}
        vars_map = {}
        for atom in atoms:
            if isinstance(atom, ExpressionAtom):
                items = atom.get_children()
                if len(items) == 2:
                    current_field_info = items[1].get_children()
                    if len(current_field_info) != 2:
                        raise SyntaxError("Incorrect number of arguments")
                    # (musicbrainz.artist (id $id) (name $name)
                    # field to select
                    field_name = repr(current_field_info[0])
                    vars_map[field_name] = repr(current_field_info[1])
                    # table
                    table = repr(items[0])
                    if table not in fields:
                        fields[table] = set()
                    # add selected field to corresponding category (filed/condition)
                    fields[table].add(field_name)
        return fields, vars_map

    def save_query_result(self, sql_space, space, query_atom):
        # if no fields provided get them from information_schema.columns
        res = sql_space.query(query_atom)
        variables = []
        for val in res:
            temp_dict = {}
            for k, v in val.items():
                temp_dict['$' + str(k)] = str(v)
            variables.append(temp_dict)
        atoms = self.get_query_atoms(query_atom)
        new_atoms = []
        for var in variables:
            for atom in atoms:
                if isinstance(atom, ExpressionAtom):
                    temp = repr(atom)
                    for k, v in var.items():
                        temp = temp.replace(k, v)
                    new_atoms.append(temp)
        for atom in new_atoms:
            space.add_atom(E(S(atom)))
        return res

    def insert(self, space, query_atom):
        fields, vars_map = SqlHelper.get_fields_and_values(query_atom)
        res = []
        for table, field_names in fields.items():
            values = []
            for v in field_names:
                values.append(vars_map[v].replace('"', "") if "(" in vars_map[v] and vars_map[v][-2] == ')'
                              else vars_map[v].replace('"', "'"))
            fields_str = ", ".join(list(field_names))
            values_str = ", ".join(list(values))
            query = f'''{self.insert_command_sql} {table} ({fields_str}) VALUES ({values_str}) RETURNING 0;'''
            res.extend(space.query(E(S(query))))
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

    def construct_query(self, query_atom):
        fields, conditions, limit, vars_map = SqlHelper.get_fields_and_conditions(query_atom)
        sql_query = "SELECT"

        vars_names = []
        for k, values in fields.items():
            for val in values:
                sql_query = sql_query + f" {k}.{val},"
                vars_names.append(vars_map[val])
        sql_query = sql_query[:-1] + " FROM "
        for k in fields.keys():
            sql_query = sql_query + f"{k},"

        sql_condition = " WHERE"
        for k, values in conditions.items():
            for val in values:
                if val in vars_map:
                    sql_condition = sql_condition + f" {k}.{val} = {vars_map[val]} AND"
        if len(sql_condition) > 6:
            sql_query = sql_query[:-1] + sql_condition[:-4]
        else:
            sql_query = sql_query[:-1]
        if len(limit) > 0:
            sql_query = sql_query + f" LIMIT {limit}"
        return sql_query, vars_names

    def insert(self, sql_query):
        try:
            if len(sql_query) > 6:
                self.cursor.execute(sql_query)
                self.conn.commit()
        except (Exception, psycopg2.DatabaseError) as error:
            bindings_set = BindingsSet.empty()
            bindings = Bindings()
            bindings.add_var_binding("error on insert: ", ValueAtom(error))
            bindings_set.push(bindings)
            return bindings_set
        return BindingsSet.empty()

    def query(self, query_atom):
        try:
            atoms = query_atom.get_children()
            if len(atoms) > 0 and SqlHelper.insert_command_sql in repr(atoms[0]):
                return self.insert(repr(atoms[0]))
            else:
                new_bindings_set = BindingsSet.empty()
                sql_query, vars_names = self.construct_query(query_atom)
                if len(sql_query) > 6:
                    self.cursor.execute(sql_query)
                    values = self.cursor.fetchall()
                    if len(vars_names) == 0 and len(values) > 0:
                        vars = [f"var{i + 1}" for i in range(len(values[0]))]
                    else:
                        vars = [v[1:] for v in vars_names]
                    if len(vars) > 0 and len(values) > 0:
                        return results2bindings(vars, values)
                return new_bindings_set
        except (Exception, psycopg2.DatabaseError) as error:
            print(error)


def wrapsqlop(func):
    def wrapper(*args):
        if len(args) > 1:
            if isinstance(args[0], GroundedAtom):
                space1 = args[0].get_object()
                if isinstance(space1, SpaceRef):
                    if isinstance(args[1], GroundedAtom):
                        space2 = args[1].get_object()
                        if isinstance(space2, SpaceRef):
                            args = args[2:]
                            res = func(space1, space2, *args)
                            return [ValueAtom(val) for val in res]
                    else:
                        args = args[1:]
                        res = func(space1, *args)
                        return [ValueAtom(val) for val in res]
        return []

    return wrapper


@register_atoms
def sql_space_atoms():
    helper = SqlHelper()
    newSQLSpaceAtom = OperationAtom('new-sql-space', lambda database, host, user, password, port: [
        G(SpaceRef(SqlSpace(database, host, user, password, port)))], unwrap=False)
    saveQueryResult = G(OperationObject('sql.save-query-result', wrapsqlop(helper.save_query_result), unwrap=False))
    sqlInsert = G(OperationObject('sql.insert', wrapsqlop(helper.insert), unwrap=False))
    return {
        r"new-sql-space": newSQLSpaceAtom,
        r"sql.save-query-result": saveQueryResult,
        r"sql.insert": sqlInsert
    }
