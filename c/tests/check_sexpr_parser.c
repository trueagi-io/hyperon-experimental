#include <hyperon/hyperon.h>

#include "test.h"
#include "util.h"
#include "int_gnd.h"

void setup(void) {
}

void teardown(void) {
}

START_TEST (test_tokenizer_parser)
{
    tokenizer_t tokenizer = tokenizer_new();
    static token_api_t int_atom_token = { .construct_atom = &int_atom_from_str, .free_context = NULL };
    tokenizer_register_token(&tokenizer, "\\d+", &int_atom_token, NULL);
    sexpr_parser_t parser = sexpr_parser_new("(= (fac $n) (* $n (fac (- $n 1))))");

    atom_t parse_result = sexpr_parser_parse(&parser, &tokenizer);
    atom_t expected_result = expr(
            atom_sym("="), 
            expr(atom_sym("fac"), atom_var("n"), atom_ref_null()),
            expr(atom_sym("*"), atom_var("n"),
                expr(atom_sym("fac"),
                    expr(atom_sym("-"), atom_var("n"), atom_gnd(int_new(1)), atom_ref_null()),
                    atom_ref_null()),
                atom_ref_null()),
            atom_ref_null()
        );
    ck_assert(atom_eq(&parse_result, &expected_result));

    //Calling the parser a second time should produce nothing
    atom_t second_parse_result = sexpr_parser_parse(&parser, &tokenizer);
    ck_assert(atom_is_null(&second_parse_result));

    atom_free(parse_result);
    atom_free(expected_result);
    sexpr_parser_free(parser);
    tokenizer_free(tokenizer);
}
END_TEST

typedef struct node_types {
    int32_t count;
    syntax_node_type_t type_buf[32];
} node_types;

void save_node_types(const syntax_node_t* node, void *context) {
    node_types* nodes = (node_types*)context;
    nodes->type_buf[nodes->count] = syntax_node_type(node);
    nodes->count++;
};

START_TEST (test_syntax_tree_parser)
{
    sexpr_parser_t parser = sexpr_parser_new("(+ $one \"one\") ; Is it 2?");

    syntax_node_t root_node = sexpr_parser_parse_to_syntax_tree(&parser);

    node_types nodes;
    nodes.count = 0;
    syntax_node_iterate(&root_node, &save_node_types, &nodes);

    ck_assert_int_eq(nodes.count, 8);
    ck_assert_int_eq(nodes.type_buf[0], OPEN_PAREN);
    ck_assert_int_eq(nodes.type_buf[1], WORD_TOKEN);
    ck_assert_int_eq(nodes.type_buf[2], WHITESPACE);
    ck_assert_int_eq(nodes.type_buf[3], VARIABLE_TOKEN);
    ck_assert_int_eq(nodes.type_buf[4], WHITESPACE);
    ck_assert_int_eq(nodes.type_buf[5], STRING_TOKEN);
    ck_assert_int_eq(nodes.type_buf[6], CLOSE_PAREN);
    ck_assert_int_eq(nodes.type_buf[7], EXPRESSION_GROUP);

    syntax_node_free(root_node);
    sexpr_parser_free(parser);
}
END_TEST

void init_test(TCase* test_case) {
    tcase_set_timeout(test_case, 300); //300s = 5min.  To test for memory leaks
    tcase_add_checked_fixture(test_case, setup, teardown);
    tcase_add_test(test_case, test_tokenizer_parser);
    tcase_add_test(test_case, test_syntax_tree_parser);
}

TEST_MAIN(init_test);
