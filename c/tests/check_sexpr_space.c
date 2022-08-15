#include <hyperon/hyperon.h>

#include "test.h"
#include "util.h"
#include "int_gnd.h"

void setup(void) {
}

void teardown(void) {
}

START_TEST (test_parsing_expr)
{
    tokenizer_t* tokenizer = tokenizer_new();
    droppable_t empty_context = { 0, 0 };
    tokenizer_register_token(tokenizer, "\\d+", int_atom_from_str, empty_context);
    sexpr_space_t* text = sexpr_space_new(tokenizer);
    grounding_space_t* space = grounding_space_new();

    ck_assert(sexpr_space_add_str(text, "(= (fac $n) (* $n (fac (- $n 1))))"));
    sexpr_space_into_grounding_space(text, space);

    ck_assert(1 == grounding_space_len(space));
    ck_assert(atom_eq(grounding_space_get(space, 0),
                expr(atom_sym("="), expr(atom_sym("fac"), atom_var("n"), 0),
                    expr(atom_sym("*"), atom_var("n"),
                        expr(atom_sym("fac"),
                            expr(atom_sym("-"), atom_var("n"), atom_gnd(int_new(1)), 0),
                            0),
                        0),
                    0)));

    grounding_space_free(space);
    sexpr_space_free(text);
}
END_TEST

START_TEST (test_tokenizer_parser)
{
    tokenizer_t* tokenizer = tokenizer_new();
    droppable_t empty_context = { 0, 0 };
    tokenizer_register_token(tokenizer, "\\d+", int_atom_from_str, empty_context);
    sexpr_parser_t* parser = sexpr_parser_new("(= (fac $n) (* $n (fac (- $n 1))))");

    atom_t* atom = sexpr_parser_parse(parser, tokenizer);
    ck_assert(atom_eq(atom,
                expr(atom_sym("="), expr(atom_sym("fac"), atom_var("n"), 0),
                    expr(atom_sym("*"), atom_var("n"),
                        expr(atom_sym("fac"),
                            expr(atom_sym("-"), atom_var("n"), atom_gnd(int_new(1)), 0),
                            0),
                        0),
                    0)));
    ck_assert(!sexpr_parser_parse(parser, tokenizer));

    atom_free(atom);
    sexpr_parser_free(parser);
    tokenizer_free(tokenizer);
}
END_TEST

void init_test(TCase* test_case) {
    tcase_add_checked_fixture(test_case, setup, teardown);
    tcase_add_test(test_case, test_parsing_expr);
    tcase_add_test(test_case, test_tokenizer_parser);
}

TEST_MAIN(init_test);
