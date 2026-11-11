#include <hyperon/hyperon.h>

#include <stdlib.h>
#include <string.h>

#include "test.h"
#include "util.h"

typedef struct {
    const char* expected;
    unsigned calls;
    serial_result_t result;
} string_receiver;

static serial_result_t receive_string(void* context, const char* value) {
    string_receiver* receiver = context;
    receiver->calls++;
    ck_assert_str_eq(value, receiver->expected);
    char* copy = malloc(strlen(value) + 1);
    ck_assert_ptr_nonnull(copy);
    strcpy(copy, value);
    ck_assert_str_eq(value, copy);
    free(copy);
    return receiver->result;
}

static atom_t parse_string(tokenizer_t* tokenizer, const char* source) {
    sexpr_parser_t parser = sexpr_parser_new(source);
    atom_t atom = sexpr_parser_parse(&parser, tokenizer);
    ck_assert(!atom_is_null(&atom));
    ck_assert_int_eq(atom_get_metatype(&atom), GROUNDED);
    sexpr_parser_free(parser);
    return atom;
}

START_TEST(test_string_serialization)
{
    metta_t metta = new_test_metta();
    tokenizer_t tokenizer = metta_tokenizer(&metta);
    serializer_api_t api = { .serialize_str = receive_string };
    serializer_api_t absent = { 0 };
    string_receiver receiver = { .result = OK };
    const char* sources[] = {
        "\"\"", "\"ASCII\"",
        "\"\\u{017e}lu\\u{0165}ou\\u{010d}k\\u{00fd} \\u{1f980}\""
    };
    const char* values[] = {
        "", "ASCII",
        "\xc5\xbe" "lu\xc5\xa5" "ou\xc4\x8d" "k\xc3\xbd \xf0\x9f\xa6\x80"
    };
    for (size_t i = 0; i < 3; i++) {
        atom_t atom = parse_string(&tokenizer, sources[i]);
        receiver.expected = values[i];
        unsigned before = receiver.calls;
        ck_assert_int_eq(atom_gnd_serialize(&atom, &api, &receiver), OK);
        ck_assert_uint_eq(receiver.calls, before + 1);
        ck_assert_int_eq(atom_gnd_serialize(&atom, &absent, NULL), NOT_SUPPORTED);
        receiver.result = NOT_SUPPORTED;
        ck_assert_int_eq(atom_gnd_serialize(&atom, &api, &receiver), NOT_SUPPORTED);
        ck_assert_uint_eq(receiver.calls, before + 2);
        receiver.result = OK;
        atom_free(atom);
    }
    // Escapes are parsed into genuine grounded Rust strings, not C string prefixes.
    const char* rejected[] = {
        "\"\\u{0}start\"", "\"mid\\u{0}dle\"", "\"end\\u{0}\"", "\"\\u{0}\\u{0}\""
    };
    for (size_t i = 0; i < 4; i++) {
        atom_t atom = parse_string(&tokenizer, rejected[i]);
        unsigned before = receiver.calls;
        ck_assert_int_eq(atom_gnd_serialize(&atom, &api, &receiver), NOT_SUPPORTED);
        ck_assert_uint_eq(receiver.calls, before);
        ck_assert_int_eq(atom_gnd_serialize(&atom, &absent, NULL), NOT_SUPPORTED);
        atom_free(atom);

        atom = parse_string(&tokenizer, "\"after rejection\"");
        receiver.expected = "after rejection";
        ck_assert_int_eq(atom_gnd_serialize(&atom, &api, &receiver), OK);
        ck_assert_uint_eq(receiver.calls, before + 1);
        atom_free(atom);
    }
    tokenizer_free(tokenizer);
    metta_free(metta);
}
END_TEST

void init_test(TCase* test_case) {
    tcase_add_test(test_case, test_string_serialization);
}

TEST_MAIN(init_test);
