import unittest

from hyperon import *

class HyperonTestCase(unittest.TestCase):

    def __init__(self, methodName):
        super().__init__(methodName)

    def testParseToSyntaxNodes(self):
        parser = SExprParser("(+ one \"one\")")
        syntax_node = parser.parse_to_syntax_tree()
        leaf_node_list = syntax_node.unroll()
        leaf_node_types = [];
        for node in leaf_node_list:
            leaf_node_types.append(node.get_type())

        expected_node_types = [SyntaxNodeType.OPEN_PAREN,
            SyntaxNodeType.WORD_TOKEN,
            SyntaxNodeType.WHITESPACE,
            SyntaxNodeType.WORD_TOKEN,
            SyntaxNodeType.WHITESPACE,
            SyntaxNodeType.STRING_TOKEN,
            SyntaxNodeType.CLOSE_PAREN];

        self.assertEqual(leaf_node_types, expected_node_types)

    def testParseErr(self):
        tokenizer = Tokenizer()
        parser = SExprParser("(+ one \"one")
        parsed_atom = parser.parse(tokenizer)
        self.assertEqual(parsed_atom, None)
        self.assertEqual(parser.parse_err(), "Unclosed String Literal")

        parser = SExprParser("(+ one \"one\"")
        parsed_atom = parser.parse(tokenizer)
        self.assertEqual(parsed_atom, None)
        self.assertEqual(parser.parse_err(), "Unexpected end of expression")

        parser = SExprParser("(+ one \"one\")")
        parsed_atom = parser.parse(tokenizer)
        self.assertTrue(parsed_atom is not None)
        self.assertEqual(parser.parse_err(), None)
