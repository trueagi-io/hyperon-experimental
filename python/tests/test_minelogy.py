import unittest
import re

from hyperon import *
from common import Atomese, AtomspaceAtom


class MinelogyTest(unittest.TestCase):

    def test_minelogy(self):
        # A nearly direct reimplementation of minelogy as it
        # was in the minecraft demo. Not optimal representation -
        # just testing.
        atomese = Atomese()
        mines = atomese.parse('''
            (((: log type) (: $x variant))
             (: (stone_axe wooden_axe None) tools)
             ((: log type) (: $x variant))
            )
            ((:: ((: grass type) (: dirt type)))
             (: (stone_shovel wooden_shovel None) tools)
             (: dirt type)
            )
            ((:: ((: sand type)))
             (: (stone_shovel wooden_shovel None) tools)
             (: sand type)
            )
            (((: stone type) (: stone variant))
             (: (stone_pickaxe wooden_pickaxe) tools)
             ((: cobblestone type) (: $_ variant))
            )
            (((: stone type) (: $x variant))
             (: (stone_pickaxe wooden_pickaxe) tools)
             ((: stone type) (: $x variant))
            )
            ''')
        crafts = atomese.parse('''
            (((: log type) (: $x variant) (: 1 quantity))
             ((: planks type) (: $x variant) (: 4 quantity)))
            (((: planks type) (: $x variant) (: 2 quantity))
             ((: stick type) (: $_ variant) (: 4 quantity)))
            ((:: (((: stick type) (: $x variant) (: 2 quantity))
                  ((: planks type) (: $y variant) (: 3 quantity))))
             ((: wooden_pickaxe type) (: $_ variant) (: 1 quantity)))
            ''')
        # atomese.add_token("mines", ValueAtom(mines))
        # atomese.add_atom("mines", AtomspaceAtom(mines, "mines"))
        atomese.add_atom("mines", G(AtomspaceAtom(mines, "mines")))
        atomese.add_atom("crafts", G(AtomspaceAtom(crafts, "crafts")))
        utils = atomese.parse('''
            (= (get-mine-block $ent-type $ent-var)
               (match mines
                      (((: $block type) (: $variant variant))
                       (: $tools tools)
                       ((: $ent-type type) (: $ent-var variant)))
                      ((: $block type) (: $variant variant))
               )
            )
            (= (get-mine-block $ent-type)
               (match mines
                      ((:: $xs)
                       (: $tools tools)
                       (: $ent-type type))
                      (:: $xs)
               )
            )
            (= (get-mine-block $ent-type)
               (get-mine-block $ent-type $any))
            (= (get-mine-tools $ent-type $ent-var)
               (match mines
                      (((: $block type) (: $variant variant))
                       (: $tools tools)
                       ((: $ent-type type) (: $ent-var variant)))
                      $tools
               )
            )
            (= (get-mine-tools $ent-type)
               (match mines
                      ((:: $any)
                       (: $tools tools)
                       (: $ent-type type))
                      $tools
               )
            )
            (= (get-mine-tools $ent-type)
               (get-mine-tools $ent-type $any))
            (= (get-ingredients $ent-type $variant)
               (match crafts
                      (((: $ent type) (: $var variant) (: $quant quantity))
                       ((: $ent-type type) (: $variant variant) (: $_ quantity)))
                      ((: $ent type) (: $var variant) (: $quant quantity))
               )
            )
            (= (get-ingredients $ent-type $variant)
               (match crafts
                      ((:: $ingredients)
                       ((: $ent-type type) (: $variant variant) (: $_ quantity)))
                      (:: $ingredients)
               )
            )
            (= (get-ingredients $ent-type)
               (get-ingredients $ent-type $any)
            )
            (= (how-get $ent-type)
               (let $t (get-ingredients $ent-type)
                    (do-craft $t))
            )
            (= (how-get $ent-type)
               (let $t (get-mine-block $ent-type)
                    (do-mine $t))
            )
            ''')
        target = atomese.parse_single('(how-get cobblestone)')
        output = interpret(utils, target)
        self.assertEqual(repr(output[0]),
            '(do-mine ((: stone type) (: stone variant)))')
        target = atomese.parse_single('(how-get stick)')
        output = interpret(utils, target)
        self.assertEqual(repr(output[0]),
            '(do-craft ((: planks type) (: $x variant) (: 2 quantity)))')


init_logger()
if __name__ == "__main__":
    unittest.main()
