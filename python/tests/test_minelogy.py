import unittest
import re

from hyperon import *
from common import MeTTa, SpaceAtom


class MinelogyTest(unittest.TestCase):

    def test_minelogy(self):
        # A nearly direct reimplementation of minelogy as it
        # was in the minecraft demo. Not optimal representation -
        # just testing.
        mines = MeTTa()
        mines.add_parse('''
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
        crafts = MeTTa()
        crafts.add_parse('''
            (((: log type) (: $x variant) (: 1 quantity))
             ((: planks type) (: $x variant) (: 4 quantity)))
            (((: planks type) (: $x variant) (: 2 quantity))
             ((: stick type) (: $_ variant) (: 4 quantity)))
            ((:: (((: stick type) (: $x variant) (: 2 quantity))
                  ((: planks type) (: $y variant) (: 3 quantity))))
             ((: wooden_pickaxe type) (: $_ variant) (: 1 quantity)))
            ''')
        utils = MeTTa()
        utils.add_atom("&mines", SpaceAtom(mines.space, "&mines"))
        utils.add_atom("&crafts", SpaceAtom(crafts.space, "&crafts"))
        utils.add_parse('''
            (= (get-mine-block $ent-type $ent-var)
               (match &mines
                      (((: $block type) (: $variant variant))
                       (: $tools tools)
                       ((: $ent-type type) (: $ent-var variant)))
                      ((: $block type) (: $variant variant))
               )
            )
            (= (get-mine-block $ent-type)
               (match &mines
                      ((:: $xs)
                       (: $tools tools)
                       (: $ent-type type))
                      (:: $xs)
               )
            )
            (= (get-mine-block $ent-type)
               (get-mine-block $ent-type $any))
            (= (get-mine-tools $ent-type $ent-var)
               (match &mines
                      (((: $block type) (: $variant variant))
                       (: $tools tools)
                       ((: $ent-type type) (: $ent-var variant)))
                      $tools
               )
            )
            (= (get-mine-tools $ent-type)
               (match &mines
                      ((:: $any)
                       (: $tools tools)
                       (: $ent-type type))
                      $tools
               )
            )
            (= (get-mine-tools $ent-type)
               (get-mine-tools $ent-type $any))
            (= (get-ingredients $ent-type $variant)
               (match &crafts
                      (((: $ent type) (: $var variant) (: $quant quantity))
                       ((: $ent-type type) (: $variant variant) (: $_ quantity)))
                      ((: $ent type) (: $var variant) (: $quant quantity))
               )
            )
            (= (get-ingredients $ent-type $variant)
               (match &crafts
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
        output = utils.interpret('(how-get cobblestone)')
        self.assertEqual(repr(output[0]),
            '(do-mine ((: stone type) (: stone variant)))')
        output = utils.interpret('(how-get stick)')
        self.assertEqual(repr(output[0]),
            '(do-craft ((: planks type) (: $x variant) (: 2 quantity)))')

    def test_minelogy_wtypes(self):
        # TODO: revisit this example, when types are automatically checked
        kb = MeTTa()
        kb.add_parse('''
            (: BlockT Type)
            (: log BlockT)
            (: grass BlockT)
            (: dirt BlockT)
            (: sand BlockT)
            (: BlockV Type)
            (: oak BlockV)
            (: stone BlockV)
            (: Block Type)
            (: CBlockT (-> BlockT Block))
            (: CBlockV (-> BlockT BlockV Block))
            (: EntityT Type)
            (: log EntityT)
            (: dirt EntityT)
            (: sand EntityT)
            (: planks EntityT)
            (: stick EntityT)
            (: wooden_pickaxe EntityT)
            (: EntityV Type)
            (: oak EntityV)
            (: stone BlockV)
            (: CEntityT (-> EntityT Entity))
            (: CEntityV (-> EntityT EntityV Entity))
            (: mine (-> Block (List Tool) Entity))
            (: mine (-> (List BlockT) (List Tool) Entity))
            (= (mine (CBlockV log $v)
                     (list stone_axe wooden_axe None))
               (CEntityV log $v))
            (= (mine (list grass dirt)
                     (list stone_shovel wooden_shovel None))
               (CEntityT dirt))
            (= (mine (list sand)
                     (list stone_shovel wooden_shovel None))
               (CEntityT sand))
            (= (mine (CBlockV stone stone)
                     (list stone_pickaxe wooden_pickaxe))
               (CEntityT cobblestone))
            (= (mine (CBlockV stone $v)
                     (list stone_pickaxe wooden_pickaxe))
               (CEntityV stone $v))
            (: craft (-> (List (Entity Int)) (Entity Int)))
            (= (craft (list ((CEntityV log $v) 1)))
               ((CEntityV planks $v) 4))
            (= (craft (list ((CEntityV planks $_) 2)))
               ((CEntityT stick) 4))
            (= (craft (list ((CEntityT stick) 2)
                            ((CEntityV planks $_) 3)))
               ((CEntityT wooden_pickaxe) 1))
            ''')
        utils = MeTTa()
        utils.add_atom("&kb", SpaceAtom(kb.space, "&kb"))
        utils.add_parse('''
            (= (get-mine-block $t)
               (match &kb
                      (= (mine $block $tool) (CEntityT $t))
                      $block))
            (= (get-mine-block $t $v)
               (match &kb
                      (= (mine $block $tool) (CEntityV $t $v))
                      $block))
            (= (get-ingredients $t)
               (match &kb
                      (= (craft $ingred) ((CEntityT $t) $_))
                      $ingred))
            (= (get-ingredients $t $v)
               (match &kb
                      (= (craft $ingred) ((CEntityV $t $v) $_))
                      $ingred))
        ''')
        # REM: utils.interpret will not work here, because
        # utils.space doesn't contain equalities for `mine` and `craft`
        output = kb.interpret('(mine (CBlockV log oak) $_)')
        self.assertEqual(repr(output[0]), '(CEntityV log oak)')
        output = kb.interpret('(craft (list ((CEntityV log oak) $_)))')
        self.assertEqual(repr(output[0]), '((CEntityV planks oak) 4)')
        # REM: interpretation is done until end, because
        # `match &kb` switches the context, so equalities for `mine` and `craft`
        # are found even we start with `utils` - not `kb`
        output = utils.interpret('(get-mine-block log oak)')
        self.assertEqual(repr(output[0]), '(CBlockV log oak)')
        output = utils.interpret('(get-mine-block cobblestone)')
        self.assertEqual(repr(output[0]), '(CBlockV stone stone)')
        output = utils.interpret('(get-ingredients planks oak)')
        self.assertEqual(repr(output[0]), '(list ((CEntityV log oak) 1))')
        output = utils.interpret('(get-ingredients wooden_pickaxe)')
        self.assertEqual(repr(output[0]), '(list ((CEntityT stick) 2) ((CEntityV planks $_) 3))')


if __name__ == "__main__":
    unittest.main()
