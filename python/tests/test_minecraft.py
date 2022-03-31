import unittest
import re

from hyperon import *
from common import MeTTa

def newInInventory(inventory):
    return OperationAtom(
        "in-inventory",
        lambda obj: [ValueAtom(obj in inventory)],
        unwrap=False)

def craft_op(inventory, obj, where, comp):
    print(str(obj) + " crafted in " + str(where) + " from " + str(comp))
    inventory.append(obj)
    return [obj]

def newCraftOp(inventory):
    return OperationAtom(
        "craft",
        lambda obj, where, comp: craft_op(inventory, obj, where, comp),
        unwrap=False)

def mine_op(inventory, obj, tool):
    print(str(obj) + " mined by " + str(tool))
    inventory.append(obj)
    return [obj]

def newMineOp(inventory):
    return OperationAtom(
        "mine",
        lambda obj, tool: mine_op(inventory, obj, tool),
        unwrap=False)

class MinecraftTest(unittest.TestCase):

    def test_minecraft_planning(self):
        metta = MeTTa()
        inventory = [S('inventory'), S('hands')]
        metta.add_token("in-inventory", lambda _: newInInventory(inventory))
        metta.add_token("craft", lambda _: newCraftOp(inventory))
        metta.add_token("mine", lambda _: newMineOp(inventory))

        metta.add_parse('''
            (= (if True $then $else) $then)
            (= (if False $then $else) $else)

            (= (wood) (spruce-wood))
            (= (spruce-wood) (mine spruce-tree hand))

            (= (four-planks) (craft four-planks inventory (wood)))
            (= (pack $n planks) (if (> $n 0) (allof (four-planks) (pack (- $n 4) planks)) nop))

            (= (crafting-table) (craft crafting-table inventory  (pack 4 planks)))

            (= (stick) (craft stick inventory (pack 2 planks)))
            (= (pack $n sticks) (if (> $n 0) (allof (stick) (pack (- $n 1) sticks)) nop))

            (= (wooden-pickaxe) (craft wooden-pickaxe
                           (crafting-table) (allof (pack 3 planks) (pack 2 sticks))))

            (= (cobblestone) (mine cobble-ore (wooden-pickaxe)))
            (= (pack $n cobblestones) (if (> $n 0) (allof (cobblestone) (pack (- $n 1) cobblestones)) nop))

            (= (stone-pickaxe) (craft stone-pickaxe (crafting-table)
                           (allof (pack 3 cobblestones) (pack 2 sticks))))
        ''')

        self.assertFalse(S('wooden-pickaxe') in inventory)
        metta.interpret('(wooden-pickaxe)')
        self.assertTrue(S('four-planks') in inventory)
        self.assertTrue(S('crafting-table') in inventory)
        self.assertTrue(S('wooden-pickaxe') in inventory)

    def test_minecraft_planning_with_abstractions(self):
        metta = MeTTa()

        inventory = [S('inventory'), S('hands'), S('crafting-table'), S('stick'),
                     S('iron-ingot'), S('iron-pickaxe')]
        metta.add_token("in-inventory", lambda _: newInInventory(inventory))

        metta.add_parse('''
            (= (can-be-mined diamond) True)
            (= (can-be-made diamond) False)
            (= (diamond mined-using iron-pickaxe) True)
            (= (diamond mined-from diamond-ore) True)

            (= (can-be-made iron-pickaxe) True)
            (= (can-be-mined iron-pickaxe) False)
            (= (iron-pickaxe made-from
                (, stick stick iron-ingot iron-ingot iron-ingot)) True)
            (= (iron-pickaxe made-at crafting-table) True)

            (= (can-be-made crafting-table) True)
            (= (can-be-mined crafting-table) False)
            (= (crafting-table made-from (pack 4 plank)) True)
            (= (crafting-table made-at inventory) True)

            (= (can-be-made inventory) False)
            (= (can-be-mined inventory) False)

            (= (if True $then $else) $then)
            (= (if False $then $else) $else)

            (= (make $x) (if (and ($x made-from $comp) ($x made-at $tool))
                             (, (get $tool) (get $comp) (do-make $x $tool $comp)) nop))

            (= (mine $x) (if (and ($x mined-using $tool) ($x mined-from $source))
                             (, (get $tool) (find $source) (do-mine $x $source $tool)) nop))

            (= (get $x) (if (and (not (in-inventory $x)) (can-be-mined $x)) (mine $x) nop))
            (= (get $x) (if (and (not (in-inventory $x)) (can-be-made $x)) (make $x) nop))
        ''')

        metta.interpret('(get diamond)')
        # (, (get iron-pickaxe) (find diamond-ore)
        #    (do-mine diamond diamond-ore iron-pickaxe))

init_logger()
if __name__ == "__main__":
    unittest.main()
