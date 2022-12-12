from hyperon.atoms import G, AtomType
from hyperon.runner import MeTTa
from hyperon.ext import register_atoms

# This 'extension' is just a way to replace `(import! r r.metta)` with
# a version of `import!` that uses a grounded atom for a runner (instead of space)

# It also demonstrates a possible way to embed `import` into `extend-py`

@register_atoms
def my_imported_runner_atom():
    # We don't use metta here, but we could...
    content = '''
        (: fact (-> Number Number))
        (= (fact $x)
           (case $x
            ((0 1)
             ($_ (* $x (fact (- $x 1)))))
           )
        )
    '''
    runner = MeTTa()
    runner.run(content)
    runnerAtom = G(runner, AtomType.ATOM)
    return {
        'r': runnerAtom
    }
