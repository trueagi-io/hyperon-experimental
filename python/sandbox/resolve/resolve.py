from hyperon.base import Atom
from hyperon.atoms import OperationAtom, E
from hyperon.ext import register_tokens
import hyperonpy as hp


@register_tokens(pass_metta=True)
def my_resolver_atoms(metta):

    def run_resolved_symbol_op(runner, atom, *args):
        expr = E(atom, *args)
        result = hp.metta_evaluate_atom(runner.cmetta, expr.catom)
        result = [Atom._from_catom(catom) for catom in result]
        return result

    def resolve_atom(metta, token):
        # TODO: nested modules...
        runner_name, atom_name = token.split('::')
        # FIXME: using `run` for this is an overkill,
        #        but there is no good Python API for this;
        #        we may have an interface function for
        #        `tokenizer` to resolve individual symbols -
        #        metta.tokenizer().find_token ...
        #        or something else...
        # TODO: assert
        runner = metta.run('! ' + runner_name)[0][0].get_object()
        atom = runner.run('! ' + atom_name)[0][0]
        # A hack to make runner::&self work
        # TODO? the problem is that we need to return an operation to make this
        # work in parent expressions, thus, it is unclear how to return pure
        # symbols
        if atom.get_type() == hp.AtomKind.GROUNDED:
            return atom
        # TODO: borrow atom type to op
        return OperationAtom(
            token,
            lambda *args: run_resolved_symbol_op(runner, atom, *args),
            unwrap=False)

    return {
        r"[^\s]+::[^\s]+": lambda token: resolve_atom(metta, token)
    }
