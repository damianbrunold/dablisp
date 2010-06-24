/*  dablisp - a kind of common lisp subset written in java
    Copyright 2008 Damian Brunold

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
package dablisp.specialforms;

import dablisp.*;

public class SpecialFormFunction extends SpecialForm {

    @Override
    public void compile(Obj args, CompiledCode code, VM vm, CompileEnv env) {
        // (function x)
        checkArgs(args, 1, 1);
        if (args.first().isSymbol()) {
            code.emitFVar(args.first().asSymbol());
        } else if (args.first().isCons() && args.first().first() == Symbol.LAMBDA) {
            // (lambda lambdalist &rest body)
            Obj def = args.first();
            FnCompiledDef fn = new FnCompiledDef(Symbol.intern("#:anon"), def.second(), def.asCons().nthCons(2), vm, env);
            code.emitConst(fn);
            code.emitSetEnv();
        } else {
            throw new LispException(Symbol.GENERALCONDITION, "function expects symbol or lambda expression but got " + args.first());
        }
    }

}
