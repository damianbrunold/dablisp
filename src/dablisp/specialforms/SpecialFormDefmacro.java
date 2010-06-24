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

public class SpecialFormDefmacro extends SpecialForm {

    @Override
    public void compile(Obj args, CompiledCode code, VM vm, CompileEnv env) {
        // (defmacro name (x y) body)
        checkArgsMin(args, 2);
        Symbol name = args.first().asSymbol();
        Obj body = MacroExpander.expand(args.asCons().nthCons(2), vm);
        MacroDef macro = new MacroDef(name, args.second(), body, vm, env);
        code.emitConst(macro);
        code.emitSetEnv();
        if (env.isFuncBound(name)) {
            Lisp.getErrorOutput().print();
            Lisp.getErrorOutput().print("WARN: redefining function " + name);
            code.emitSetFVar(name);
        } else {
            code.emitFbind(name);
        }
        code.emitConst(name);
        env.fbind(name);
    }

}
