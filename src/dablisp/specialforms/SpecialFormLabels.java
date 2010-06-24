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

import dablisp.CompileEnv;
import dablisp.CompiledCode;
import dablisp.Compiler;
import dablisp.FnCompiledDef;
import dablisp.Obj;
import dablisp.Symbol;
import dablisp.VM;

public class SpecialFormLabels extends SpecialForm {

    @Override
    public void compile(Obj args, CompiledCode code, VM vm, CompileEnv env) {
        // (labels ((fn1 lambdalist1 fnbody1) (fn2 lambdalist2 body2)) body)
        checkArgsMin(args, 1);
        Obj o = args.first();
        code.emitNewFEnv();
        env = CompileEnv.extendFEnv(env);
        while (o != Symbol.NIL) {
            Obj def = o.first();
            Symbol name = def.first().asSymbol();
            FnCompiledDef fn = new FnCompiledDef(name, def.second(), def.asCons().nthCons(2), vm, env);
            code.emitConst(fn);
            code.emitSetEnv();
            code.emitFbind(name);
            env.fbind(name);
            o = o.rest();
        }
        Compiler.compileProgn(args.rest(), code, vm, env);
        code.emitPopFEnv();
    }

}
