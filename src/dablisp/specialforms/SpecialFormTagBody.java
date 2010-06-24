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
import dablisp.Obj;
import dablisp.Symbol;
import dablisp.TagBody;
import dablisp.VM;

public class SpecialFormTagBody extends SpecialForm {

    @Override
    public void compile(Obj args, CompiledCode code, VM vm, CompileEnv env) {
        // (tagbody &rest tags-and-forms)
        if (args != Symbol.NIL) {
            TagBody tagbody = new TagBody();
            env.addTagBody(tagbody);
            code.emitAddTagBody(tagbody);
            Obj o = args;
            while (o != Symbol.NIL) {
                if (o.first().isSymbol() || o.first().isIntNum()) {
                    tagbody.addTarget(o.first(), code.createLabel());
                }
                o = o.rest();
            }
            while (args != Symbol.NIL) {
                if (args.first().isCons()) {
                    Compiler.compile(args.first(), code, vm, env);
                    code.emitPop();
                } else if (args.first().isSymbol() || args.first().isIntNum()) {
                    code.emitLabel(tagbody.getTarget(args.first()));
                }
                args = args.rest();
            }
            code.emitConst(Symbol.NIL);
        }
    }

}
