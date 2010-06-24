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
package dablisp.fn;

import dablisp.CompileEnv;
import dablisp.CompiledCode;
import dablisp.Compiler;
import dablisp.Env;
import dablisp.Fn;
import dablisp.Lisp;
import dablisp.MacroExpander;
import dablisp.Obj;
import dablisp.Stack;
import dablisp.VM;

public class FnEval extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCount(argc, 1);
        Obj expr = stack.pop();
        VM oldvm = Lisp.vm;
        Lisp.vm = new VM();
        Lisp.vm.denv = oldvm.denv;
        Lisp.vm.rootFEnv = oldvm.rootFEnv;
        Lisp.vm.fenv = oldvm.rootFEnv;
        Lisp.vm.nullEnv = oldvm.nullEnv;
        Lisp.vm.env = oldvm.nullEnv;
        Lisp.vm.code = new CompiledCode();
        expr = MacroExpander.expand(expr, Lisp.vm);
        CompileEnv cenv = CompileEnv.createRootEnv(Lisp.vm.denv.symbols(), Lisp.vm.fenv.symbols());
        CompiledCode code = Compiler.compile(expr, Lisp.vm, cenv);
        Obj result = Lisp.vm.run(code);
        Lisp.vm = oldvm;
        stack.push(result);
    }

    @Override
    public String getName() {
        return "eval";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " form) - function -  evaluates the form and returns its value";
    }

}
