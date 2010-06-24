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

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import dablisp.CompileEnv;
import dablisp.CompiledCode;
import dablisp.Compiler;
import dablisp.Env;
import dablisp.Fn;
import dablisp.Lexer;
import dablisp.Lisp;
import dablisp.LispException;
import dablisp.MacroExpander;
import dablisp.Obj;
import dablisp.Parser;
import dablisp.ReadStream;
import dablisp.Stack;
import dablisp.Symbol;
import dablisp.Vector;

public class FnLoad extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCount(argc, 1);
        Vector fname = stack.pop().asStr();
        String fn = fname.getStrValue();
        try {
            InputStream strm = null;
            try {
                if (fn.startsWith("!")) {
                    strm = FnLoad.class.getResourceAsStream("/" + fn.substring(1));
                } else {
                    strm = new FileInputStream(fn);
                }
                Lexer lexer = new Lexer(new ReadStream(strm));
                Obj expr = Parser.parse(lexer);
                Obj result = Symbol.NIL;
                while (expr != null) {
                    try {
                        expr = MacroExpander.expand(expr, Lisp.vm);
                        CompileEnv cenv = CompileEnv.createRootEnv(Lisp.vm.denv.symbols(), Lisp.vm.fenv.symbols());
                        CompiledCode code = Compiler.compile(expr, Lisp.vm, cenv);
                        result = Lisp.vm.run(code);
                    } catch (RuntimeException e) {
                        System.out.println("FAIL: " + expr + ": " + e);
                        throw new LispException(Symbol.GENERALCONDITION, "runtime exception", e);
                    }
                    expr = Parser.parse(lexer);
                }
                stack.push(result);
            } finally {
                if (strm != null) strm.close();
            }
        } catch (IOException e) {
            throw new LispException(Symbol.IOCONDITION, e.getMessage());
        }
    }

    @Override
    public String getName() {
        return "load";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " filename) - function -  loads the file filename. If filename starts with ! then the file is loaded from the jarfile";
    }

}
