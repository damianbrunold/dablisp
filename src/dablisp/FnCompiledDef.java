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
package dablisp;

public class FnCompiledDef extends Fn {

    protected Symbol name;
    protected Obj lambdalist;
    protected String docstring;
    public Env lexEnv;
    public Env lexFEnv;
    public CompiledCode code;

    public FnCompiledDef(Symbol name, Obj lambdalist, Obj body, VM vm, CompileEnv env) {
        this(name, lambdalist, false, body, vm, env);
    }

    protected FnCompiledDef(Symbol name, Obj lambdalist, boolean destructure, Obj body, VM vm, CompileEnv env) {
        this.name = name;
        this.lambdalist = lambdalist;
        if (body != Symbol.NIL && body.first().isStr() && body.rest() != Symbol.NIL) {
            this.docstring = body.first().asStr().getStrValue();
            body = body.rest();
        } else {
            this.docstring = name.getName();
        }
        code = new CompiledCode();
        Compiler.compileLambdalist(name, lambdalist, destructure, code, vm, env);
        Compiler.compileBlock(name, body, code, vm, env);
        code.emitReturn();
        code.resolveAddresses();
        code.optimize();
        code.publish();
    }

    public void setName(Symbol name) {
        this.name = name;
    }

    @Override
    public void setEnv(Env env, Env fenv) {
        lexEnv = env;
        lexFEnv = fenv;
    }

    @Override
    public boolean isPrimitive() {
        return false;
    }

    @Override
    public String getName() {
        return name != null ? name.getName() : "";
    }

    @Override
    public String getDocstring() {
        return new Cons(name, lambdalist) + " - function -  " + docstring;
    }

    @Override
    public void applyfn(int argc, Stack stack, Env env, Env fenv) {
        apply(argc, stack, env, fenv);
    }

    @Override
    public void apply(int argc, Stack stack, Env dynenv, Env dynfenv) {
        stack.push(IntNum.fromLong(argc));
        Lisp.vm.pushControlStack(this);
    }

}
