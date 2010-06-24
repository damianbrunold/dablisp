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

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

public class VM {

    public Set<Symbol> constants = new HashSet<Symbol>();

    public Env nullEnv = Env.createRootEnv();
    public Env rootFEnv = Env.createRootEnv();

    public DEnv denv = new DEnv();

    public State state = new State(nullEnv, rootFEnv, 0, 0, null, null, 0, null);
    public Env env = nullEnv;
    public Env fenv = rootFEnv;
    public int ip;
    public int reg;
    public Fn fn;
    public CompiledCode code;
    public LinkedList<Handler> handlers = null;

    public Stack stack = new Stack();
    public LinkedList<State> controlstack = new LinkedList<State>();

    public Obj run(CompiledCode code) {
        this.code = code;
        return run();
    }

    public Obj run() {
        try {
            ip = 0;
            if (Lisp.vm.denv.get(Symbol.intern("*vm-trace*")) != Symbol.NIL) {
                while (ip < code.ops.length) {
                  System.out.print(ip + " " + code.ops[ip] + " " + (code.args[ip] != null ? code.args[ip] : ""));
                  execute();
                  System.out.println(" [" + stack + "]");
              }
            } else {
                while (ip < code.ops.length) {
                    execute();
                }
            }
            if (stack.size() != 1) {
                String msg = "stack error: " + stack;
                stack.clear();
                // TODO real condition? at least handle controlstack correctly
                throw new LispException(Symbol.GENERALCONDITION, msg);
            }
            return stack.pop();
        } catch (LispException e) {
            stack.clear();
            controlstack.clear();
            env = nullEnv;
            fenv = rootFEnv;
            //denv.clear(); // TODO
            throw e;
        }
    }

    public void execute() {
        switch (code.ops[ip]) {
        case CONST: executeConst(); break;
        case ARGC: executeArgc(); break;
        case POP: executePop(); break;
        case DUP: executeDup(); break;
        case SWAP: executeSwap(); break;
        case BUBBLE: executeBubble(); break;
        case INC: executeInc(); break;
        case DEC: executeDec(); break;
        case VAR: executeVar(); break;
        case DVAR: executeDvar(); break;
        case FVAR: executeFvar(); break;
        case SETVAR: executeSetVar(); break;
        case SETDVAR: executeSetDvar(); break;
        case SETFVAR: executeSetFvar(); break;
        case SETENV: executeSetEnv(); break;
        case BIND: executeBind(); break;
        case FBIND: executeFbind(); break;
        case DBIND: executeDbind(); break;
        case JUMP: executeJump(); break;
        case JUMPF: executeJumpF(); break;
        case JUMPT: executeJumpT(); break;
        case LABEL: executeLabel(); break;
        case NEWENV: executeNewEnv(); break;
        case POPENV: executePopEnv(); break;
        case NEWFENV: executeNewFEnv(); break;
        case POPFENV: executePopFEnv(); break;
        case CALL: executeCall(); break;
        case FCALL: executeFCall(); break;
        case RETURN: executeReturn(); break;
        case THROW: executeThrow(); break;
        case ADDCATCHER: executeAddCatcher(); break;
        case ADDUNWINDPROTECT: executeAddUnwindProtect(); break;
        case ADDBLOCK: executeAddBlock(); break;
        case RETURNFROM: executeReturnFrom(); break;
        case ADDTAGBODY: executeAddTagBody(); break;
        case GO: executeGo(); break;
        case POPHANDLER: executePopHandler(); break;
        case SUB: executeSub(); break;
        case SUBRET: executeSubRet(); break;
        case SUBRETV: executeSubRetV(); break;
        case BRETV: executeBRetV(); break;
        case PUSHREG: executePushReg(); break;
        case POPREG: executePopReg(); break;
        case PEEKREG: executePeekReg(); break;
        case INCREG: executeIncReg(); break;
        case DECREG: executeDecReg(); break;
        case DECZREG: executeDecZReg(); break;
        case DEC2ZREG: executeDec2ZReg(); break;
        case JUMPZREG: executeJumpZReg(); break;
        case SUBZREG: executeSubZReg(); break;
        case JUMPNZREG: executeJumpNZReg(); break;
        case RESTLIST: executeRestList(); break;
        case KWVAL: executeKwVal(); break;
        case SPLITLIST: executeSplitList(); break;
        case MVALSPLIT: executeMvalSplit(); break;
        case ERROR: executeError(); break;
        case MINARGS: executeMinArgs(); break;
        case CHECKNOARGS: executeCheckNoArgs(); break;
        case CAR: executeCar(); break;
        case CDR: executeCdr(); break;
        case CONS: executeCons(); break;
        case NUMEQ: executeNumEq(); break;
        case EQ: executeEq(); break;
        default: throw new RuntimeException("unknown opcode: " + code.ops[ip] + " at ip " + ip);
        }
    }

    /**
     * Pushes the argument on the stack
     */
    private void executeConst() {
        stack.push(code.args[ip]);
        ip++;
    }

    /**
     * Pushes the argument (signifying the number of fnargs) on the stack
     */
    private void executeArgc() {
        stack.push(code.args[ip]);
        ip++;
    }

    /**
     * Pops the top of the stack
     */
    private void executePop() {
        stack.pop();
        ip++;
    }

    /**
     * Duplicates the top of the stack
     */
    private void executeDup() {
        stack.push(stack.peek());
        ip++;
    }

    /**
     * Swaps the two topmost stack elements
     */
    private void executeSwap() {
        Obj o1 = stack.pop();
        Obj o2 = stack.peek();
        stack.replace(o1);
        stack.push(o2);
        ip++;
    }

    /**
     * Pops a number from the stack and if this number is non-zero
     * extracts this element of the stack and pushes it on the top
     * of the stack (thus 'bubbling' up this element)
     */
    private void executeBubble() {
        int n = (int) stack.pop().asIntNum().intValue();
        if (n != 0) {
            Obj o = stack.peek(n);
            stack.remove(n, 1);
            stack.push(o);
        }
        ip++;
    }

    /**
     * Increments the number on the top of the stack
     */
    private void executeInc() {
        stack.replace(stack.peek().asNumber().add(IntNum.ONE));
        ip++;
    }

    /**
     * Decrements the number on the top of the stack
     */
    private void executeDec() {
        stack.replace(stack.peek().asNumber().subtract(IntNum.ONE));
        ip++;
    }

    /**
     * Resolves the symbol on the top of the stack in the current lexical environment
     * and pushes the value on the stack
     */
    private void executeVar() {
        stack.push(env.get(code.args[ip].asSymbol()));
        ip++;
    }

    /**
     * Resolves the symbol on the top of the stack in the current dynamic environment
     * and pushes the value on the stack
     */
    private void executeDvar() {
        stack.push(denv.get(code.args[ip].asSymbol()));
        ip++;
    }

    /**
     * Resolves the symbol on the top of the stack in the current lexical functional environment
     * and pushes the value on the stack
     */
    private void executeFvar() {
        stack.push(fenv.get(code.args[ip].asSymbol()));
        ip++;
    }

    private void executeSetVar() {
        env.set(code.args[ip].asSymbol(), stack.pop());
        ip++;
    }

    private void executeSetDvar() {
        denv.set(code.args[ip].asSymbol(), stack.pop());
        ip++;
    }

    private void executeSetFvar() {
        fenv.set(code.args[ip].asSymbol(), stack.pop());
        ip++;
    }

    private void executeBind() {
        env.bind(code.args[ip].asSymbol(), stack.pop());
        ip++;
    }

    private void executeDbind() {
        denv.bind(code.args[ip].asSymbol(), stack.pop());
        ip++;
    }

    private void executeFbind() {
        fenv.bind(code.args[ip].asSymbol(), stack.pop().asFn());
        ip++;
    }

    private void executeSetEnv() {
        stack.peek().asFn().setEnv(env, fenv);
        ip++;
    }

    private void executeJump() {
        ip = code.targets[ip];
    }

    private void executeJumpT() {
        if (stack.pop() != Symbol.NIL) {
            ip = code.targets[ip];
        } else {
            ip++;
        }
    }

    private void executeJumpF() {
        if (stack.pop() == Symbol.NIL) {
            ip = code.targets[ip];
        } else {
            ip++;
        }
    }

    private void executeLabel() {
        ip++;
    }

    private void executeNewEnv() {
        env = Env.extend(env);
        denv.mark();
        ip++;
    }

    private void executePopEnv() {
        env = env.discardEnv();
        denv.restore();
        ip++;
    }

    private void executeNewFEnv() {
        fenv = Env.extend(fenv);
        ip++;
    }

    private void executePopFEnv() {
        fenv = fenv.discardEnv();
        ip++;
    }

    private void executeCall() {
        Fn newfn = stack.pop().asFn();
        ip++; // FIRST increment ip
        if (newfn.isPrimitive()) {
            int argc = (int) stack.pop().asIntNum().intValue();
            newfn.applyfn(argc, stack, nullEnv, rootFEnv);
        } else {
            pushControlStack((FnCompiledDef) newfn);
        }
    }

    private void executeFCall() {
        Fn newfn = fenv.get(code.args[ip].asSymbol()).asFn();
        ip++; // FIRST increment ip
        if (newfn.isPrimitive()) {
            int argc = stack.popInt();
            newfn.applyfn(argc, stack, nullEnv, rootFEnv);
        } else {
            pushControlStack((FnCompiledDef) newfn);
        }
    }

    public void pushControlStack(FnCompiledDef newfn) {
        Lisp.enterTrace(newfn, stack);
        controlstack.push(new State(env, fenv, ip, reg, fn, code, denv.getLevel(), handlers));
        handlers = null;
        env = Env.extend(newfn.lexEnv);
        fenv = Env.extend(newfn.lexFEnv);
        ip = 0;
        fn = newfn;
        code = newfn.code;
    }

    private void popControlStack() {
        State state = controlstack.pop();
        handlers = state.handlers;
        env = state.env;
        fenv = state.fenv;
        ip = state.ip;
        reg = state.reg;
        fn = state.fn;
        code = state.code;
        denv.setLevel(state.denvlevel);
    }

    private void executeReturn() {
        Lisp.exitTrace(fn, stack);
        popControlStack();
    }

    private Catcher getCatcher(Obj tag, LinkedList<Handler> handlers) {
        if (handlers == null) return null;
        for (Handler handler : handlers) {
            if (handler.isCatcher() && handler.asCatcher().tag == tag) return handler.asCatcher();
        }
        return null;
    }

    private Catcher getCatcher(Obj tag) {
        Catcher catcher = getCatcher(tag, handlers);
        if (catcher != null) return catcher;
        for (int i = 0; i < controlstack.size(); i++) {
            catcher = getCatcher(tag, controlstack.get(i).handlers);
            if (catcher != null) return catcher;
        }
        return null;
    }

    private LinkedList<Handler> removeSubCatchers(Catcher catcher, LinkedList<Handler> handlers) {
        if (handlers == null) return null;
        for (int i = 0; i < handlers.size(); i++) {
            Handler h = handlers.get(i);
            if (h.isCatcher()) {
                Catcher c = h.asCatcher();
                if (c == catcher) {
                    break;
                } else {
                    handlers.remove(i);
                    i--;
                }
            }
        }
        if (handlers.isEmpty()) return null;
        return handlers;
    }

    private void removeSubCatchers(Catcher catcher) {
        handlers = removeSubCatchers(catcher, handlers);
        if (handlers == null) {
            for (int i = 0; i < controlstack.size(); i++) {
                State s = controlstack.get(i);
                s.handlers = removeSubCatchers(catcher, s.handlers);
                if (s.handlers != null) return;
            }
        }
    }

    private void executeThrow() {
        Obj result = stack.pop();
        Obj tag = stack.pop();
        Catcher catcher = getCatcher(tag);
        if (catcher == null) throw new LispException(tag, result);
        removeSubCatchers(catcher);
        while (true) {
            while (handlers != null) {
                Handler handler = popHandler();
                if (handler.isCatcher() && handler == catcher) {
                    stack.setPointer(catcher.stackpointer);
                    stack.push(result);
                    ip = catcher.target;
                    return;
                } else if (handler.isUnwindProtect()) {
                    stack.push(tag);
                    stack.push(result);
                    stack.push(IntNum.fromLong(ip));
                    ip = handler.asUnwindProtect().sub;
                    return;
                }
            }
            popControlStack();
        }
    }

    private void addHandler(Handler handler) {
        if (handlers == null) handlers = new LinkedList<Handler>();
        handlers.push(handler);
    }

    private Handler popHandler() {
        Handler handler = handlers.pop();
        if (handlers.isEmpty()) handlers = null;
        return handler;
    }

    private void executeAddCatcher() {
        Obj tag = stack.pop();
        int target = code.targets[ip];
        addHandler(new Catcher(tag, target, stack.getPointer()));
        ip++;
    }

    private void executeAddUnwindProtect() {
        int target = code.targets[ip];
        addHandler(new UnwindProtect(target));
        ip++;
    }

    private void executeAddBlock() {
        Symbol name = code.args[ip].asSymbol();
        int target = code.targets[ip];
        Block block = new Block(name, target, stack.getPointer(), env, fenv, denv.getLevel());
        addHandler(block);
        ip++;
    }

    private void executeReturnFrom() {
        Symbol name = code.args[ip].asSymbol();
        int resultip = code.targets[ip];
        while (true) {
            while (handlers != null) {
                Handler handler = popHandler();
                if (handler.isBlock() && handler.asBlock().name == name) {
                    Block block = handler.asBlock();
                    stack.setPointer(block.stackpointer);
                    if (resultip != -1) {
                        addHandler(handler);
                        stack.push(IntNum.fromLong(block.target));
                        ip = resultip;
                    } else {
                        env = block.env;
                        fenv = block.fenv;
                        denv.setLevel(block.denvlevel);
                        stack.push(Symbol.NIL);
                        ip = block.target;
                    }
                    return;
                } else if (handler.isUnwindProtect()) {
                    stack.push(name);
                    stack.push(IntNum.fromLong(ip));
                    ip = handler.asUnwindProtect().sub;
                    return;
                }
            }
            popControlStack();
        }
    }

    private void executeBRetV() {
        Symbol blockname = code.args[ip].asSymbol();
        Block block = null;
        while (handlers != null) {
            Handler handler = popHandler();
            if (handler.isBlock() && handler.asBlock().name == blockname) {
                block = handler.asBlock();
                break;
            }
        }
        if (block == null) throw new LispException(Symbol.GENERALCONDITION, "block " + blockname + " not found");
        Obj result = stack.pop();
        ip = (int) stack.pop().asIntNum().intValue();
        stack.push(result);
        env = block.env;
        fenv = block.fenv;
        denv.setLevel(block.denvlevel);
    }

    private void executeAddTagBody() {
        TagBody tagbody = code.args[ip].asTagBody();
        tagbody.stackpointer = stack.getPointer();
        addHandler(tagbody);
        ip++;
    }

    private void executeGo() {
        Obj tag = code.args[ip];
        while (true) {
            while (handlers != null) {
                Handler handler = popHandler();
                if (handler.isTagBody()) {
                    TagBody tagbody = handler.asTagBody();
                    int target = tagbody.getTarget(tag);
                    if (target != -1) {
                        stack.setPointer(tagbody.stackpointer);
                        addHandler(handler);
                        ip = target;
                        return;
                    }
                } else if (handler.isUnwindProtect()) {
                    stack.push(IntNum.fromLong(ip));
                    ip = handler.asUnwindProtect().sub;
                    return;
                }
            }
            popControlStack();
        }
    }

    private void executePopHandler() {
        popHandler();
        ip++;
    }

    private void executeSub() {
        stack.push(IntNum.fromLong(ip + 1));
        ip = code.targets[ip];
    }

    private void executeSubRet() {
        stack.pop();
        ip = (int) stack.pop().asIntNum().intValue();
    }

    private void executeSubRetV() {
        Obj result = stack.pop();
        ip = (int) stack.pop().asIntNum().intValue();
        stack.push(result);
    }

    private void executePushReg() {
        stack.push(IntNum.fromLong(reg));
        ip++;
    }

    private void executePopReg() {
        reg = (int) stack.pop().asIntNum().intValue();
        ip++;
    }

    private void executePeekReg() {
        reg = (int) stack.peek().asIntNum().intValue();
        ip++;
    }

    private void executeIncReg() {
        reg++;
        ip++;
    }

    private void executeDecReg() {
        reg -= code.args[ip].asIntNum().intValue();
        ip++;
    }

    private void executeDecZReg() {
        if (reg > 0) reg--;
        ip++;
    }

    private void executeDec2ZReg() {
        reg -= 2;
        if (reg < 0) reg = 0;
        ip++;
    }

    private void executeJumpZReg() {
        if (reg == 0) {
            ip = code.targets[ip];
        } else {
            ip++;
        }
    }

    private void executeSubZReg() {
        if (reg == 0) {
            stack.push(IntNum.fromLong(ip + 1));
            ip = code.targets[ip];
        } else {
            ip++;
        }
    }

    private void executeJumpNZReg() {
        if (reg != 0) {
            ip = code.targets[ip];
        } else {
            ip++;
        }
    }

    private void executeRestList() {
        int count = (int) stack.pop().asIntNum().intValue();
        Cons sentinel = new Cons(Symbol.NIL, Symbol.NIL);
        Cons c = sentinel;
        for (int i = 0; i < count; i++) {
            Cons d = new Cons(stack.peek(i), Symbol.NIL);
            c.setCdr(d);
            c = d;
        }
        stack.push(sentinel.rest());
        ip++;
    }

    private void executeKwVal() {
        Symbol kw = code.args[ip].asSymbol();
        int defaultTarget = code.targets[ip];
        for (int i = 0; i < reg; i += 2) {
            Symbol s = stack.peek(i).asSymbol();
            if (s == kw) {
                Obj val = stack.peek(i + 1);
                stack.remove(i, 2);
                stack.push(val);
                reg -= 2;
                ip++;
                return;
            }
        }
        stack.push(IntNum.fromLong(ip + 1));
        ip = defaultTarget;
    }

    private void executeSplitList() {
        Obj list = stack.pop();
        LinkedList<Obj> temp = new LinkedList<Obj>();
        while (list != Symbol.NIL) {
            temp.push(list.first());
            list = list.rest();
        }
        for (Obj obj : temp) {
            stack.push(obj);
        }
        stack.push(IntNum.fromLong(temp.size()));
        ip++;
    }

    private void executeMvalSplit() {
        Obj val = stack.pop();
        int count = (int) stack.pop().asIntNum().intValue();
        for (int i = val.size() - 1; i >= 0; i--) {
            stack.push(val.val(i));
        }
        stack.push(IntNum.fromLong(count + val.size()));
        ip++;
    }

    private void executeError() {
        throw new LispException(Symbol.GENERALCONDITION, code.args[ip]);
    }

    private void executeMinArgs() {
        if (reg < code.args[ip].asIntNum().intValue()) {
            throw new LispException(Symbol.GENERALCONDITION, "Not enough arguments, got " + reg + " but need " + code.args[ip].asIntNum());
        }
        ip++;
    }

    private void executeCheckNoArgs() {
        if (reg > 0) {
            throw new LispException(Symbol.GENERALCONDITION, "Too many arguments, " + reg + " arguments are unused");
        }
        ip++;
    }

    /**
     * pops a stack entry and pushes its CAR on the stack
     */
    private void executeCar() {
        stack.replace(stack.peek().asCons().getCar());
        ip++;
    }

    /**
     * pops a stack entry and pushes its CDR on the stack
     */
    private void executeCdr() {
        stack.replace(stack.peek().asCons().getCdr());
        ip++;
    }

    /**
     * pushes the cons of the two top stack entries on the stack
     */
    private void executeCons() {
        Obj car = stack.pop();
        Obj cdr = stack.peek();
        stack.replace(new Cons(car, cdr));
        ip++;
    }

    /**
     * Compares the two top stack entries for = and pushes T or NIL on the stack
     */
    private void executeNumEq() {
        Number a = stack.pop().asNumber();
        Number b = stack.peek().asNumber();
        if (a.eql(b)) {
            stack.replace(Symbol.T);
        } else {
            stack.replace(Symbol.NIL);
        }
        ip++;
    }

    /**
     * Compares the two top stack entries for EQ and pushes T or NIL on the stack
     */
    private void executeEq() {
        Obj a = stack.pop();
        Obj b = stack.peek();
        if (a == b) {
            stack.replace(Symbol.T);
        } else {
            stack.replace(Symbol.NIL);
        }
        ip++;
    }

}
