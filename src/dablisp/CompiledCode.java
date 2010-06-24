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

import java.util.ArrayList;
import java.util.List;

public class CompiledCode {

    public OpCode[] ops = new OpCode[0];
    public Obj[] args;
    public int[] targets;

    private List<OpCode> tempops = new ArrayList<OpCode>();
    private List<Obj> tempargs = new ArrayList<Obj>();
    private List<Integer> temptargets = new ArrayList<Integer>();
    private int[] labels;

    private static int NEXT_LABEL = 1;

    public int createLabel() {
        return NEXT_LABEL++;
    }

    public int getLabelAddress(int label) {
        return labels[label];
    }

    public void emitOperation(OpCode op) {
        emitOperation(op, null, -1);
    }

    public void emitOperation(OpCode op, int target) {
        emitOperation(op, null, target);
    }

    public void emitOperation(OpCode op, Obj arg) {
        emitOperation(op, arg, -1);
    }

    public void emitOperation(OpCode op, Obj arg, int target) {
        if (tempops.size() != tempargs.size() || tempops.size() != temptargets.size()) throw new RuntimeException("corrupted compiled code");
        tempops.add(op);
        tempargs.add(arg);
        temptargets.add(target);
    }

    public void emitNewEnv() {
        emitOperation(OpCode.NEWENV);
    }

    public void emitPopEnv() {
        emitOperation(OpCode.POPENV);
    }

    public void emitNewFEnv() {
        emitOperation(OpCode.NEWFENV);
    }

    public void emitPopFEnv() {
        emitOperation(OpCode.POPFENV);
    }

    public void emitConst(Obj constant) {
        emitOperation(OpCode.CONST, constant);
    }

    public void emitConst(int i) {
        emitConst(IntNum.fromLong(i));
    }

    public void emitArgc(int argc) {
        emitOperation(OpCode.ARGC, IntNum.fromLong(argc));
    }

    public void emitVar(Symbol s) {
        emitOperation(OpCode.VAR, s);
    }

    public void emitFVar(Symbol s) {
        emitOperation(OpCode.FVAR, s);
    }

    public void emitDVar(Symbol s) {
        emitOperation(OpCode.DVAR, s);
    }

    public void emitSetVar(Symbol s) {
        emitOperation(OpCode.SETVAR, s);
    }

    public void emitSetFVar(Symbol s) {
        emitOperation(OpCode.SETFVAR, s);
    }

    public void emitSetDVar(Symbol s) {
        emitOperation(OpCode.SETDVAR, s);
    }

    public void emitCall() {
        emitOperation(OpCode.CALL);
    }

    public void emitFCall(Symbol fn) {
        emitOperation(OpCode.FCALL, fn);
    }

    public void emitJump(int targetLabel) {
        emitOperation(OpCode.JUMP, targetLabel);
    }

    public void emitJumpF(int targetLabel) {
        emitOperation(OpCode.JUMPF, targetLabel);
    }

    public void emitJumpT(int targetLabel) {
        emitOperation(OpCode.JUMPT, targetLabel);
    }

    public int emitLabel() {
        int label = createLabel();
        emitLabel(label);
        return label;
    }

    public void emitLabel(int label) {
        emitOperation(OpCode.LABEL, label);
    }

    public void emitPop() {
        emitOperation(OpCode.POP);
    }

    public void emitDup() {
        emitOperation(OpCode.DUP);
    }

    public void emitSwap() {
        emitOperation(OpCode.SWAP);
    }

    public void emitBubble() {
        emitOperation(OpCode.BUBBLE);
    }

    public void emitInc() {
        emitOperation(OpCode.INC);
    }

    public void emitDec() {
        emitOperation(OpCode.DEC);
    }

    public void emitBind(Symbol s) {
        emitOperation(OpCode.BIND, s);
    }

    public void emitDBind(Symbol s) {
        emitOperation(OpCode.DBIND, s);
    }

    public void emitFbind(Symbol s) {
        emitOperation(OpCode.FBIND, s);
    }

    public void emitReturn() {
        emitOperation(OpCode.RETURN);
    }

    public void emitThrow() {
        emitOperation(OpCode.THROW);
    }

    public void emitAddCatcher(int targetLabel) {
        emitOperation(OpCode.ADDCATCHER, targetLabel);
    }

    public void emitAddUnwindProtect(int cleanupLabel) {
        emitOperation(OpCode.ADDUNWINDPROTECT, cleanupLabel);
    }

    public void emitAddBlock(Symbol name, int targetLabel) {
        emitOperation(OpCode.ADDBLOCK, name, targetLabel);
    }

    public void emitReturnFrom(Symbol tag, int resultLabel) {
        emitOperation(OpCode.RETURNFROM, tag, resultLabel);
    }

    public void emitAddTagBody(TagBody tagbody) {
        emitOperation(OpCode.ADDTAGBODY, tagbody);
    }

    public void emitGo(Obj tag) {
        emitOperation(OpCode.GO, tag);
    }

    public void emitPopHandler() {
        emitOperation(OpCode.POPHANDLER);
    }

    public void emitSub(int targetLabel) {
        emitOperation(OpCode.SUB, targetLabel);
    }

    public void emitSubRet() {
        emitOperation(OpCode.SUBRET);
    }

    public void emitSubRetV() {
        emitOperation(OpCode.SUBRETV);
    }

    public void emitBRetV(Symbol block) {
        emitOperation(OpCode.SUBRETV, block);
    }

    public void emitSetEnv() {
        emitOperation(OpCode.SETENV);
    }

    public void emitPushReg() {
        emitOperation(OpCode.PUSHREG);
    }

    public void emitPopReg() {
        emitOperation(OpCode.POPREG);
    }

    public void emitPeekReg() {
        emitOperation(OpCode.PEEKREG);
    }

    public void emitIncReg() {
        emitOperation(OpCode.INCREG);
    }

    public void emitDecReg(int count) {
        emitOperation(OpCode.DECREG, IntNum.fromLong(count));
    }

    public void emitDecZReg() {
        emitOperation(OpCode.DECZREG);
    }

    public void emitDec2ZReg() {
        emitOperation(OpCode.DEC2ZREG);
    }

    public void emitJumpZReg(int targetLabel) {
        emitOperation(OpCode.JUMPZREG, targetLabel);
    }

    public void emitSubZReg(int targetLabel) {
        emitOperation(OpCode.SUBZREG, targetLabel);
    }

    public void emitJumpNZReg(int targetLabel) {
        emitOperation(OpCode.JUMPNZREG, targetLabel);
    }

    public void emitRestList() {
        emitOperation(OpCode.RESTLIST);
    }

    public void emitKwVal(Symbol kw, int defaultLabel) {
        emitOperation(OpCode.KWVAL, kw, defaultLabel);
    }

    public void emitSplitList() {
        emitOperation(OpCode.SPLITLIST);
    }

    public void emitMvalSplit() {
        emitOperation(OpCode.MVALSPLIT);
    }

    public void emitMinArgs(int minargs) {
        emitOperation(OpCode.MINARGS, IntNum.fromLong(minargs));
    }

    public void emitCheckNoArgs() {
        emitOperation(OpCode.CHECKNOARGS);
    }

    public void emitError(Obj msg) {
        emitOperation(OpCode.ERROR, msg);
    }

    public void emitCar() {
        emitOperation(OpCode.CAR);
    }

    public void emitCdr() {
        emitOperation(OpCode.CDR);
    }

    public void emitCons() {
        emitOperation(OpCode.CONS);
    }

    public void emitNumEq() {
        emitOperation(OpCode.NUMEQ);
    }

    public void emitEq() {
        emitOperation(OpCode.EQ);
    }

    private OpCode getOp(int address) {
        return tempops.get(address);
    }

    private Obj getArg(int address) {
        return tempargs.get(address);
    }

    private int getTarget(int address) {
        return temptargets.get(address);
    }

    private int getSize() {
        return tempops.size();
    }

    private boolean hasAddressReference(int i) {
        return (getOp(i) != OpCode.LABEL && getTarget(i) != -1) || getOp(i) == OpCode.ADDTAGBODY;
    }

    public void resolveAddresses() {
        labels = new int[NEXT_LABEL];
        for (int i = 0; i < getSize(); i++) {
            if (getOp(i) == OpCode.LABEL) {
                labels[getTarget(i)] = i;
                tempops.remove(i);
                tempargs.remove(i);
                temptargets.remove(i);
                i--;
            }
        }
        for (int i = 0; i < getSize(); i++) {
            if (hasAddressReference(i)) {
                if (getOp(i) == OpCode.ADDTAGBODY) {
                    TagBody tagbody = tempargs.get(i).asTagBody();
                    tagbody.resolveAddresses(labels);
                } else {
                    temptargets.set(i, labels[getTarget(i)]);
                }
            }
        }
    }

    public void removeOp(int idx) {
        for (int i = 0; i < getSize(); i++) {
            if (i != idx && hasAddressReference(i)) {
                if (getOp(i) == OpCode.ADDTAGBODY) {
                    TagBody tagbody = getArg(i).asTagBody();
                    tagbody.decAddressesAbove(idx);
                } else {
                    int ref = getTarget(i);
                    if (ref > idx) {
                        temptargets.set(i, ref - 1);
                    }
                }
            }
        }
        tempops.remove(idx);
        tempargs.remove(idx);
        temptargets.remove(idx);
    }

    private boolean isReferenced(int idx) {
        for (int i = 0; i < getSize(); i++) {
            if (i != idx) {
                if (hasAddressReference(i)) {
                    if (getOp(i) == OpCode.ADDTAGBODY) {
                        TagBody tagbody = getArg(i).asTagBody();
                        if (tagbody.references(idx)) return true;
                    } else {
                        if (getTarget(i) == idx) return true;
                    }
                }
            }
        }
        return false;
    }

    public void optimize() {
        resolveMultiJumps();
        optimizeConditionals();
        pullbackJumps();
        removeJumpsToNextIp();
        boolean changed = removeUnreachableCode();
        while (changed) {
            changed = removeUnreachableCode();
        }
        removePopregPushreg();
        removeMultiReturns();
        removeUnusedBlocks();
        removePushPop();
        optimizeDupSetvarPop();
        optimizeConstPopenvPop();
        removeLastJumpToEnd();
    }

    private void optimizeConditionals() {
        for (int i = 0; i < getSize() - 1; i++) {
            if (getOp(i) == OpCode.CONST && getArg(i) == Symbol.NIL && getOp(i + 1) == OpCode.JUMPT) {
                removeOp(i); // const
                removeOp(i); // jumpt
            } else if (getOp(i) == OpCode.CONST && getArg(i) != Symbol.NIL && getOp(i + 1) == OpCode.JUMPF) {
                removeOp(i); // const
                removeOp(i); // jumpf
            }
        }
    }

    private boolean isJump(OpCode op) {
        return op == OpCode.JUMP || op == OpCode.GO ||
               op == OpCode.RETURN || op == OpCode.RETURNFROM ||
               op == OpCode.SUBRET || op == OpCode.SUBRETV || op == OpCode.BRETV ||
               op == OpCode.ERROR;
    }

    private int resolveMultiJump(int i) {
        int target = getTarget(i);
        while (target < getSize() && getOp(target) == OpCode.JUMP) {
            target = getTarget(target);
        }
        return target;
    }

    private void resolveMultiJumps() {
        for (int i = 0; i < getSize(); i++) {
            if (getOp(i) == OpCode.JUMP) {
                int target = resolveMultiJump(i);
                if (target != getTarget(i)) {
                    temptargets.set(i, target);
                }
            }
        }
    }

    private void pullbackJumps() {
        for (int i = 0; i < getSize(); i++) {
            if (getOp(i) == OpCode.JUMP) {
                int target = getTarget(i);
                if (target != getSize()) {
                    OpCode op = getOp(target);
                    if (isJump(op)) {
                        tempops.set(i, op);
                        tempargs.set(i, getArg(target));
                        temptargets.set(i, getTarget(target));
                    }
                }
            }
        }
    }

    private void removeJumpsToNextIp() {
        for (int i = 0; i < getSize(); i++) {
            if (getOp(i) == OpCode.JUMP) {
                if (getTarget(i) == i + 1) {
                    removeOp(i);
                    i--;
                }
            }
        }
    }

    private void removeMultiReturns() {
        for (int i = 0; i < getSize() - 1; i++) {
            if (getOp(i) == OpCode.RETURN && getOp(i + 1) == OpCode.RETURN) {
                removeOp(i);
                i--;
            }
        }
    }

    private boolean removeUnreachableCode() {
        boolean changed = false;
        List<Boolean> unreachable = new ArrayList<Boolean>(getSize());
        boolean lastwasjump = false;
        for (int i = 0; i < getSize(); i++) {
            unreachable.add(false);
            OpCode op = getOp(i);
            if (lastwasjump) {
                if (!isReferenced(i)) {
                    unreachable.set(i, true);
                } else if (!isJump(op)) {
                    lastwasjump = false;
                }
            } else {
                if (isJump(op)) {
                    lastwasjump = true;
                }
            }
        }
        for (int i = 0; i < getSize(); i++) {
            if (unreachable.get(i)) {
                removeOp(i);
                unreachable.remove(i);
                changed = true;
            }
        }
        return changed;
    }

    private void removePushPop() {
        for (int i = 0; i < getSize() - 1; i++) {
            OpCode op = getOp(i);
            if (op == OpCode.CONST || op == OpCode.VAR || op == OpCode.FVAR || op == OpCode.DVAR) {
                OpCode op2 = getOp(i + 1);
                if (op2 == OpCode.POP) {
                    if (!isReferenced(i + 1)) {
                        removeOp(i); // const, var, fvar, dvar
                        removeOp(i); // pop
                        i--;
                    }
                }
            }
        }
    }

    private void removePopregPushreg() {
        for (int i = 0; i < getSize() - 1; i++) {
            OpCode op = getOp(i);
            if (op == OpCode.POPREG) {
                OpCode op2 = getOp(i + 1);
                if (op2 == OpCode.PUSHREG) {
                    if (!isReferenced(i + 1)) {
                        tempops.set(i + 1, OpCode.PEEKREG);
                        removeOp(i); // popreg
                        i--;
                    }
                }
            }
        }
    }

    private void optimizeDupSetvarPop() {
        for (int i = 0; i < getSize() - 2; i++) {
            OpCode op1 = getOp(i);
            OpCode op2 = getOp(i + 1);
            OpCode op3 = getOp(i + 2);
            if (op1 == OpCode.DUP && op2 == OpCode.SETVAR && op3 == OpCode.POP) {
                if (!isReferenced(i) && !isReferenced(i + 1) && !isReferenced(i + 2)) {
                    removeOp(i); // dup
                    removeOp(i + 1); // pop
                }
            }
        }
    }

    private void optimizeConstPopenvPop() {
        for (int i = 0; i < getSize() - 2; i++) {
            OpCode op1 = getOp(i);
            OpCode op2 = getOp(i + 1);
            OpCode op3 = getOp(i + 2);
            if (op1 == OpCode.CONST && op2 == OpCode.POPENV && op3 == OpCode.POP) {
                if (!isReferenced(i + 1) && !isReferenced(i + 2)) {
                    removeOp(i); // const
                    removeOp(i + 1); // pop
                }
            }
        }
    }

    private void removeUnusedBlocks() {
        for (int i = 0; i < getSize(); i++) {
            if (getOp(i) == OpCode.ADDBLOCK) {
                boolean hasReturnFrom = false;
                Symbol name = getArg(i).asSymbol();
                int end = getTarget(i);
                for (int j = i + 1; j < end; j++) {
                    if (getOp(j) == OpCode.RETURNFROM && getArg(j) == name) {
                        hasReturnFrom = true;
                        break;
                    }
                }
                if (!hasReturnFrom) {
                    removeOp(i);
                }
            }
        }
    }

    private void removeLastJumpToEnd() {
        if (getOp(getSize() - 1) == OpCode.JUMP) {
            if (getTarget(getSize() - 1) == getSize()) {
                removeOp(getSize() - 1);
            }
        }
    }

    public void publish() {
        ops = tempops.toArray(new OpCode[0]);
        args = tempargs.toArray(new Obj[0]);
        targets = new int[temptargets.size()];
        for (int i = 0; i < temptargets.size(); i++) {
            targets[i] = temptargets.get(i);
        }
        tempops = null;
        tempargs = null;
        temptargets = null;
    }

    @Override
    public String toString() {
        StringBuilder b = new StringBuilder();
        b.append("#<CompiledCode>\n");
        if (ops == null) {
            for (int i = 0; i < getSize(); i++) {
                b.append(i).append(" ");
                b.append(tempops.get(i));
                if (tempargs.get(i) != null) {
                    b.append(" ");
                    b.append(tempargs.get(i));
                }
                if (temptargets.get(i) != -1) {
                    b.append(" ");
                    b.append(temptargets.get(i));
                }
                b.append("\n");
            }
        } else {
            for (int i = 0; i < ops.length; i++) {
                b.append(i).append(" ");
                b.append(ops[i]);
                if (args[i] != null) {
                    b.append(" ");
                    b.append(args[i]);
                }
                if (targets[i] != -1) {
                    b.append(" ");
                    b.append(targets[i]);
                }
                b.append("\n");
            }
        }
        return b.toString();
    }

}
