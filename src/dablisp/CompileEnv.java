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
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

public class CompileEnv {

    private CompileEnv parent;
    private List<Symbol> vars;
    private List<Symbol> specials;
    private List<Symbol> fvars;
    private LinkedList<TagBody> tagbodies;
    private LinkedList<Symbol> blocks;

    public static CompileEnv createRootEnv(Set<Symbol> denv, Set<Symbol> fenv) {
        CompileEnv env = new CompileEnv();
        env.specials.addAll(denv);
        env.fvars.addAll(fenv);
        return env;
    }

    public static CompileEnv extendEnv(CompileEnv env) {
        return new CompileEnv(env, false);
    }

    public static CompileEnv extendFEnv(CompileEnv env) {
        return new CompileEnv(env, true);
    }

    private CompileEnv() {
        this.parent = null;
        fvars = new ArrayList<Symbol>();
        vars = new ArrayList<Symbol>();
        specials = new ArrayList<Symbol>();
        tagbodies = new LinkedList<TagBody>();
        blocks = new LinkedList<Symbol>();
    }

    private CompileEnv(CompileEnv parent, boolean extendfenv) {
        this.parent = parent;
        if (extendfenv) {
            fvars = new ArrayList<Symbol>();
        } else {
            vars = new ArrayList<Symbol>();
            specials = new ArrayList<Symbol>();
            tagbodies = new LinkedList<TagBody>();
            blocks = new LinkedList<Symbol>();
        }
    }

    public boolean isRootEnv() {
        return parent == null;
    }

    public boolean isBound(Symbol symbol) {
        if (vars != null && vars.contains(symbol)) return true;
        if (parent != null) return parent.isBound(symbol);
        return false;
    }

    public boolean isSpecial(Symbol symbol) {
        if (specials != null && specials.contains(symbol)) return true;
        if (parent != null) return parent.isSpecial(symbol);
        return false;
    }

    public boolean isFuncBound(Symbol symbol) {
        if (fvars != null && fvars.contains(symbol)) return true;
        if (parent != null) return parent.isFuncBound(symbol);
        return false;
    }

    public void bind(Symbol symbol) {
        if (!isBound(symbol)) {
            if (vars != null) {
                vars.add(symbol);
            } else {
                parent.bind(symbol);
            }
        }
    }

    public void sbind(Symbol symbol) {
        if (!isSpecial(symbol)) {
            if (specials != null) {
                specials.add(symbol);
            } else {
                parent.sbind(symbol);
            }
        }
    }

    public void fbind(Symbol symbol) {
        if (!isFuncBound(symbol)) {
            if (fvars != null) {
                fvars.add(symbol);
            } else {
                parent.fbind(symbol);
            }
        }
    }

    public void addTagBody(TagBody tagbody) {
        if (tagbodies != null) {
            tagbodies.push(tagbody);
        } else {
            parent.addTagBody(tagbody);
        }
    }

    public int resolveGo(Obj tag) {
        if (!tag.isSymbol() && !tag.isIntNum()) throw new LispException(Symbol.GENERALCONDITION, "go tag must be symbol or integer");
        if (tagbodies != null) {
            for (TagBody tagbody : tagbodies) {
                int target = tagbody.getTarget(tag);
                if (target != -1) {
                    return target;
                }
            }
        }
        if (parent != null) {
            return parent.resolveGo(tag);
        }
        throw new LispException(Symbol.GENERALCONDITION, "go to undefined tag " + tag);
    }

    public void addBlock(Symbol block) {
        if (blocks != null) {
            blocks.push(block);
        } else {
            parent.addBlock(block);
        }
    }

    public boolean hasBlock(Symbol block) {
        if (blocks != null && blocks.contains(block)) return true;
        if (parent != null) return parent.hasBlock(block);
        return false;
    }

}
