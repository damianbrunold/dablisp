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

public class Symbol extends Atom implements Comparable<Symbol> {

    public static final Symbol T = intern("t");
    public static final Symbol NIL = intern("nil");
    public static final Symbol QUOTE = intern("quote");
    public static final Symbol BACKQUOTE = intern("backquote");
    public static final Symbol COMMA = intern("comma");
    public static final Symbol COMMAATSIGN = intern("comma-atsign");
    public static final Symbol COMMADOT = intern("comma-dot");
    public static final Symbol DEFUN = intern("defun");
    public static final Symbol DEFMACRO = intern("defmacro");
    public static final Symbol LABELS = intern("labels");
    public static final Symbol FLET = intern("flet");
    public static final Symbol LAMBDA = intern("lambda");
    public static final Symbol IF = intern("if");
    public static final Symbol SETQ = intern("setq");
    public static final Symbol PSETQ = intern("psetq");
    public static final Symbol DEFVAR = intern("defvar");
    public static final Symbol DEFPARAMETER = intern("defparameter");
    public static final Symbol DEFCONSTANT = intern("defconstant");
    public static final Symbol PROGN = intern("progn");
    public static final Symbol FUNCTION = intern("function");
    public static final Symbol FUNCALL = intern("funcall");
    public static final Symbol LET = intern("let");
    public static final Symbol LETSTAR = intern("let*");
    public static final Symbol DO = intern("do");
    public static final Symbol DOSTAR = intern("do*");
    public static final Symbol LLOPTIONAL = intern("&optional");
    public static final Symbol LLREST = intern("&rest");
    public static final Symbol LLBODY = intern("&body");
    public static final Symbol LLKEY = intern("&key");
    public static final Symbol THROW = intern("throw");
    public static final Symbol CATCH = intern("catch");
    public static final Symbol UNWINDPROTECT = intern("unwind-protect");
    public static final Symbol BLOCK = intern("block");
    public static final Symbol RETURNFROM = intern("return-from");
    public static final Symbol MULTIPLEVALUECALL = intern("multiple-value-call");
    public static final Symbol DESTRUCTURINGBIND = intern("destructuring-bind");
    public static final Symbol TAGBODY = intern("tagbody");
    public static final Symbol GO = intern("go");

    public static final Symbol CHAR = intern("char");
    public static final Symbol CHARACTER = intern("character");
    public static final Symbol STRING = intern("string");
    public static final Symbol INTEGER = intern("integer");
    public static final Symbol FLOAT = intern("float");
    public static final Symbol SYMBOL = intern("symbol");
    public static final Symbol KEYWORD = intern("keyword");
    public static final Symbol NUMBER = intern("number");
    public static final Symbol REAL = intern("real");
    public static final Symbol CONS = intern("cons");
    public static final Symbol CAR = intern("car");
    public static final Symbol CDR = intern("cdr");
    public static final Symbol ATOM = intern("atom");
    public static final Symbol LIST = intern("list");
    public static final Symbol NULL = intern("null");
    public static final Symbol STREAM = intern("stream");
    public static final Symbol NATIVE = intern("native");
    public static final Symbol DOUBLE = intern("double");
    public static final Symbol BOOLEAN = intern("boolean");
    public static final Symbol OBJECT = intern("object");
    public static final Symbol SHORT = intern("short");
    public static final Symbol LONG = intern("long");
    public static final Symbol BYTE = intern("byte");
    public static final Symbol HASHTABLE = intern("hashtable");
    public static final Symbol ARRAY = intern("array");
    public static final Symbol VECTOR = intern("vector");
    public static final Symbol SIMPLEVECTOR = intern("simple-vector");

    public static final Symbol UNBOUNDCONDITION = intern("unbound-condition");
    public static final Symbol LEXERCONDITION = intern("lexer-condition");
    public static final Symbol PARSERCONDITION = intern("parser-condition");
    public static final Symbol FNCONDITION = intern("fn-condition");
    public static final Symbol TYPECONDITION = intern("type-condition");
    public static final Symbol KEYWORDCONDITION = intern("keyword-condition");
    public static final Symbol IOCONDITION = intern("io-condition");
    public static final Symbol STACKCONDITION = intern("stack-condition");
    public static final Symbol MEMORYCONDITION = intern("memory-condition");
    public static final Symbol DIVZEROCONDITION = intern("divzero-condition");
    public static final Symbol FORMATCONDITION = intern("format-condition");
    public static final Symbol GENERALCONDITION = intern("general-condition");
    public static final Symbol SYNTAXCONDITION = intern("syntax-condition");
    public static final Symbol STREAMCONDITION = intern("stream-condition");
    public static final Symbol RETURNCONDITION = intern("return-condition");
    public static final Symbol INVOKECONDITION = intern("invoke-condition");

    private String name;
    private String printname;

    public Symbol(String name) {
        this.name = name;
        printname = getPrintName(getName());
        name.hashCode(); // trigger caching of hash value
    }

    private String getPrintName(String name) {
        StringBuilder b = new StringBuilder();
        boolean escape = false;
        boolean bar = false;
        for (int i = 0; i < name.length(); i++) {
            char ch = name.charAt(i);
            if (escape) {
                b.append(ch);
                escape = false;
            } else if (ch == '\\') {
                escape = true;
            } else if (bar) {
                if (ch == '|') bar = false;
                else b.append(ch);
            } else if (ch == '|') {
                bar = !bar;
            } else {
                b.append(ch);
            }
        }
        return b.toString();
    }

    public String getName() {
        return name.startsWith(":") ? name.substring(1) : name;
    }

    public String getPrintName() {
        return printname;
    }

    @Override
    public boolean isKeyword() {
        return name.startsWith(":");
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public String displayValue() {
        return printname;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof Symbol) {
            return o == this;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public int compareTo(Symbol o) {
        return name.compareTo(o.name);
    }

    @Override
    public boolean isSymbol() {
        return true;
    }

    @Override
    public Symbol asSymbol() {
        return this;
    }

    @Override
    public Obj types() {
        if (this == Symbol.NIL) {
            return new Cons(Symbol.NULL, new Cons(Symbol.SYMBOL, super.types()));
        } else if (isKeyword()) {
            return new Cons(Symbol.KEYWORD, new Cons(Symbol.SYMBOL, super.types()));
        } else {
            return new Cons(Symbol.SYMBOL, super.types());
        }
    }

    @Override
    public IntNum sxhash() { return IntNum.fromLong(name.hashCode()); }

    public static Symbol intern(String name) {
        return SymbolTable.intern(name);
    }

}
