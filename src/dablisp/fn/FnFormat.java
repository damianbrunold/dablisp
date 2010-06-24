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

import dablisp.Env;
import dablisp.Fn;
import dablisp.Lisp;
import dablisp.LispException;
import dablisp.Obj;
import dablisp.Stack;
import dablisp.Strm;
import dablisp.Symbol;
import dablisp.Vector;

public class FnFormat extends Fn {

    @SuppressWarnings("null")
    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCountMin(argc, 2);
        Obj strm = stack.pop();
        Strm out = null;
        boolean asstr = false;
        if (strm == Symbol.NIL) {
            asstr = true;
        } else if (strm == Symbol.T) {
            out = Lisp.getStandardOutput();
        } else {
            out = strm.asStrm();
        }
        Vector fmt = stack.pop().asStr();
        Obj args = stack.popList(argc - 2);
        Vector result = format(fmt, args);
        if (asstr) {
            stack.push(result);
        } else {
            out.princ(result);
            stack.push(Symbol.NIL);
        }
    }

    private Vector format(Vector fmt, Obj args) {
        StringBuilder builder = new StringBuilder();
        format(builder, fmt.getStrValue(), args);
        return new Vector(builder.toString());
    }

    private boolean isDirective(String s, char dir) {
    	return Character.toLowerCase(s.charAt(s.length() - 1)) == Character.toLowerCase(dir);
    }

    private void format(StringBuilder builder, String fmt, Obj args) {
        String[] s = split(fmt);
        builder.append(s[0]);
        if (s[1] != null) {
            if (isDirective(s[1], 'a')) {
                builder.append(args.first().displayValue());
                args = args.rest();
            } else if (isDirective(s[1], 's')) {
                builder.append(args.first());
                args = args.rest();
            } else if (isDirective(s[1], 'c')) {
                builder.append(args.first().asChar().getValue());
                args = args.rest();
            } else if (isDirective(s[1], 'd')) {
                builder.append(Long.toString(args.first().asIntNum().intValue(), 10));
                args = args.rest();
            } else if (isDirective(s[1], 'b')) {
                builder.append(Long.toString(args.first().asIntNum().getValue(), 2));
                args = args.rest();
            } else if (isDirective(s[1], 'o')) {
                builder.append(Long.toString(args.first().asIntNum().getValue(), 8));
                args = args.rest();
            } else if (isDirective(s[1], 'x')) {
                builder.append(Long.toString(args.first().asIntNum().getValue(), 16));
                args = args.rest();
            } else if (isDirective(s[1], 'f')) {
                builder.append(String.format("%f", args.first().asFloatNum().getValue()));
                args = args.rest();
            } else if (isDirective(s[1], 'e')) {
                builder.append(String.format("%e", args.first().asFloatNum().getValue()));
                args = args.rest();
            } else if (isDirective(s[1], 'g')) {
                builder.append(String.format("%g", args.first().asFloatNum().getValue()));
                args = args.rest();
            } else if (isDirective(s[1], '%')) {
                builder.append(System.getProperty("line.separator"));
            } else if (isDirective(s[1], '&')) {
            	if (!builder.toString().endsWith(System.getProperty("line.separator"))) {
            		builder.append(System.getProperty("line.separator"));
            	}
            } else {
                throw new LispException(Symbol.FORMATCONDITION, "illegal specifier " + s[2]);
            }
            if (s[2] != null) {
                format(builder, s[2], args);
            }
        }
    }

    private String[] split(String fmt) {
    	// TODO handle full directive syntax...
        String[] result = new String[3];
        int pos = fmt.indexOf("~");
        if (pos == -1) {
            result[0] = fmt;
            result[1] = null;
            result[2] = null;
        } else {
            result[0] = fmt.substring(0, pos);
            result[1] = fmt.substring(pos, pos + 2);
            result[2] = fmt.substring(pos + 2);
        }
        return result;
    }

    @Override
    public String getName() {
        return "format";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " stream spec &rest args) - function -  writes formatted output to a stream";
    }

}
