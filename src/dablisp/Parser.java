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

public class Parser {

    public static Obj parse(Lexer lexer) {
        return parse(lexer, lexer.getToken());
    }

    private static Obj parse(Lexer lexer, Token token) {
        if (token == null) return null;
        if (token.type == TokenType.PAROPEN) {
            return parseCons(lexer, lexer.getToken());
        } else if (token.type == TokenType.SHARPPAROPEN) {
            return parseVector(lexer, lexer.getToken());
        } else if (token.type == TokenType.QUOTE) {
            return new Cons(Symbol.QUOTE, new Cons(parse(lexer), Symbol.NIL));
        } else if (token.type == TokenType.SHARPQUOTE) {
            return new Cons(Symbol.FUNCTION, new Cons(parse(lexer), Symbol.NIL));
        } else if (token.type == TokenType.BACKQUOTE) {
            return new Cons(Symbol.BACKQUOTE, new Cons(parse(lexer), Symbol.NIL));
        } else if (token.type == TokenType.COMMA) {
            return new Cons(Symbol.COMMA, new Cons(parse(lexer), Symbol.NIL));
        } else if (token.type == TokenType.COMMAATSIGN) {
            return new Cons(Symbol.COMMAATSIGN, new Cons(parse(lexer), Symbol.NIL));
        } else if (token.type == TokenType.COMMADOT) {
            return new Cons(Symbol.COMMADOT, new Cons(parse(lexer), Symbol.NIL));
        } else {
            return tokenToObj(token);
        }
    }

    private static Obj parseVector(Lexer lexer, Token token) {
        if (token == null) throw new LispException(Symbol.PARSERCONDITION, "unexpected end of file");
        List<Obj> values = new ArrayList<Obj>();
        while (token.type != TokenType.PARCLOSE) {
            values.add(parse(lexer, token));
            token = lexer.getToken();
            if (token == null) throw new LispException(Symbol.PARSERCONDITION, "unexpected end of file");
        }
        Vector v = new Vector(Symbol.T, Symbol.NIL, values.size());
        for (int i = 0; i < values.size(); i++) {
            v.set(values.get(i), i);
        }
        return v;
    }

    private static Obj parseCons(Lexer lexer, Token token) {
        if (token == null) throw new LispException(Symbol.PARSERCONDITION, "unexpected end of file");
        if (token.type == TokenType.PARCLOSE) {
            return Symbol.NIL;
        } else {
            Obj car = parse(lexer, token);
            token = lexer.getToken();
            if (token == null) throw new LispException(Symbol.PARSERCONDITION, "unexpected end of file");
            if (token.type == TokenType.DOT) {
                Obj cdr = parse(lexer);
                if (lexer.getToken().type != TokenType.PARCLOSE) throw new LispException(Symbol.PARSERCONDITION, "expected closing parenthesis but got " + token.value);
                return new Cons(car, cdr);
            } else {
                Obj cdr = parseCons(lexer, token);
                return new Cons(car, cdr);
            }
        }
    }

    private static Obj tokenToObj(Token token) {
        switch (token.type) {
        case INTNUM:
            if (token.value.startsWith("x")) return IntNum.fromLong(Long.parseLong(token.value.substring(1), 16));
            if (token.value.startsWith("o")) return IntNum.fromLong(Long.parseLong(token.value.substring(1), 8));
            if (token.value.startsWith("b")) return IntNum.fromLong(Long.parseLong(token.value.substring(1), 2));
            return IntNum.fromLong(Long.parseLong(token.value.startsWith("+") ? token.value.substring(1) : token.value));
        case FLOATNUM: return new FloatNum(Double.parseDouble(token.value.startsWith("+") ? token.value.substring(1) : token.value));
        case STR: return new Vector(token.value);
        case CHAR:
            if (token.value.equalsIgnoreCase("Newline")) {
                return new Char('\n');
            } else if (token.value.equalsIgnoreCase("Return")) {
                return new Char('\r');
            } else if (token.value.equalsIgnoreCase("Space")) {
                return new Char(' ');
            } else if (token.value.equalsIgnoreCase("Tab")) {
                return new Char('\t');
            } else {
                return new Char(token.value.charAt(0));
            }
        case SYMBOL:
        	return Symbol.intern(token.value);
        default:
            throw new LispException(Symbol.PARSERCONDITION, "cannot convert " + token.value + " to obj");
        }
    }

}
