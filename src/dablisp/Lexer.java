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

import java.io.IOException;

public class Lexer {

    private static enum LexerState {
        WHITESPACE, SYMBOL, STRING, NUMBER, PLUSMINUS, FLOAT, SHARP, COMMA, CHAR, DOT, COMMENT, TERMINATE,
    }

    private ReadStream strm;
    private boolean putback;
    private Token token;

    public Lexer(ReadStream strm) {
        this.strm = strm;
    }

    public void putBack(Token token) {
        if (putback)
            throw new LispException(Symbol.LEXERCONDITION, "cannot put back token");
        this.token = token;
        putback = true;
    }

    private void unread(char ch) {
        try {
            strm.unread(ch);
        } catch (IOException e) {
            throw new LispException(Symbol.LEXERCONDITION, "cannot unread character", e);
        }
    }

    private int read() {
        try {
            return strm.read();
        } catch (IOException e) {
            throw new LispException(Symbol.LEXERCONDITION, "cannot read character", e);
        }
    }

    public Token getToken() {
        if (putback) {
            putback = false;
            return token;
        }
        String digits = "0123456789";
        String hexdigits = "0123456789abcdefABCDEF";
        String octdigits = "01234567";
        String bindigits = "01";
        TokenType type = TokenType.UNDEFINED;
        LexerState state = LexerState.WHITESPACE;
        boolean escape = false;
        boolean bar = false;
        StringBuilder token = new StringBuilder();
        while (state != LexerState.TERMINATE) {
            int c = read();
            if (c == -1) {
                state = LexerState.TERMINATE;
                break;
            }
            char ch = (char) c;
            switch (state) {
            case WHITESPACE:
                if (isWhitespace(ch))
                    break;
                if (ch == '(')
                    return new Token("(", TokenType.PAROPEN);
                if (ch == ')')
                    return new Token(")", TokenType.PARCLOSE);
                if (ch == '\'')
                    return new Token("'", TokenType.QUOTE);
                if (ch == '`')
                    return new Token("`", TokenType.BACKQUOTE);
                if (ch == '#')
                    state = LexerState.SHARP;
                else if (ch == ',')
                    state = LexerState.COMMA;
                else if (ch == '.')
                    state = LexerState.DOT;
                else if (ch == ';')
                    state = LexerState.COMMENT;
                else if (Character.isDigit(ch)) {
                    state = LexerState.NUMBER;
                    type = TokenType.INTNUM;
                    token.append(ch);
                } else if (ch == '-' || ch == '+') {
                    state = LexerState.PLUSMINUS;
                    type = TokenType.INTNUM;
                    token.append(ch);
                } else if (ch == '"') {
                    state = LexerState.STRING;
                    type = TokenType.STR;
                } else {
                    bar = ch == '|';
                    state = LexerState.SYMBOL;
                    type = TokenType.SYMBOL;
                    token.append(ch);
                }
                break;

            case COMMENT:
                if (ch == '\n') {
                    state = LexerState.WHITESPACE;
                }
                break;

            case SYMBOL:
                if (escape) {
                    escape = false;
                    token.append(ch);
                } else if (ch == '\\') {
                    escape = true;
                    token.append(ch);
                } else if (bar) {
                    if (ch == '|')
                        bar = false;
                    token.append(ch);
                } else if (ch == '|') {
                    bar = !bar;
                    token.append(ch);
                } else if (ch == ')' || ch == '(' || isWhitespace(ch) || ch == ';') {
                    unread(ch);
                    state = LexerState.TERMINATE;
                } else {
                    token.append(ch);
                }
                break;

            case STRING:
                if (escape) {
                    token.append(ch);
                    escape = false;
                } else if (ch == '\\') {
                    escape = true;
                } else if (ch == '"') {
                    state = LexerState.TERMINATE;
                } else {
                    token.append(ch);
                }
                break;

            case PLUSMINUS:
                if (Character.isDigit(ch)) {
                    state = LexerState.NUMBER;
                    token.append(ch);
                } else if (ch == '.') {
                    type = TokenType.FLOATNUM;
                    state = LexerState.FLOAT;
                    token.append("0.");
                } else {
                    type = TokenType.SYMBOL;
                    state = LexerState.SYMBOL;
                    unread(ch);
                }
                break;

            case NUMBER:
                if (digits.indexOf(ch) != -1) {
                    token.append(ch);
                } else if (ch == '.' && !token.toString().startsWith("x")) {
                    token.append(ch);
                    state = LexerState.FLOAT;
                    type = TokenType.FLOATNUM;
                } else if (ch == ')' || ch == '(' || isWhitespace(ch) || ch == ';') {
                    unread(ch);
                    type = TokenType.INTNUM;
                    state = LexerState.TERMINATE;
                } else {
                    token.append(ch);
                    type = TokenType.SYMBOL;
                    state = LexerState.SYMBOL;
                }
                break;

            case FLOAT:
                if (Character.isDigit(ch)) {
                    token.append(ch);
                } else {
                    unread(ch);
                    type = TokenType.FLOATNUM;
                    state = LexerState.TERMINATE;
                }
                break;

            case SHARP:
                if (ch == '\\') {
                    state = LexerState.CHAR;
                } else if (ch == '(') {
                    return new Token("#(", TokenType.SHARPPAROPEN);
                } else if (ch == '\'') {
                    token.append("#'");
                    type = TokenType.SHARPQUOTE;
                    state = LexerState.TERMINATE;
                } else if (ch == 'x') {
                    token.append("x");
                    digits = hexdigits;
                    type = TokenType.INTNUM;
                    state = LexerState.NUMBER;
                } else if (ch == 'b') {
                    token.append("b");
                    digits = bindigits;
                    type = TokenType.INTNUM;
                    state = LexerState.NUMBER;
                } else if (ch == 'o') {
                    token.append("o");
                    digits = octdigits;
                    type = TokenType.INTNUM;
                    state = LexerState.NUMBER;
                } else {
                    // TODO throw error?
                }
                break;

            case CHAR:
                if (ch == ')' || ch == '(' || isWhitespace(ch) || ch == ';') {
                    unread(ch);
                    type = TokenType.CHAR;
                    state = LexerState.TERMINATE;
                } else {
                    token.append(ch);
                }
                break;

            case COMMA:
                if (ch == '@') {
                    token.append(",@");
                    type = TokenType.COMMAATSIGN;
                    state = LexerState.TERMINATE;
                } else if (ch == '.') {
                    token.append(",.");
                    type = TokenType.COMMADOT;
                    state = LexerState.TERMINATE;
                } else {
                    token.append(',');
                    unread(ch);
                    type = TokenType.COMMA;
                    state = LexerState.TERMINATE;
                }
                break;

            case DOT:
                if (Character.isDigit(ch)) {
                    type = TokenType.FLOATNUM;
                    state = LexerState.FLOAT;
                    token.append("0.");
                    token.append(ch);
                } else {
                    unread(ch);
                    token.append('.');
                    type = TokenType.DOT;
                    state = LexerState.TERMINATE;
                }
                break;

            }
        }
        if (token.length() == 0 && type != TokenType.STR) {
            return null;
        }
        return new Token(token.toString(), type);
    }

    private boolean isWhitespace(int ch) {
        return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r';
    }

}
