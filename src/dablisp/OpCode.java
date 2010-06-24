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

public enum OpCode {

    BIND,
    DBIND,
    FBIND,

    JUMP,
    JUMPF,
    JUMPT,
    LABEL,

    NEWENV,
    POPENV,

    NEWFENV,
    POPFENV,

    CALL,
    FCALL,
    RETURN,

    CONST,
    ARGC,
    POP,
    DUP,
    SWAP,
    BUBBLE,
    INC,
    DEC,
    NUMEQ,
    EQ,

    VAR,
    DVAR,
    FVAR,

    SETVAR,
    SETDVAR,
    SETFVAR,

    THROW,
    ADDCATCHER,
    ADDUNWINDPROTECT,
    ADDBLOCK,
    RETURNFROM,
    ADDTAGBODY,
    GO,
    POPHANDLER,

    SUB,
    SUBRET,
    SUBRETV,
    BRETV,

    SETENV,

    PUSHREG,
    POPREG,
    PEEKREG,
    INCREG,
    DECREG,
    DECZREG,
    DEC2ZREG,
    JUMPZREG,
    JUMPNZREG,
    SUBZREG,

    MINARGS,
    CHECKNOARGS,
    RESTLIST,
    KWVAL,
    SPLITLIST,
    MVALSPLIT,

    ERROR,

    CAR,
    CDR,
    CONS,

}
