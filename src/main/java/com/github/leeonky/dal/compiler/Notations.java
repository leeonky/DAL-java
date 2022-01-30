package com.github.leeonky.dal.compiler;

import com.github.leeonky.interpreter.Notation;

import static com.github.leeonky.interpreter.Notation.notation;

public class Notations {
    public static final String WHICH_S = "which";
    public static final String IS_s = "is";
    public static final String AND_s = "and";
    public static final String OR_s = "or";

    public static class Keywords {
        public static final Notation
                WHICH = notation("which"),
                TRUE = notation("true"),
                FALSE = notation("false"),
                NULL = notation("null"),
                AND = notation("and"),
                OR = notation("or");
    }

    public static class Operators {
        public static final Notation
                WILDCARD = notation("*"),
                ROW_WILDCARD = notation("***"),
                ELEMENT_ELLIPSIS = notation("..."),
                AND = notation("&&"),
                OR = notation("||"),
                COMMA = notation(","),
                GREATER_OR_EQUAL = notation(">="),
                LESS_OR_EQUAL = notation("<="),
                GREATER = notation(">"),
                LESS = notation("<"),
                PLUS = notation("+"),
                SUBTRACTION = notation("-"),
                MULTIPLICATION = notation("*"),
                DIVISION = notation("/"),
                NOT_EQUAL = notation("!="),
                MINUS = notation("-"),
                NOT = notation("!"),
                MATCHER = notation(":"),
                EQUAL = notation("=");
    }
}
