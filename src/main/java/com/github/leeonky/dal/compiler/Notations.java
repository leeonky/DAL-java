package com.github.leeonky.dal.compiler;

import com.github.leeonky.interpreter.Notation;

import static com.github.leeonky.interpreter.Notation.notation;

public class Notations {


    public static class Keywords {
        public static final Notation
                WHICH = notation("which"),
                IS = notation("is"),
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
                EQUAL = notation("="),
                DOT = notation("."),
                IS = Keywords.IS,
                WHICH = Keywords.WHICH;
    }

    public static final Notation
            SINGLE_QUOTED = notation("'"),
            DOUBLE_QUOTED = notation("\""),
            OPENING_BRACKET = notation("["),
            CLOSING_BRACKET = notation("]"),
            OPENING_PARENTHESES = notation("("),
            CLOSING_PARENTHESES = notation(")"),
            SCHEMA_AND = notation("/"),
            REGEX_NOTATION = notation("/"),
            OPENING_BRACES = notation("{"),
            CLOSING_BRACES = notation("}"),
            COMMA = notation(","),
            COLUMN_SPLITTER = notation("|"),
            SEQUENCE_AZ = notation("+"),
            SEQUENCE_ZA = notation("-"),
            SEQUENCE_AZ_2 = notation("￪"),
            SEQUENCE_ZA_2 = notation("￬");


    @Deprecated
    public static final String IS_s = "is";
    @Deprecated
    public static final String AND_s = "and";
    @Deprecated
    public static final String OR_s = "or";
}
