package com.github.leeonky.dal.compiler;

import java.util.HashSet;
import java.util.Set;

import static com.github.leeonky.dal.compiler.Constants.KeyWords.*;
import static java.util.Arrays.asList;

public class Constants {
    public static final Character DOT = '.';
    public static final Character MINUS = '-';

    public static final Set<Character> DELIMITER = new HashSet<>(asList('=', '>', '<', '+', '-', '*', '/', ':',
            '&', '|', '!', ',', '(', ')', '[', ']', '{', '}', ' ', '\t', '\n'));
    public static final Set<Character> DELIMITER_OR_DOT = new HashSet<Character>(DELIMITER) {{
        add(DOT);
    }};
    public static final Set<Character> DIGITAL = new HashSet<>(asList('1', '2', '3', '4', '5', '6', '7', '8', '9', '0'));
    public static final Set<Character> DIGITAL_OR_MINUS = new HashSet<Character>(DIGITAL) {{
        add(MINUS);
    }};
    public static final String SCHEMA_DELIMITER = "/";
    public static final String LIST_ELLIPSIS = "...";
    public static final Set<String> ALL_KEY_WORDS = new HashSet<>(asList(IS, WHICH, TRUE, FALSE, NULL, AND, OR));
    public static final String SEQUENCE_AZ = "+";
    public static final String SEQUENCE_ZA = "-";

    public static class KeyWords {
        public static final String WHICH = "which";
        public static final String IS = "is";
        public static final String NULL = "null";
        public static final String TRUE = "true";
        public static final String FALSE = "false";
        public static final String AND = "and";
        public static final String OR = "or";
    }

    public static class Operators {
        public static final String MATCH = ":";
        public static final String EQ = "=";
    }
}
