package com.github.leeonky.dal;

import java.util.HashSet;
import java.util.Set;

import static com.github.leeonky.dal.Constants.KeyWords.IS;
import static com.github.leeonky.dal.Constants.KeyWords.WHICH;
import static java.util.Arrays.asList;

public class Constants {
    public static final Set<Character> TOKEN_DELIMITER = new HashSet<>(asList('=', '>', '<', '+', '-', '*', '/', ':', '&', '|', '!', '(', ')', '[', ']', ' ', '\t', '\n'));
    public static final Set<Character> OPERATOR_CHAR = new HashSet<>(asList('=', '>', '<', '+', '-', '*', '/', ':', '&', '|', '!', ','));
    public static final Set<Character> DIGITAL_CHAR = new HashSet<>(asList('1', '2', '3', '4', '5', '6', '7', '8', '9', '0'));
    public static final Set<String> KEYWORD_SETS = new HashSet<>(asList(IS, WHICH));

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
