package com.github.leeonky.dal.compiler;

import java.util.HashSet;
import java.util.Set;

import static com.github.leeonky.dal.compiler.Notations.*;
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
    public static final String ELEMENT_ELLIPSIS = "...";


    public static final Set<String> ALL_KEY_WORDS = new HashSet<>(asList(IS_s,
            WHICH.getLabel(), TRUE.getLabel(), FALSE.getLabel(), NULL.getLabel(), AND_s, OR_s));
    public static final String SEQUENCE_AZ = "+";
    public static final String SEQUENCE_ZA = "-";

    public static final String SEQUENCE_AZ_2 = "￪";
    public static final String SEQUENCE_ZA_2 = "￬";

    public static class Operators {
        public static final String MATCH = ":";
        public static final String EQ = "=";
    }
}
