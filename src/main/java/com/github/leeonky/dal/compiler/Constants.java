package com.github.leeonky.dal.compiler;

import java.util.HashSet;
import java.util.Set;

import static java.util.Arrays.asList;

public class Constants {
    public static final Set<Character> DELIMITER = new HashSet<>(asList('=', '>', '<', '+', '-', '*', '/', ':',
            '&', '|', '!', ',', '(', ')', '[', ']', '{', '}', ' ', '\t', '\n', '#', '\'', '"'));
    public static final Set<Character> DELIMITER_OR_DOT = new HashSet<Character>(DELIMITER) {{
        add('.');
    }};
    public static final Set<Character> DIGITAL = new HashSet<>(asList('1', '2', '3', '4', '5', '6', '7', '8', '9', '0'));
    public static final Set<Character> DIGITAL_OR_MINUS = new HashSet<Character>(DIGITAL) {{
        add('-');
    }};
}
