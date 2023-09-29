package com.github.leeonky.dal.compiler;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static java.util.Arrays.asList;

public class Constants {
    public static final Set<Character> DELIMITER = new HashSet<>(asList('=', '>', '<', '+', '-', '*', '/', ':',
            '&', '|', '!', ',', '(', ')', '[', ']', '{', '}', ' ', '\t', '\n', '\r', '#', '\'', '"'));
    //    TODO missing testing \r
    public static final Set<Character> PROPERTY_DELIMITER = new HashSet<Character>(DELIMITER) {{
        add('.');
    }};

    public static final Set<String> PROPERTY_DELIMITER_STRING = PROPERTY_DELIMITER.stream().map(Object::toString)
            .collect(Collectors.toSet());

    public static final Set<Character> RELAX_PROPERTY_DELIMITER = new HashSet<Character>() {{
        addAll(PROPERTY_DELIMITER);
        for (char c : "-+%;".toCharArray()) {
            remove(c);
        }
    }};

    public static final List<String> EXPRESSION_RELAX_STRING_TAIL = asList(" ", "\r", "\t", "\n", ",", "||", "&&");
    public static final List<String> OBJECT_SCOPE_RELAX_STRING_TAIL = asList(" ", "\r", "\t", "\n", ",", "}");
    public static final List<String> LIST_SCOPE_RELAX_STRING_TAIL = asList(" ", "\r", "\t", "\n", ",", "]");
    public static final List<String> TABLE_CELL_RELAX_STRING_TAIL = asList("|", "\n", "\r");
    public static final List<String> BRACKET_RELAX_STRING_TAIL = asList("]");

    public static final Set<Character> DIGITAL = new HashSet<>(asList('1', '2', '3', '4', '5', '6', '7', '8', '9', '0'));
    public static final Set<Character> DIGITAL_OR_MINUS = new HashSet<Character>(DIGITAL) {{
        add('-');
    }};
}
