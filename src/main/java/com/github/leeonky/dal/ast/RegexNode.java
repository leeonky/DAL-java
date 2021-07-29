package com.github.leeonky.dal.ast;

import java.util.Objects;
import java.util.regex.Pattern;

import static java.lang.String.format;

public class RegexNode extends Node {
    private final Pattern pattern;

    public RegexNode(String regex) {
        pattern = Pattern.compile(regex);
    }

    @Override
    public String inspect() {
        return format("/%s/", pattern.toString());
    }

    @Override
    public boolean evaluable() {
        return false;
    }

    public boolean matches(String actual) {
        boolean matches = pattern.matcher(actual).matches();
        //TODO error log
        return matches;
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof RegexNode && ((RegexNode) o).pattern.toString().equals(pattern.toString());
    }

    @Override
    public int hashCode() {
        return Objects.hash(pattern.toString());
    }
}
