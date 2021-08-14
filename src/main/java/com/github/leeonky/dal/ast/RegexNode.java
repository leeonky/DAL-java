package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;

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

    public boolean matches(String actual) {
        return pattern.matcher(actual).matches();
    }

    @Override
    public boolean judge(Operator.Equal operator, Object input, RuntimeContext context) {
        if (input instanceof String)
            return matches((String) input);
        throw new RuntimeException("Operator eq before regex need a string input value", operator.getPosition());
    }

    @Override
    public boolean judge(Operator.Matcher operator, Object input, RuntimeContext context) {
        return matches(context.getConverter().convert(String.class, input));
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof RegexNode && ((RegexNode) o).pattern.toString().equals(pattern.toString());
    }
}
