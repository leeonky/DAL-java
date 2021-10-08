package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Optional;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.github.leeonky.dal.ast.PropertyNode.Type.DOT;
import static com.github.leeonky.dal.runtime.FunctionUtil.getValue;
import static com.github.leeonky.dal.runtime.FunctionUtil.oneOf;
import static com.github.leeonky.dal.runtime.IfThenFactory.when;
import static java.lang.String.format;

public class Token {
    private final StringBuilder contentBuilder;
    private final int position;

    public int getPosition() {
        return position;
    }

    public Token(int position) {
        this.position = position;
        contentBuilder = new StringBuilder();
    }

    public Number getInteger() {
        String content = getContent();
        return getValue(() -> Integer.decode(content),
                () -> Long.decode(content),
                () -> decodeBigInteger(content),
                () -> oneOf(() -> parseNumber(content, "y", Byte::decode),
                        () -> parseNumber(content, "s", Short::decode),
                        () -> parseNumber(content, "l", Long::decode),
                        () -> parseNumber(content, "bi", this::decodeBigInteger))
                        .orElseThrow(() -> new SyntaxException("expect an integer", position)));
    }

    private BigInteger decodeBigInteger(String str) {
        Matcher matcher = Pattern.compile("0[xX](.*)").matcher(str);
        if (matcher.matches())
            return new BigInteger(matcher.group(1), 16);
        return new BigInteger(str);
    }

    public Number getNumber() {
        String content = getContent();
        return getValue(this::getInteger, () -> oneOf(() -> parseNumber(content, "f", Float::valueOf),
                () -> parseNumber(content, "bd", BigDecimal::new),
                () -> parseNumber(content, "d", Double::valueOf))
                .orElseGet(() -> {
                    double value = Double.parseDouble(content);
                    if (Double.isInfinite(value))
                        return new BigDecimal(content);
                    return value;
                }));
    }

    private <T extends Number> Optional<T> parseNumber(String content, String postfix,
                                                       Function<String, T> factory) {
        Pattern pattern = Pattern.compile(format("([^_]*)(%s|%s)$", postfix, postfix.toUpperCase()));
        Matcher matcher = pattern.matcher(content);
        return when(matcher.matches()).optional(() -> factory.apply(matcher.group(1)));
    }

    public String getContent() {
        return contentBuilder.toString();
    }

    public void append(char c) {
        contentBuilder.append(c);
    }

    public char lastChar() {
        return contentBuilder.charAt(contentBuilder.length() - 1);
    }

    public Token append(String str) {
        contentBuilder.append(str);
        return this;
    }

    public Node toDotProperty(Node instanceNode) {
        if (contentBuilder.length() == 0)
            throw new SyntaxException("property is not finished", position);
        return new PropertyNode(instanceNode, getContent(), DOT);
    }

    public boolean isNumber() {
        try {
            return getNumber() != null;
        } catch (Exception ignore) {
            return false;
        }
    }

    public boolean all() {
        return true;
    }
}
