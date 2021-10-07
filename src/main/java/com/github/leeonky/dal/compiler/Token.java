package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.github.leeonky.dal.ast.PropertyNode.Type.DOT;

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

    //    TODO refactor
    public Number getInteger() {
        String content = getContent();
        try {
            return Integer.decode(content);
        } catch (NumberFormatException ignore) {
            try {
                return Long.decode(content);
            } catch (NumberFormatException ignore2) {
                Pattern pattern = Pattern.compile("([^_]*)[yY]$");
                Matcher matcher = pattern.matcher(content);
                if (matcher.matches()) {
                    return Byte.decode(matcher.group(1));
                }
                pattern = Pattern.compile("([^_]*)[sS]$");
                matcher = pattern.matcher(content);
                if (matcher.matches()) {
                    return Short.decode(matcher.group(1));
                }
                pattern = Pattern.compile("([^_]*)[lL]$");
                matcher = pattern.matcher(content);
                if (matcher.matches()) {
                    return Long.decode(matcher.group(1));
                }
                pattern = Pattern.compile("([^_]*)(bi|BI)$");
                matcher = pattern.matcher(content);
                if (matcher.matches()) {
                    return decodeBigInteger(matcher.group(1));
                }
                throw new SyntaxException("expect an integer", position);
            }
        }
    }

    private BigInteger decodeBigInteger(String str) {
        Matcher matcher = Pattern.compile("0[xX](.*)").matcher(str);
        if (matcher.matches())
            return new BigInteger(matcher.group(1), 16);
        return new BigInteger(str);
    }

    public Number getNumber() {
        try {
            return getInteger();
        } catch (SyntaxException ignore) {
            //    TODO refactor
            String content = getContent();
            Pattern pattern = Pattern.compile("([^_]*)[fF]$");
            Matcher matcher = pattern.matcher(content);
            if (matcher.matches()) {
                return Float.valueOf(matcher.group(1));
            }
            pattern = Pattern.compile("([^_]*)(bd|BD)$");
            matcher = pattern.matcher(content);
            if (matcher.matches()) {
                return new BigDecimal(matcher.group(1));
            }
            pattern = Pattern.compile("([^_]*)[dD]$");
            matcher = pattern.matcher(content);
            if (matcher.matches()) {
                return Double.valueOf(matcher.group(1));
            }

//          TODO should parse BigInteger, float, double, BigDecimal
            return new BigDecimal(content);
        }
    }

    public String getContent() {
        return contentBuilder.toString();
    }

    public void append(char c) {
        contentBuilder.append(c);
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
