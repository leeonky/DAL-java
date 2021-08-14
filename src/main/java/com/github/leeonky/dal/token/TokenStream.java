package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.TypeExpression.Operator;

import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static java.lang.String.format;
import static java.util.Arrays.asList;
import static java.util.Optional.of;

public class TokenStream {
    private static final Set<String> UNARY_OPERATORS_WITHOUT_INTENTION = new HashSet<>(asList("!"));
    private static final Set<String> UNARY_OPERATORS = new HashSet<String>(asList("-")) {{
        addAll(UNARY_OPERATORS_WITHOUT_INTENTION);
    }};

    private final List<Token> tokens = new ArrayList<>();
    private int index = 0;

    public Token pop() {
        return tokens.get(index++);
    }

    public boolean hasTokens() {
        return index < tokens.size();
    }

    public Token appendToken(Token token) {
        tokens.add(Objects.requireNonNull(token));
        return token;
    }

    public Token.Type currentType() {
        return currentToken().getType();
    }

    //TODO to be private
    public Token currentToken() {
        if (tokens.size() <= index)
            throw new NoMoreTokenException();
        return tokens.get(index);
    }

    public int getPosition() {
        return hasTokens() ? currentToken().getPositionBegin() : (index > 0 ? tokens.get(index - 1).getPositionEnd() : 0);
    }

    public boolean isCurrentKeywordAndTake(String keyword) {
        if (hasTokens() && currentToken().getType() == Token.Type.KEY_WORD && keyword.equals(currentToken().getValue())) {
            index++;
            return true;
        }
        return false;
    }

    public Iterable<Operator> getSchemaOperators() {
        return () -> new Iterator<Operator>() {
            @Override
            public boolean hasNext() {
                return hasTokens() && currentType() == Token.Type.OPERATOR
                        && ("|".equals(currentToken().getValue())
                        || "/".equals(currentToken().getValue()));
            }

            @Override
            public Operator next() {
                if ("|".equals(pop().getValue()))
                    return Operator.AND;
                else
                    return Operator.OR;
            }
        };
    }

    public Optional<Token> lastToken() {
        return tokens.isEmpty() ? Optional.empty() : of(tokens.get(tokens.size() - 1));
    }

    public int size() {
        return tokens.size();
    }

    public Stream<Token> tokens() {
        return tokens.stream();
    }

    public boolean isFromBeginning() {
        return index == 0;
    }

    public Optional<Token> tryFetchUnaryOperator() {
        return Optional.ofNullable(isCurrentUnaryOperator() ? pop() : null);
    }

    private boolean isCurrentUnaryOperator() {
        return currentType() == Token.Type.OPERATOR &&
                (isFromBeginning() ? UNARY_OPERATORS_WITHOUT_INTENTION : UNARY_OPERATORS)
                        .contains(currentToken().getValue());
    }

    public Node parseBetween(Token.Type opening, Token.Type closing, char closingChar, Supplier<Node> supplier) {
        if (currentType() == opening) {
            pop();
            Node node = supplier.get();
            if (!hasTokens())
                throw new SyntaxException(getPosition(), format("missed `%c`", closingChar));
            if (currentType() != closing)
                throw new SyntaxException(getPosition(), format("unexpected token, `%c` expected", closingChar));
            pop();
            return node;
        }
        return null;
    }
}
