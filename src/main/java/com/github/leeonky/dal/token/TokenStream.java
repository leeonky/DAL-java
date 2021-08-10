package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.BracketNode;
import com.github.leeonky.dal.ast.SchemaAssertionExpression;

import java.util.*;
import java.util.stream.Stream;

import static java.util.Arrays.asList;

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
        return tokens.get(index).getType();
    }

    public boolean isCurrentSingleEvaluateNode() {
        return currentType() == Token.Type.PROPERTY;
    }

    public int getPosition() {
        return hasTokens() ? tokens.get(index).getPositionBegin() : (index > 0 ? tokens.get(index - 1).getPositionEnd() : 0);
    }

    public boolean isCurrentKeywordAndTake(String keyword) {
        final Token token = tokens.get(index);
        if (token.getType() == Token.Type.KEY_WORD && keyword.equals(token.getValue())) {
            index++;
            return true;
        }
        return false;
    }

    public boolean isCurrentBeginBracket() {
        return currentType() == Token.Type.BEGIN_BRACKET;
    }

    public boolean isCurrentEndBracketAndTakeThenFinishBracket(BracketNode bracketNode) {
        if (currentType() == Token.Type.END_BRACKET) {
            if (bracketNode == null)
                throw new SyntaxException(index, "missed begin bracket");
            bracketNode.finishBracket();
            index++;
            return true;
        }
        return false;
    }

    public boolean isSingleUnaryOperator(boolean withoutIntention) {
        return currentType() == Token.Type.OPERATOR &&
                (withoutIntention ? UNARY_OPERATORS_WITHOUT_INTENTION : UNARY_OPERATORS)
                        .contains(tokens.get(index).getValue());
    }

    public Iterable<SchemaAssertionExpression.Operator> getSchemaOperators() {
        return () -> new Iterator<SchemaAssertionExpression.Operator>() {
            @Override
            public boolean hasNext() {
                return hasTokens() && currentType() == Token.Type.OPERATOR
                        && ("|".equals(tokens.get(index).getValue())
                        || "/".equals(tokens.get(index).getValue()));
            }

            @Override
            public SchemaAssertionExpression.Operator next() {
                if ("|".equals(pop().getValue()))
                    return SchemaAssertionExpression.Operator.AND;
                else
                    return SchemaAssertionExpression.Operator.OR;
            }
        };
    }

    public boolean isCurrentRegexNode() {
        return currentType() == Token.Type.REGEX;
    }

    public boolean isLastTokenOperatorMatches() {
        return !tokens.isEmpty() && tokens.get(tokens.size() - 1).isOperatorMatches();
    }

    public int size() {
        return tokens.size();
    }

    public Stream<Token> stream() {
        return tokens.stream();
    }
}
