package com.github.leeonky.dal.token;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Node;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static com.github.leeonky.dal.token.Token.Type.*;
import static com.github.leeonky.dal.util.IfThenFactory.when;
import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static java.util.Optional.of;

public class TokenStream {
    private static final AtomicInteger DEFAULT_VALUE = new AtomicInteger();
    private static final Set<String> UNARY_OPERATORS_WITHOUT_INTENTION = new HashSet<>(singletonList("!"));
    private static final Set<String> UNARY_OPERATORS = new HashSet<String>(singletonList("-")) {{
        addAll(UNARY_OPERATORS_WITHOUT_INTENTION);
    }};
    private final List<Token> tokens = new ArrayList<>();
    private final Map<Token.Type, AtomicInteger> braceLevels = new HashMap<>();
    private int index = 0;

    private Token pop() {
        return tokens.get(index++);
    }

    private boolean hasTokens() {
        return index < tokens.size();
    }

    public Token appendToken(Token token) {
        tokens.add(Objects.requireNonNull(token));
        return token;
    }

    private Token currentToken() {
        assert (tokens.size() > index);
        return tokens.get(index);
    }

    public int getPosition() {
        return hasTokens() ? currentToken().getPositionBegin()
                : (index > 0 ? tokens.get(index - 1).getPositionEnd() : 0);
    }

    public Optional<Token> popKeyWord(String keyword) {
        return when(isType(KEY_WORD) && keyword.equals(currentToken().getValue())).optional(this::pop);
    }

    public boolean isCurrentSchemaConnectorAndTake() {
        if (isType(OPERATOR) && (Constants.SCHEMA_DELIMITER.equals(currentToken().getValue()))) {
            index++;
            return true;
        }
        return false;
    }

    public Optional<Token> lastToken() {
        return tokens.isEmpty() ? Optional.empty() : of(tokens.get(tokens.size() - 1));
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
        return isType(OPERATOR) && (isFromBeginning() ? UNARY_OPERATORS_WITHOUT_INTENTION : UNARY_OPERATORS)
                .contains((String) currentToken().getValue());
    }

    public Optional<Node> parseBetween(Token.Type opening, Token.Type closing, char closingChar, Supplier<Node> supplier) {
        return when(isType(opening)).optional(() -> {
            try {
                Token openingToken = pop();
                braceLevels.computeIfAbsent(closing, k -> new AtomicInteger(0)).incrementAndGet();
                Node node = supplier.get();
                if (!hasTokens())
                    throw new SyntaxException(getPosition(), format("missed `%c`", closingChar));
                if (!(isType(closing)))
                    throw new SyntaxException(getPosition(), format("unexpected token, `%c` expected", closingChar));
                pop();
                return node.setPositionBegin(openingToken.getPositionBegin());
            } finally {
                braceLevels.get(closing).decrementAndGet();
            }
        });
    }

    public void checkingParenthesis() {
        if (isType(CLOSING_PARENTHESIS) && braceLevels.getOrDefault(CLOSING_PARENTHESIS, DEFAULT_VALUE).get() == 0)
            throw new SyntaxException(getPosition(), "missed '('");
    }

    public Object popTokenForPropertyOrIndex() {
        if (!hasTokens())
            throw new SyntaxException(getPosition(), "should given one property or array index in `[]`");
        int sign = popBy(OPERATOR, "-").map(o -> -1).orElse(1);
        Object propertyOrIndex = pop().getPropertyOrIndex();
        if (propertyOrIndex instanceof Integer)
            return sign * (int) propertyOrIndex;
        return propertyOrIndex;
    }


    public Object popOnlyOneTokenForPropertyOnIndex() {
        if (tokens.size() != 1)
            throw new IllegalTokenContentException("should given one property or array index in `[]`");
        return pop().getPropertyOrIndex();
    }

    public Optional<Token> popBy(Token.Type type) {
        return when(isType(type)).optional(this::pop);
    }

    private boolean isType(Token.Type type) {
        return hasTokens() && currentToken().getType() == type;
    }

    public Optional<Token> popBy(Token.Type type, Object value) {
        return when(isType(type) && Objects.equals(currentToken().getValue(), value)).optional(this::pop);
    }

    public Optional<Token> popJudgementOperator() {
        return when(currentToken().isJudgement()).optional(this::pop);
    }

    public Optional<Token> popCalculationOperator() {
        return when(isType(OPERATOR)
                && !currentToken().isJudgement() && !currentToken().isComma()).optional(this::pop);
    }

    public <T extends Node> Optional<T> fetchNode(Supplier<T> supplier) {
        return when(hasTokens()).optional(supplier);
    }

    public <T extends Node> List<T> fetchElements(Token.Type closingType, Function<Integer, T> function) {
        List<T> result = new ArrayList<>();
        int index = 0;
        while (hasTokens() && !isType(closingType)) {
            result.add(function.apply(index++));
            popOptionalComma();
        }
        return result;
    }

    private void popOptionalComma() {
        if (hasTokens() && currentToken().isComma())
            pop();
    }

    public boolean isAfterKeyWordWhich() {
        assert (index > 0);
        return tokens.get(index - 1).isKeyWord(Constants.KeyWords.WHICH);
    }
}
