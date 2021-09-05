package com.github.leeonky.dal.token;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Node;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static com.github.leeonky.dal.token.Token.Type.CLOSING_PARENTHESIS;
import static com.github.leeonky.dal.util.IfThenFactory.when;
import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static java.util.Optional.of;

//TODO clean method
public class TokenStream {
    private static final AtomicInteger DEFAULT_VALUE = new AtomicInteger();
    private static final Set<String> UNARY_OPERATORS_WITHOUT_INTENTION = new HashSet<>(singletonList("!"));
    private static final Set<String> UNARY_OPERATORS = new HashSet<String>(singletonList("-")) {{
        addAll(UNARY_OPERATORS_WITHOUT_INTENTION);
    }};
    private final List<Token> tokens = new ArrayList<>();
    private final Map<Token.Type, AtomicInteger> braceLevels = new HashMap<>();
    private int index = 0;

    public Token pop() {
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
        if (tokens.size() <= index)
            throw new NoMoreTokenException();
        return tokens.get(index);
    }

    public int getPosition() {
        return hasTokens() ? currentToken().getPositionBegin()
                : (index > 0 ? tokens.get(index - 1).getPositionEnd() : 0);
    }

    public Optional<Token> popKeyWord(String keyword) {
        return when(hasTokens()
                && currentToken().getType() == Token.Type.KEY_WORD && keyword.equals(currentToken().getValue()))
                .optional(this::pop);
    }

    public boolean isCurrentSchemaConnectorAndTake() {
        if (hasTokens() && currentToken().getType() == Token.Type.OPERATOR
                && (Constants.SCHEMA_DELIMITER.equals(currentToken().getValue()))) {
            index++;
            return true;
        }
        return false;
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
        return currentToken().getType() == Token.Type.OPERATOR &&
                (isFromBeginning() ? UNARY_OPERATORS_WITHOUT_INTENTION : UNARY_OPERATORS)
                        .contains((String) currentToken().getValue());
    }

    public Node parseBetween(Token.Type opening, Token.Type closing, char closingChar, Supplier<Node> supplier) {
        return when(currentToken().getType() == opening).thenReturn(() -> {
            try {
                Token openingToken = pop();
                braceLevels.computeIfAbsent(closing, k -> new AtomicInteger(0)).incrementAndGet();
                Node node = supplier.get();
                if (!hasTokens())
                    throw new SyntaxException(getPosition(), format("missed `%c`", closingChar));
                if (!(currentToken().getType() == closing))
                    throw new SyntaxException(getPosition(), format("unexpected token, `%c` expected", closingChar));
                pop();
                return node.setPositionBegin(openingToken.getPositionBegin());
            } finally {
                braceLevels.get(closing).decrementAndGet();
            }
        });
    }

    public void checkingParenthesis() {
        if (currentToken().getType() == CLOSING_PARENTHESIS && braceLevels.getOrDefault(CLOSING_PARENTHESIS, DEFAULT_VALUE).get() == 0)
            throw new SyntaxException(getPosition(), "missed '('");
    }

    public Object popTokenForPropertyOrIndex() {
        if (!hasTokens())
            throw new SyntaxException(getPosition(), "should given one property or array index in `[]`");
        return pop().getPropertyOrIndex();
    }


    public Object popOnlyOneTokenForPropertyOnIndex() {
        if (size() != 1)
            throw new IllegalTokenContentException("should given one property or array index in `[]`");
        return pop().getPropertyOrIndex();
    }

    public Optional<Token> popByType(Token.Type type) {
        return when(currentToken().getType() == type).optional(this::pop);
    }

    public Optional<Token> popJudgementOperator() {
        return when(currentToken().judgement()).optional(this::pop);
    }

    public <T extends Node> Optional<T> fetchNode(Supplier<T> supplier) {
        return when(hasTokens()).optional(supplier);
    }

    public <T extends Node> List<T> fetchElements(Token.Type closingType, Function<Integer, T> function) {
        List<T> result = new ArrayList<>();
        int index = 0;
        while (hasTokens() && !(currentToken().getType() == closingType))
            result.add(function.apply(index++));
        return result;
    }
}
