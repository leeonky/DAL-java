package com.github.leeonky.dal.token;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Node;

import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static java.lang.String.format;
import static java.util.Arrays.asList;
import static java.util.Optional.empty;
import static java.util.Optional.of;

//TODO clean method
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
        if (hasTokens() && currentToken().getType() == Token.Type.KEY_WORD && keyword.equals(currentToken().getValue())) {
            return of(pop());
        }
        return empty();
    }

    public boolean isCurrentSchemaConnectorAndTake() {
        if (hasTokens() && currentToken().getType() == Token.Type.OPERATOR && (Constants.SCHEMA_DELIMITER.equals(currentToken().getValue()))) {
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
        return isType(Token.Type.OPERATOR) &&
                (isFromBeginning() ? UNARY_OPERATORS_WITHOUT_INTENTION : UNARY_OPERATORS)
                        .contains(currentToken().getValue());
    }

    public Node parseBetween(Token.Type opening, Token.Type closing, char closingChar, Supplier<Node> supplier) {
        if (isType(opening)) {
            Token openingToken = pop();
            Node node = supplier.get();
            if (!hasTokens())
                throw new SyntaxException(getPosition(), format("missed `%c`", closingChar));
            if (!isType(closing))
                throw new SyntaxException(getPosition(), format("unexpected token, `%c` expected", closingChar));
            pop();
            return node.setPositionBegin(openingToken.getPositionBegin());
        }
        return null;
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
        if (isType(type))
            return of(pop());
        return Optional.empty();
    }

    //TODO to be private
    public boolean isType(Token.Type type) {
        return currentToken().getType() == type;
    }

    public <T extends Node> Optional<T> fetchNode(Supplier<T> supplier) {
        if (hasTokens())
            return of(supplier.get());
        return empty();
    }

    public <T extends Node> List<T> fetchElements(Token.Type closingType, Function<Integer, T> function) {
        List<T> result = new ArrayList<>();
        int index = 0;
        while (hasTokens() && !isType(closingType))
            result.add(function.apply(index++));
        return result;
    }
}
