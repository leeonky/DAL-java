package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.IntStream;

import static com.github.leeonky.dal.runtime.IfThenFactory.when;
import static java.util.Optional.empty;
import static java.util.Optional.of;

public class SourceCode {
    private final String code;
    private final char[] chars;
    private int position = 0;

    public static TokenMatcher tokenMatcher(Predicate<Character> startsWith, Collection<String> excluded,
                                            boolean trim, Set<Character> delimiters, Predicate<Token> validator) {
        return sourceCode -> {
            if (sourceCode.whenFirstChar(startsWith) && sourceCode.hasCode()
                    && excluded.stream().noneMatch(sourceCode::startsWith)) {
                Token token = new Token(sourceCode.position);
                if (trim) {
                    sourceCode.popChar();
                    sourceCode.leftTrim();
                }
                if (sourceCode.hasCode())
                    do token.append(sourceCode.popChar());
                    while (sourceCode.hasCode() && !delimiters.contains(sourceCode.currentChar()));
                if (validator.test(token))
                    return of(token);
                sourceCode.position = token.getPosition();
            }
            return empty();
        };
    }

    public SourceCode(String code) {
        this.code = code;
        chars = code.toCharArray();
    }

    private int seek(int seek) {
        int position = this.position;
        this.position += seek;
        return position;
    }

    private char currentChar() {
        return chars[position];
    }

    private char popChar() {
        return chars[position++];
    }

    private boolean whenFirstChar(Predicate<Character> predicate) {
        return leftTrim().hasCode() && predicate.test(currentChar());
    }

    //TODO private
    public boolean hasCode() {
        return position < chars.length;
    }

    public SourceCode leftTrim() {
        while (hasCode() && Character.isWhitespace(currentChar()))
            position++;
        return this;
    }

    public boolean startsWith(String word) {
        leftTrim();
        return (code.startsWith(word, position));
    }

    public char escapedPop(EscapeChars escapeChars) {
        return escapeChars.escapeAt(code, position, length -> position += length).orElseGet(this::popChar);
    }

    public boolean isBeginning() {
        return IntStream.range(0, position).mapToObj(i -> chars[i]).allMatch(Character::isWhitespace);
    }

    public SyntaxException syntaxError(String message, int positionOffset) {
        return new SyntaxException(message, position + positionOffset);
    }

    public Optional<Token> popWord(String word) {
        return popWord(word, () -> true);
    }

    public Optional<Token> popWord(String word, Supplier<Boolean> predicate) {
        return when(startsWith(word) && predicate.get()).optional(() -> new Token(seek(word.length())).append(word));
    }

    public <T> Optional<Node> fetchElements(TokenParser.FetchBy fetchBy, Character opening, char closing,
                                            Function<Integer, T> element, Function<List<T>, Node> nodeFactory) {
        return when(whenFirstChar(opening::equals)).optional(() -> {
            int startPosition = seek(1);
            List<T> elements = new ArrayList<>();
            int index = 0;
            while (hasCode() && closing != currentChar()) {
                elements.add(element.apply(index++));
                fetchBy.afterFetchElement(this);
            }
            if (!hasCode())
                throw syntaxError(String.format("should end with `%c`", closing), 0);
            seek(1);
            return nodeFactory.apply(elements).setPositionBegin(startPosition);
        });
    }
}
