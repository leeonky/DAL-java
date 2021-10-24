package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;

import java.util.*;
import java.util.function.BiPredicate;
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

    public static TokenMatcher tokenMatcher(Predicate<Character> startsWith, Collection<String> excluded, boolean trim,
                                            Set<Character> delimiters, Predicate<Token> validator) {
        return tokenMatcher(startsWith, excluded, trim, (c1, c2) -> delimiters.contains(c2), validator);
    }

    public static TokenMatcher tokenMatcher(Predicate<Character> startsWith, Collection<String> excluded, boolean trim,
                                            BiPredicate<Character, Character> endsWith, Predicate<Token> validator) {
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
                    while (sourceCode.hasCode() && !endsWith.test(token.lastChar(), sourceCode.currentChar()));
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

    public char escapedPop(Map<String, Character> escapeChars) {
        return escapeChars.entrySet().stream().filter(e -> code.startsWith(e.getKey(), position)).map(e -> {
            seek(e.getKey().length());
            return e.getValue();
        }).findFirst().orElseGet(this::popChar);
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

    public <T> Optional<Node> fetchElementNode(FetchBy fetchBy, Character opening, char closing,
                                               Supplier<T> element, Function<List<T>, Node> nodeFactory) {
        return when(whenFirstChar(opening::equals)).optional(() -> {
            int startPosition = seek(1);
            List<T> elements = new ArrayList<>();
            while (hasCode() && !fetchBy.isClosing(closing, this)) {
                elements.add(element.get());
                fetchBy.afterFetchElement(this);
            }
            if (!hasCode())
                throw syntaxError(String.format("should end with `%c`", closing), 0);
            seek(1);
            return nodeFactory.apply(elements).setPositionBegin(startPosition);
        });
    }

    public enum FetchBy {
        BY_CHAR,
        BY_NODE {
            @Override
            protected void afterFetchElement(SourceCode sourceCode) {
                sourceCode.popWord(",");
                sourceCode.leftTrim();
            }

            @Override
            protected boolean isClosing(char closing, SourceCode sourceCode) {
                return sourceCode.startsWith(String.valueOf(closing));
            }
        };

        protected void afterFetchElement(SourceCode tokenParser) {
        }

        protected boolean isClosing(char closing, SourceCode sourceCode) {
            return closing == sourceCode.currentChar();
        }
    }
}
