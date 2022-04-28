package com.github.leeonky.interpreter;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.function.Supplier;

import static com.github.leeonky.interpreter.IfThenFactory.when;

public class SourceCode {
    private final String code;
    private int position = 0;
    private int startPosition = 0;
    private final List<Notation> lineComments;

    public static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, S extends Procedure<C, N, E, O, S>> TokenScanner<C, N, E, O, S> tokenScanner(
            Predicate<Character> startsWith, Set<String> excluded, boolean trimStart, Set<Character> delimiters) {
        return tokenScanner(startsWith, excluded, trimStart, delimiters, token -> true);
    }

    public static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, S extends Procedure<C, N, E, O, S>> TokenScanner<C, N, E, O, S> tokenScanner(
            Predicate<Character> startsWith, Set<String> excluded, boolean trimStart, Set<Character> delimiters,
            Predicate<Token> validator) {
        return tokenScanner(startsWith, excluded, trimStart, (code, position, size) -> delimiters.contains(code.charAt(position)), validator);
    }

    public static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, S extends Procedure<C, N, E, O, S>> TokenScanner<C, N, E, O, S> tokenScanner(
            Predicate<Character> startsWith, Set<String> excluded, boolean trimStart,
            TriplePredicate<String, Integer, Integer> endsWith, Predicate<Token> predicate) {
        return sourceCode -> sourceCode.tryFetch(() -> when(sourceCode.whenFirstChar(startsWith)).optional(() -> {
            Token token = tokenScanner(trimStart, endsWith).scan(sourceCode);
            return !excluded.contains(token.getContent()) && predicate.test(token) ? token : null;
        }));
    }

    public static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, S extends Procedure<C, N, E, O, S>> TokenScanner.Mandatory<C, N, E, O, S> tokenScanner(
            boolean trimStart, TriplePredicate<String, Integer, Integer> endsWith) {
        return sourceCode -> {
            Token token = new Token(sourceCode.position);
            if (trimStart) {
                sourceCode.popChar();
                sourceCode.trimBlankAndComment();
            }
            int size = 0;
            while (sourceCode.hasCode() && !endsWith.test(sourceCode.code, sourceCode.position, size++))
                token.append(sourceCode.popChar());
            return token;
        };
    }

    private SourceCode(String code, List<Notation> lineComments) {
        this.code = code;
        this.lineComments = lineComments;
        trimBlankAndComment();
        startPosition = position;
    }

    public static SourceCode createSourceCode(String code, List<Notation> lineComments) {
        return new SourceCode(code, lineComments);
    }

    private boolean codeStartWith(Notation notation) {
        while (hasCode() && Character.isWhitespace(currentChar()))
            position++;
        return code.startsWith(notation.getLabel(), position);
    }

    private SourceCode trimBlankAndComment() {
        while (lineComments.stream().anyMatch(this::codeStartWith)) {
            int newLinePosition = code.indexOf("\n", position);
            position = newLinePosition == -1 ? code.length() : newLinePosition + 1;
        }
        return this;
    }

    private int seek(int seek) {
        int position = this.position;
        this.position += seek;
        return position;
    }

    private char currentChar() {
        return code.charAt(position);
    }

    private char popChar() {
        return code.charAt(position++);
    }

    private boolean whenFirstChar(Predicate<Character> predicate) {
        return trimBlankAndComment().hasCode() && predicate.test(currentChar());
    }

    public boolean hasCode() {
        return position < code.length();
    }

    public boolean startsWith(Notation notation) {
        trimBlankAndComment();
        return code.startsWith(notation.getLabel(), position);
    }

    public boolean startsWith(String word) {
        return code.startsWith(word, position);
    }

    public char popChar(Map<String, Character> escapeChars) {
        return escapeChars.entrySet().stream().filter(e -> code.startsWith(e.getKey(), position)).map(e -> {
            seek(e.getKey().length());
            return e.getValue();
        }).findFirst().orElseGet(this::popChar);
    }

    public boolean isBeginning() {
        return code.chars().skip(startPosition).limit(position - startPosition).allMatch(Character::isWhitespace);
    }

    public SyntaxException syntaxError(String message, int positionOffset) {
        return new SyntaxException(message, position + positionOffset);
    }

    public Optional<Token> popWord(Notation notation) {
        return popWord(notation, () -> true);
    }

    public Optional<Token> popWord(Notation notation, Supplier<Boolean> predicate) {
        return when(startsWith(notation) && predicate.get()).optional(() -> new Token(seek(notation.length()))
                .append(notation.getLabel()));
    }

    public <N> Optional<N> tryFetch(Supplier<Optional<N>> supplier) {
        int position = this.position;
        Optional<N> optionalNode = supplier.get();
        if (!optionalNode.isPresent())
            this.position = position;
        return optionalNode;
    }

    public boolean isEndOfLine() {
        if (!hasCode())
            return true;
        while (Character.isWhitespace(currentChar()) && currentChar() != '\n')
            popChar();
        return currentChar() == '\n';
    }

    public String codeBefore(Notation notation) {
        int index = code.indexOf(notation.getLabel(), position);
        return index >= 0 ? code.substring(position, index) : code.substring(position);
    }

    public int nextPosition() {
        return trimBlankAndComment().position;
    }
}
