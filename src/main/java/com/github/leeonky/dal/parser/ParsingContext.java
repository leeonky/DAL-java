package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.token.IllegalTokenContentException;
import com.github.leeonky.dal.token.Scanner;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;

import static com.github.leeonky.dal.parser.SourceCodeMatcher.createSourceCodeMatcher;
import static com.github.leeonky.dal.util.IfThen.when;

public class ParsingContext {
    public static final SourceCodeMatcher DIGITAL = oneCharMatcher(Scanner.DIGITAL_CHAR::contains);
    public static final SourceCodeMatcher DELIMITER = oneCharMatcher(Scanner.TOKEN_DELIMITER::contains);
    public static final SourceCodeMatcher OPERATOR = oneCharMatcher(Scanner.OPERATOR_CHAR::contains);
    public static final SourceCodeMatcher AFTER_TOKEN_MATCHES = createSourceCodeMatcher(context ->
            context.last != null && context.last.isOperatorMatches());
    public static final SourceCodeMatcher AFTER_OPERATOR_MATCHES = createSourceCodeMatcher(context ->
            context.parsedCode.isOperatorMatches());
    public static final SourceCodeMatcher ANY_CHARACTERS = createSourceCodeMatcher(context ->
            context.sourceCode.notEnd());
    public static final TokenStartEnd END_OF_CODE = TokenStartEnd.createTokenStartEnd(context ->
            !context.sourceCode.notEnd());

    private final SourceCode sourceCode;
    private final ParsedCode parsedCode = new ParsedCode();

    private Token last;

    public ParsingContext(SourceCode sourceCode, Token last) {
        this.sourceCode = sourceCode;
        this.last = last;
    }

    private static SourceCodeMatcher oneCharMatcher(Predicate<Character> predicate) {
        return createSourceCodeMatcher(context -> predicate.test(context.sourceCode.currentChar()));
    }

    public static SourceCodeMatcher CHARACTER(char a) {
        return oneCharMatcher(character -> Objects.equals(character, a));
    }

    public static TokenStartEnd included(SourceCodeMatcher sourceCodeMatcher) {
        return TokenStartEnd.createTokenStartEnd(context -> when(sourceCodeMatcher.matches(context))
                .then(() -> context.parsedCode.feed(context.sourceCode.takeCurrentChar())));
    }

    public static TokenStartEnd excluded(SourceCodeMatcher sourceCodeMatcher) {
        return TokenStartEnd.createTokenStartEnd(context ->
                when(sourceCodeMatcher.matches(context)).then(context.sourceCode::takeCurrentChar));
    }

    public Token parseToken(TokenStartEnd start, TokenContentInString content, TokenStartEnd end,
                            Function<String, Token> constructor) {
        int position = sourceCode.getPosition();
        return when(start.matches(this)).thenReturn(() -> {
            parseContent(content, end);
            Token token = createToken(constructor);
            if (token != null)
                return token.setPositionBegin(position).setPositionEnd(sourceCode.getPosition());
            return null;
        });
    }

    public Token parseToken(TokenStartEnd start, TokenContentInToken content, TokenStartEnd end,
                            Function<List<Token>, Token> constructor) {
        int position = sourceCode.getPosition();
        //TODO same logic with scanner
        return when(start.matches(this)).thenReturn(() -> {
            List<Token> tokens = new ArrayList<>();
            while (!end.matches(this)) {
                Token token = content.getToken(this);
                if (token == null)
                    throw new SyntaxException(sourceCode.getPosition(), "Unexpected token");
                tokens.add(token);
                sourceCode.trimLeft();
            }
            try {
                last = constructor.apply(tokens);
                last.setPositionBegin(position);
                last.setPositionEnd(sourceCode.getPosition());
            } catch (IllegalTokenContentException e) {
                throw new SyntaxException(sourceCode.getPosition() - 1, e.getMessage());
            }
            return last;
        });
    }

    private void parseContent(TokenContentInString content, TokenStartEnd end) {
        content.preprocess(sourceCode);
        while (!end.matches(this))
            parsedCode.feed(content.getChar(sourceCode));
    }

    private Token createToken(Function<String, Token> constructor) {
        try {
            return last = constructor.apply(parsedCode.takeContent());
        } catch (IllegalTokenContentException e) {
            throw new SyntaxException(sourceCode.getPosition() - 1, e.getMessage());
        }
    }
}
