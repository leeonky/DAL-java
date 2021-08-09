package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.token.*;

import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import static com.github.leeonky.dal.parser.SourceCodeMatcher.createSourceCodeMatcher;
import static com.github.leeonky.dal.util.IfThenFactory.when;
import static com.github.leeonky.dal.util.IfThenFactory.whenNonNull;

public class TokenParser {
    public static final SourceCodeMatcher DIGITAL = oneCharMatcher(Scanner.DIGITAL_CHAR::contains);
    public static final SourceCodeMatcher DELIMITER = oneCharMatcher(Scanner.TOKEN_DELIMITER::contains);
    public static final SourceCodeMatcher OPERATOR = oneCharMatcher(Scanner.OPERATOR_CHAR::contains);
    public static final SourceCodeMatcher AFTER_TOKEN_MATCHES = createSourceCodeMatcher(parser ->
            parser.tokenStream.isLastTokenOperatorMatches());
    public static final SourceCodeMatcher AFTER_OPERATOR_MATCHES = createSourceCodeMatcher(parser ->
            parser.parsedCode.isOperatorMatches());
    public static final SourceCodeMatcher ANY_CHARACTERS = createSourceCodeMatcher(parser ->
            parser.sourceCode.notEnd());
    public static final TokenStartEnd END_OF_CODE = TokenStartEnd.createTokenStartEnd(parser ->
            !parser.sourceCode.notEnd());
    private final SourceCode sourceCode;
    private final TokenStream tokenStream;
    private final ParsedCode parsedCode = new ParsedCode();

    public TokenParser(SourceCode sourceCode) {
        this(sourceCode, new TokenStream());
    }

    public TokenParser(SourceCode sourceCode, TokenStream tokenStream) {
        this.sourceCode = sourceCode;
        this.tokenStream = tokenStream;
    }

    public static SourceCodeMatcher oneCharMatcher(Predicate<Character> predicate) {
        return createSourceCodeMatcher(parser -> predicate.test(parser.sourceCode.currentChar()));
    }

    public static SourceCodeMatcher CHARACTER(char a) {
        return oneCharMatcher(character -> Objects.equals(character, a));
    }

    public static TokenStartEnd included(SourceCodeMatcher sourceCodeMatcher) {
        return TokenStartEnd.createTokenStartEnd(parser -> when(sourceCodeMatcher.matches(parser))
                .then(() -> parser.parsedCode.feed(parser.sourceCode.takeCurrentChar())));
    }

    public static TokenStartEnd excluded(SourceCodeMatcher sourceCodeMatcher) {
        return TokenStartEnd.createTokenStartEnd(parser ->
                when(sourceCodeMatcher.matches(parser)).then(parser.sourceCode::takeCurrentChar));
    }

    Token parseToken(TokenStartEnd start, TokenContentInToken content, TokenStartEnd end,
                     Function<TokenStream, Token> constructor) {
        //TODO same logic with scanner
        return parseTokenWithSourceCodePosition(start, () -> {
            TokenParser subParser = createSubContext();
            parseTokenStreamContent(content, end, subParser);
            return constructor.apply(subParser.tokenStream);
        });
    }

    Token parseToken(TokenStartEnd start, TokenContentInString content, TokenStartEnd end,
                     Function<String, Token> constructor) {
        return parseTokenWithSourceCodePosition(start, () -> {
            parseStringContent(content, end);
            return constructor.apply(parsedCode.takeContent());
        });
    }

    private void parseTokenStreamContent(TokenContentInToken content, TokenStartEnd end, TokenParser subContext) {
        while (!end.matches(subContext)) {
            if (content.getToken(subContext) == null)
                throw new SyntaxException(sourceCode.getPosition(), "Unexpected token");
            sourceCode.trimLeft();
        }
    }

    private TokenParser createSubContext() {
        TokenParser subContext = new TokenParser(sourceCode);
        subContext.parsedCode.feed(parsedCode);
        return subContext;
    }

    private void parseStringContent(TokenContentInString content, TokenStartEnd end) {
        content.preprocess(sourceCode);
        while (!end.matches(this))
            parsedCode.feed(content.getChar(sourceCode));
    }

    private Token parseTokenWithSourceCodePosition(TokenStartEnd start, Supplier<Token> supplier) {
        int position = sourceCode.getPosition();
        return when(start.matches(this)).thenReturn(() -> (Token)
                whenNonNull(getToken(supplier))
                        .canReturn(token -> tokenStream.appendToken(token)
                                .setPositionBegin(position)
                                .setPositionEnd(sourceCode.getPosition())
                        ).orElse(() -> sourceCode.seek(position - sourceCode.getPosition())));
    }

    private Token getToken(Supplier<Token> supplier) {
        try {
            return supplier.get();
        } catch (IllegalTokenContentException e) {
            throw new SyntaxException(sourceCode.getPosition() - 1, e.getMessage());
        }
    }
}
