package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.*;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.github.leeonky.dal.cucumber.ExpressionParser.EXPLICIT_PROPERTY;
import static com.github.leeonky.dal.cucumber.MandatoryNodeParser.EXPRESSION;
import static com.github.leeonky.dal.cucumber.SourceCode.FetchBy.BY_CHAR;
import static com.github.leeonky.dal.cucumber.SourceCode.FetchBy.BY_NODE;

public interface NodeParser {
    EscapeChars SINGLE_QUOTED_ESCAPES = new EscapeChars()
            .escape("\\\\", '\\')
            .escape("\\'", '\'');
    EscapeChars DOUBLE_QUOTED_ESCAPES = new EscapeChars()
            .escape("\\\\", '\\')
            .escape("\\n", '\n')
            .escape("\\t", '\t')
            .escape("\\\"", '"');
    EscapeChars REGEX_ESCAPES = new EscapeChars()
            .escape("\\/", '/');

    NodeParser NUMBER = sourceCode -> sourceCode.fetchNumber().map(Token::toConstNumber),
            INTEGER = sourceCode -> sourceCode.fetchInteger().map(Token::toConstInteger),
            SINGLE_QUOTED_STRING = sourceCode -> sourceCode.fetchElements(BY_CHAR, '\'', '\'',
                    create(ConstNode::new), () -> sourceCode.escapedPop(SINGLE_QUOTED_ESCAPES)),
            DOUBLE_QUOTED_STRING = sourceCode -> sourceCode.fetchElements(BY_CHAR, '"', '"',
                    create(ConstNode::new), () -> sourceCode.escapedPop(DOUBLE_QUOTED_ESCAPES)),
            CONST_TRUE = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.TRUE).map(Token::toConstTrue),
            CONST_FALSE = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.FALSE).map(Token::toConstFalse),
            CONST_NULL = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.NULL).map(Token::toConstNull),
            CONST = NUMBER.combines(SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL),
            REGEX = sourceCode -> sourceCode.fetchElements(BY_CHAR, '/', '/',
                    create(RegexNode::new), () -> sourceCode.escapedPop(REGEX_ESCAPES)),
            INTEGER_OR_STRING_INDEX = INTEGER.combines(SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING),
            PARENTHESES = sourceCode -> sourceCode.fetchNode('(', ')',
                    ParenthesesNode::new, EXPRESSION, "expect a value or expression"),
            IDENTITY_PROPERTY = sourceCode -> sourceCode.fetchIdentityProperty().map(Token::toIdentityProperty),
            PROPERTY = EXPLICIT_PROPERTY.defaultInputNode().combine(IDENTITY_PROPERTY),
            SINGLE_EVALUABLE = CONST.combines(PROPERTY, PARENTHESES),
            OBJECT = sourceCode -> sourceCode.fetchElements(BY_NODE, '{', '}',
                    ObjectNode::new, () -> (Expression) EXPRESSION.fetch(sourceCode)),
            LIST = sourceCode -> sourceCode.fetchElements(BY_NODE, '[', ']',
                    ListNode::new, () -> (Expression) EXPRESSION.fetch(sourceCode)),
            WILDCARD = sourceCode -> sourceCode.fetchWord("*").map(Token::toWildcardNode),
            JUDGEMENT = REGEX.combines(OBJECT, LIST, WILDCARD);

    Optional<Node> fetch(SourceCode sourceCode);

    default NodeParser combine(NodeParser another) {
        return sourceCode -> {
            Optional<Node> optionalNode = fetch(sourceCode);
            if (optionalNode.isPresent())
                return optionalNode;
            return another.fetch(sourceCode);
        };
    }

    default MandatoryNodeParser combine(MandatoryNodeParser mandatoryNodeParser) {
        return sourceCode -> fetch(sourceCode).orElseGet(() -> mandatoryNodeParser.fetch(sourceCode));
    }

    default NodeParser combines(NodeParser... others) {
        return Stream.of(others).reduce(this, NodeParser::combine);
    }

    static <T> Function<List<Character>, T> create(Function<String, T> factory) {
        return chars -> factory.apply(chars.stream().map(String::valueOf).collect(Collectors.joining("")));
    }

    default MandatoryNodeParser toMandatoryNodeParser(String message) {
        return sourceCode -> fetch(sourceCode).orElseThrow(() -> new SyntaxException(message, sourceCode.getPosition()));
    }
}
