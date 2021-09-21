package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.RegexNode;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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

    NodeParser NUMBER = sourceCode -> sourceCode.fetch().map(Token::toConstNumber),
            SINGLE_QUOTED_STRING = sourceCode -> sourceCode.fetchBetween('\'', '\'',
                    create(ConstNode::new), () -> sourceCode.escapedPop(SINGLE_QUOTED_ESCAPES)),
            DOUBLE_QUOTED_STRING = sourceCode -> sourceCode.fetchBetween('"', '"',
                    create(ConstNode::new), () -> sourceCode.escapedPop(DOUBLE_QUOTED_ESCAPES)),
            CONST_TRUE = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.TRUE).map(Token::toConstTrue),
            CONST_FALSE = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.FALSE).map(Token::toConstFalse),
            CONST_NULL = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.NULL).map(Token::toConstNull),
            CONST = NUMBER.combines(SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL),
            REGEX = sourceCode -> sourceCode.fetchBetween('/', '/',
                    create(RegexNode::new), () -> sourceCode.escapedPop(REGEX_ESCAPES));

    NodeParser IDENTITY_PROPERTY = sourceCode -> sourceCode.fetchIdentity().map(Token::toIdentityProperty);

    Optional<Node> fetch(SourceCode sourceCode);

    default NodeParser combine(NodeParser another) {
        return sourceCode -> {
            Optional<Node> optionalNode = fetch(sourceCode);
            if (optionalNode.isPresent())
                return optionalNode;
            return another.fetch(sourceCode);
        };
    }

    default NodeParser combines(NodeParser... others) {
        return Stream.of(others).reduce(this, NodeParser::combine);
    }

    static <T> Function<List<Character>, T> create(Function<String, T> factory) {
        return chars -> factory.apply(chars.stream().map(String::valueOf).collect(Collectors.joining("")));
    }
}
