package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.ast.Node;

import java.util.Optional;
import java.util.stream.Stream;

import static com.github.leeonky.dal.Constants.DOUBLE_QUOTED_STRING_ESCAPES;
import static com.github.leeonky.dal.Constants.SINGLE_QUOTED_STRING_ESCAPES;

public interface NodeParser {

    NodeParser NUMBER = sourceCode -> sourceCode.fetch().map(Token::toConstNumber),
            SINGLE_QUOTED_STRING = sourceCode -> sourceCode.fetchBetween('\'',
                    SINGLE_QUOTED_STRING_ESCAPES).map(Token::toConstString),
            DOUBLE_QUOTED_STRING = sourceCode -> sourceCode.fetchBetween('"',
                    DOUBLE_QUOTED_STRING_ESCAPES).map(Token::toConstString),
            CONST_TRUE = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.TRUE).map(Token::toConstTrue),
            CONST_FALSE = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.FALSE).map(Token::toConstFalse),
            CONST_NULL = sourceCode -> sourceCode.fetchWord(Constants.KeyWords.NULL).map(Token::toConstNull),
            CONST = NUMBER.combines(SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL),
            REGEX = sourceCode -> sourceCode.fetchBetween('/', Constants.REGEX_ESCAPES).map(Token::toRegex);

    NodeParser DOT_PROPERTY = new DotPropertyNodeParser();

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

    class DotPropertyNodeParser implements NodeParser {

        @Override
        public Optional<Node> fetch(SourceCode sourceCode) {
            return sourceCode.fetchProperty().map(Token::toDotProperty);
        }
    }
}
