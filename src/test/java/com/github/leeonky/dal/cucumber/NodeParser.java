package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.ast.Node;

import java.util.HashMap;
import java.util.Optional;
import java.util.stream.Stream;

public interface NodeParser {
    NodeParser NUMBER = new NumberNodeParser();
    NodeParser SINGLE_QUOTED_STRING = new SingleQuotedStringNodeParser();
    NodeParser DOUBLE_QUOTED_STRING = new DoubleQuotedStringNodeParser();
    NodeParser CONST_TRUE = sourceCode -> sourceCode.fetchKeyWord("true").map(Token::toConstTrue);
    NodeParser CONST_FALSE = sourceCode -> sourceCode.fetchKeyWord("false").map(Token::toConstFalse);
    NodeParser CONST_NULL = sourceCode -> sourceCode.fetchKeyWord("null").map(Token::toConstNull);
    NodeParser CONST = NUMBER.combines(SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING, CONST_TRUE, CONST_FALSE, CONST_NULL);
    NodeParser REGEX = new RegexNodeParser();
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

    class NumberNodeParser implements NodeParser {

        @Override
        public Optional<Node> fetch(SourceCode sourceCode) {
            return sourceCode.fetch().map(Token::toConstNumber);
        }
    }

    class SingleQuotedStringNodeParser implements NodeParser {

        @Override
        public Optional<Node> fetch(SourceCode sourceCode) {
            return sourceCode.fetchBetween('\'', new HashMap<String, Character>() {{
                put("\\\\", '\\');
                put("\\'", '\'');
            }}).map(Token::toConstString);
        }
    }

    class DoubleQuotedStringNodeParser implements NodeParser {

        @Override
        public Optional<Node> fetch(SourceCode sourceCode) {
            return sourceCode.fetchBetween('"', new HashMap<String, Character>() {{
                put("\\\\", '\\');
                put("\\n", '\n');
                put("\\t", '\t');
                put("\\\"", '"');
            }}).map(Token::toConstString);
        }
    }

    class RegexNodeParser implements NodeParser {

        @Override
        public Optional<Node> fetch(SourceCode sourceCode) {
            return sourceCode.fetchBetween('/', new HashMap<String, Character>() {{
                put("\\/", '/');
            }}).map(Token::toRegex);
        }
    }

    class DotPropertyNodeParser implements NodeParser {

        @Override
        public Optional<Node> fetch(SourceCode sourceCode) {
            return sourceCode.fetchProperty().map(Token::toDotProperty);
        }
    }
}