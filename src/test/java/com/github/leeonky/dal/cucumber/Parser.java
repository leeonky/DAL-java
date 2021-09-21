package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.ast.Node;

import java.util.HashMap;
import java.util.Optional;
import java.util.stream.Stream;

public interface Parser {
    Parser NUMBER = new NumberParser(),
            SINGLE_QUOTED_STRING = new SingleQuotedStringParser(),
            DOUBLE_QUOTED_STRING = new DoubleQuotedStringParser(),
            CONST = NUMBER.combines(SINGLE_QUOTED_STRING, DOUBLE_QUOTED_STRING),
            REGEX = new RegexParser(),
            DOT_PROPERTY = new DotPropertyParser();

    Optional<Node> fetch(SourceCode sourceCode);

    default Parser combine(Parser another) {
        return sourceCode -> {
            Optional<Node> optionalNode = fetch(sourceCode);
            if (optionalNode.isPresent())
                return optionalNode;
            return another.fetch(sourceCode);
        };
    }

    default Parser combines(Parser... others) {
        return Stream.of(others).reduce(this, Parser::combine);
    }

    class NumberParser implements Parser {

        @Override
        public Optional<Node> fetch(SourceCode sourceCode) {
            return sourceCode.fetch().map(Token::toConstNumber);
        }
    }

    class SingleQuotedStringParser implements Parser {

        @Override
        public Optional<Node> fetch(SourceCode sourceCode) {
            return sourceCode.fetchBetween('\'', new HashMap<String, Character>() {{
                put("\\\\", '\\');
                put("\\'", '\'');
            }}).map(Token::toConstString);
        }
    }

    class DoubleQuotedStringParser implements Parser {

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

    class RegexParser implements Parser {

        @Override
        public Optional<Node> fetch(SourceCode sourceCode) {
            return sourceCode.fetchBetween('/', new HashMap<String, Character>() {{
                put("\\/", '/');
            }}).map(Token::toRegex);
        }
    }

    class DotPropertyParser implements Parser {

        @Override
        public Optional<Node> fetch(SourceCode sourceCode) {
            return sourceCode.fetchProperty().map(Token::toDotProperty);
        }
    }
}
