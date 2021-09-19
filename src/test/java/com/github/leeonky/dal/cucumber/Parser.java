package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.ast.Node;

import java.util.Optional;

public interface Parser {
    Parser CONST = new ConstParser();

    Optional<Node> fetch(SourceCode sourceCode);

    class ConstParser implements Parser {

        @Override
        public Optional<Node> fetch(SourceCode sourceCode) {
            return sourceCode.fetch().map(Token::toConstNumber);
        }
    }
}
