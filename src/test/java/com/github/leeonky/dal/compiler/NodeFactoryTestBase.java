package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.parser.TokenParser;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenFactory;
import com.github.leeonky.dal.token.TokenStream;

public abstract class NodeFactoryTestBase {
    public GivenCode givenCode(String code) {
        return new GivenCode(code);
    }

    public GivenToken givenToken(Token token, int positionBegin) {
        return givenToken().givenToken(token, positionBegin);
    }

    public GivenToken givenToken(Token token) {
        return givenToken().givenToken(token, 0);
    }

    public GivenToken givenToken() {
        return new GivenToken();
    }

    protected Node fetchNodeWhenGivenToken(Token token, int positionBegin) {
        return givenToken(token, positionBegin).fetchNodeBy(getDefaultNodeFactory());
    }

    protected Node fetchNodeWhenGivenToken(Token token) {
        return fetchNodeWhenGivenToken(token, 0);
    }

    protected abstract NodeFactory getDefaultNodeFactory();

    public class GivenToken {
        protected TokenStream tokenStream = new TokenStream();

        public Node fetchNodeBy(NodeFactory factory) {
            return factory.fetchNode(new NodeParser(tokenStream));
        }

        public Node fetchNode() {
            return fetchNodeBy(getDefaultNodeFactory());
        }

        public GivenToken givenToken(Token token, int positionBegin) {
            token.setPositionBegin(positionBegin);
            tokenStream.appendToken(token);
            return this;
        }

        public GivenToken givenToken(Token token) {
            return givenToken(token, 0);
        }
    }

    public class GivenCode {
        private final String code;

        public GivenCode(String code) {
            this.code = code;
        }

        public Node fetchNodeBy(NodeFactory factory) {
            return factory.fetchNode(new NodeParser(TokenFactory.createDALTokenFactory()
                    .fetchToken(new TokenParser(new SourceCode(code))).getTokenStream()));
        }

        public Node fetchNode() {
            return fetchNodeBy(getDefaultNodeFactory());
        }
    }
}
