package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.parser.TokenParser;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenFactory;
import com.github.leeonky.dal.token.TokenStream;

import static org.junit.jupiter.api.Assertions.assertThrows;

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

    protected SyntaxException invalidSyntaxToken(FetchNode fetchNode) {
        return assertThrows(SyntaxException.class, fetchNode::fetchNode);
    }

    public abstract class FetchNode {
        public abstract Node fetchNodeBy(NodeFactory factory);

        public Node fetchNode() {
            return fetchNodeBy(getDefaultNodeFactory());
        }
    }

    public class GivenToken extends FetchNode {
        protected TokenStream tokenStream = new TokenStream();

        @Override
        public Node fetchNodeBy(NodeFactory factory) {
            return factory.fetchNode(new NodeParser(tokenStream));
        }

        public GivenToken givenToken(Token token, int positionBegin) {
            token.setPositionBegin(positionBegin).setPositionEnd(positionBegin);
            tokenStream.appendToken(token);
            return this;
        }

        public GivenToken givenToken(Token token) {
            return givenToken(token, 0);
        }
    }

    public class GivenCode extends FetchNode {
        private final String code;

        public GivenCode(String code) {
            this.code = code;
        }

        @Override
        public Node fetchNodeBy(NodeFactory factory) {
            return factory.fetchNode(new NodeParser(TokenFactory.createDALTokenFactory()
                    .fetchToken(new TokenParser(new SourceCode(code))).getTokenStream()));
        }
    }
}
