package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenStream;

abstract class NodeFactoryTestBase {

    GivenToken givenToken(Token token, int positionBegin) {
        return new GivenToken().givenToken(token, positionBegin);
    }

    GivenToken givenToken() {
        return new GivenToken();
    }

    protected Node fetchNodeWhenGivenToken(Token token, int positionBegin) {
        return givenToken(token, positionBegin).fetchNodeBy(getDefaultNodeFactory());
    }

    protected Node fetchNodeWhenGivenToken(Token token) {
        return fetchNodeWhenGivenToken(token, 0);
    }

    protected abstract NodeFactory getDefaultNodeFactory();

    class GivenToken {
        protected TokenStream tokenStream = new TokenStream();

        Node fetchNodeBy(NodeFactory factory) {
            return factory.fetchNode(new NodeParser(tokenStream));
        }

        Node fetchNode() {
            return getDefaultNodeFactory().fetchNode(new NodeParser(tokenStream));
        }

        GivenToken givenToken(Token token, int positionBegin) {
            token.setPositionBegin(positionBegin);
            tokenStream.appendToken(token);
            return this;
        }
    }
}
