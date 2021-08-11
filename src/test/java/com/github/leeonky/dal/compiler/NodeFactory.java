package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.token.Token;

public interface NodeFactory {

    static NodeFactory createConstNodeFactory() {
        return nodeParser -> {
            if (nodeParser.tokenStream.hasTokens() && nodeParser.tokenStream.currentType() == Token.Type.CONST_VALUE) {
                Token token = nodeParser.tokenStream.pop();
                return new ConstNode(token.getValue())
                        .setPositionBegin(token.getPositionBegin());
            }
            return null;
        };
    }

    static NodeFactory createEvaluableNodeFactory() {
        return createConstNodeFactory();
    }

    Node fetchNode(NodeParser nodeParser);
}
