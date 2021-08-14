package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.ObjectNode;

import static com.github.leeonky.dal.token.Token.Type.CLOSING_BRACE;
import static com.github.leeonky.dal.token.Token.Type.OPENING_BRACE;

class ObjectNodeFactory implements NodeFactory {

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        return nodeParser.tokenStream.parseBetween(OPENING_BRACE, CLOSING_BRACE, '}', ObjectNode::new);
    }
}
