package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;
import com.github.leeonky.dal.token.Token;

import static com.github.leeonky.dal.ast.PropertyNode.Type.DOT;

//TODO refactor
class IdentifierPropertyNodeFactory implements NodeFactory {
    @Override
    public Node fetchNode(NodeParser nodeParser) {
        if (nodeParser.tokenStream.currentType() == Token.Type.IDENTIFIER) {
            Token token = nodeParser.tokenStream.pop();
            String[] names = ((String) token.getValue()).split("\\.");
            Node node = new PropertyNode(nodeParser.getThisNode(), names[0], PropertyNode.Type.IDENTIFIER)
                    .setPositionBegin(token.getPositionBegin());
            for (int i = 1; i < names.length; i++)
                node = new PropertyNode(node, names[i], DOT)
                        .setPositionBegin(node.getPositionBegin() + names[i - 1].length() + 1);
            return node;
        }
        return null;
    }
}
