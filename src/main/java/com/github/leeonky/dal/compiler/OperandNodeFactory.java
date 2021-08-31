package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;

import static java.util.Optional.ofNullable;

class OperandNodeFactory implements NodeFactory {

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        if (!nodeParser.tokenStream.hasTokens())
            return giveDefault(nodeParser);

        return nodeParser.tokenStream.tryFetchUnaryOperator()
                .map(token -> (Node) new Expression(new ConstNode(null),
                        token.toOperator(true), fetchNode(nodeParser)))
                .orElseGet(() -> parsePropertyChain(nodeParser,
                        ofNullable(NodeFactories.SINGLE_EVALUABLE.fetchNode(nodeParser))
                                .orElseGet(() -> giveDefault(nodeParser))));
    }

    private Node giveDefault(NodeParser nodeParser) {
        if (nodeParser.tokenStream.isFromBeginning())
            return InputNode.INSTANCE;
        throw new SyntaxException(nodeParser.tokenStream.getPosition(), "expect a value or expression");
    }

    private Node parsePropertyChain(NodeParser nodeParser, Node node) {
        if (nodeParser.setThis(node) != null && nodeParser.tokenStream.hasTokens()) {
            Node next = NodeFactories.EXPLICIT_PROPERTY.fetchNode(nodeParser);
            if (next != null)
                return parsePropertyChain(nodeParser, next);
        }
        nodeParser.setThis(InputNode.INSTANCE);
        return node;
    }
}
