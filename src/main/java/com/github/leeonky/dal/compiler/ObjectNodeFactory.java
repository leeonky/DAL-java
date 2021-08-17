package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.ObjectNode;

import static com.github.leeonky.dal.token.Token.Type.CLOSING_BRACE;
import static com.github.leeonky.dal.token.Token.Type.OPENING_BRACE;

class ObjectNodeFactory implements NodeFactory {
    private static final NodeFactory propertyNodeFactory = NodeFactory.createPropertyNodeFactory();

    //TODO should contains expression => a: 100+10
    private static final NodeFactory rightOperandNodeFactory = NodeFactory.createRightOperandNodeFactory();

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        return nodeParser.tokenStream.parseBetween(OPENING_BRACE, CLOSING_BRACE, '}', () -> {
            ObjectNode objectNode = new ObjectNode();
            if (nodeParser.tokenStream.hasTokens()) {
                //TODO refactor
                while (nodeParser.tokenStream.hasTokens() && nodeParser.tokenStream.currentType() != CLOSING_BRACE) {
                    Node node = propertyNodeFactory.fetchNode(nodeParser);
                    if (node != null)
                        //TODO use JudgementExpression type
                        objectNode.addJudgements(new Expression(
                                node,
                                nodeParser.tokenStream.pop().toOperator(false),
                                //TODO expression not finished
                                rightOperandNodeFactory.fetchNode(nodeParser)));
                }
            }
            return objectNode;
        });
    }
}