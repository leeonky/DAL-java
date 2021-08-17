package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ListNode;
import com.github.leeonky.dal.ast.Node;

import static com.github.leeonky.dal.token.Token.Type.CLOSING_BRACKET;
import static com.github.leeonky.dal.token.Token.Type.OPENING_BRACKET;

public class ListNodeFactory implements NodeFactory {
    //TODO should contains expression => a: 100+10
    private static final NodeFactory rightOperandNodeFactory = NodeFactory.createRightOperandNodeFactory();

    @Override
    public Node fetchNode(NodeParser nodeParser) {
        return nodeParser.tokenStream.parseBetween(OPENING_BRACKET, CLOSING_BRACKET, ']', () -> {
            ListNode listNode = new ListNode();
//            if (nodeParser.tokenStream.hasTokens()) {
//                //TODO refactor
//                while (nodeParser.tokenStream.hasTokens() && nodeParser.tokenStream.currentType() != CLOSING_BRACKET) {
//                    Node node = propertyNodeFactory.fetchNode(nodeParser);
//                    if (node != null)
//                        //TODO use JudgementExpression type
//                        listNode.addJudgements(new Expression(
//                                node,
//                                nodeParser.tokenStream.pop().toOperator(false),
//                                //TODO expression not finished
//                                rightOperandNodeFactory.fetchNode(nodeParser)));
//                }
//            }
            return listNode;
        });
    }
}
