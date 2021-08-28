package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.token.Token;

import static com.github.leeonky.dal.token.Token.Type.CLOSING_BRACKET;
import static com.github.leeonky.dal.token.Token.Type.OPENING_BRACKET;

//TODO set position
public class ListNodeFactory implements NodeFactory {

    //TODO should contains expression => a: 100+10
    @Override
    public Node fetchNode(NodeParser nodeParser) {
        return nodeParser.tokenStream.parseBetween(OPENING_BRACKET, CLOSING_BRACKET, ']', () -> {
            ListNode listNode = new ListNode();
            Token operatorToken = nodeParser.operators.isEmpty() ? Token.operatorToken(":") : nodeParser.operators.getFirst();
            if (nodeParser.tokenStream.hasTokens()) {
                int index = 0;
                //TODO refactor
                while (nodeParser.tokenStream.hasTokens() && nodeParser.tokenStream.currentType() != CLOSING_BRACKET) {
                    //TODO use JudgementExpression type
                    listNode.addJudgements(new Expression(
                            //TODO access element
                            new PropertyNode(InputNode.INSTANCE, index++),
                            //TODO hardcode
                            operatorToken.toOperator(false),
                            //TODO expression not finished
                            NodeFactories.RIGHT_OPERAND.fetchNode(nodeParser)));
                }
            }
            return listNode;
        });
    }
}
