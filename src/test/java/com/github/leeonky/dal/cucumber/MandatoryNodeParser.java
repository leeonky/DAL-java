package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;

import static com.github.leeonky.dal.cucumber.ExpressionParser.*;
import static com.github.leeonky.dal.cucumber.NodeParser.JUDGEMENT;
import static com.github.leeonky.dal.cucumber.NodeParser.SINGLE_EVALUABLE;

public interface MandatoryNodeParser {
    MandatoryNodeParser
            OPERAND = new OperandNodeParser(),
            JUDGEMENT_OR_OPERAND = JUDGEMENT.combine(OPERAND),
            EXPRESSION = sourceCode -> BINARY_OPERATOR_EXPRESSION.combine(SCHEMA_EXPRESSION)
                    .defaultPrevious().recursive().fetch(sourceCode, OPERAND.fetch(sourceCode));

    Node fetch(SourceCode sourceCode);

    class OperandNodeParser implements MandatoryNodeParser {
        @Override
        public Node fetch(SourceCode sourceCode) {
            return sourceCode.popUnaryOperator().map(operator -> (Node) new Expression(operator, fetch(sourceCode)))
                    .orElseGet(() -> parsePropertyChain(sourceCode, singleEvaluableNode(sourceCode)));
        }

        private Node singleEvaluableNode(SourceCode sourceCode) {
            return SINGLE_EVALUABLE.fetch(sourceCode).orElseGet(() -> {
                if (sourceCode.isBeginning())
                    return InputNode.INSTANCE;
                throw new SyntaxException("expect a value or expression", sourceCode.getPosition());
            });
        }

        private Node parsePropertyChain(SourceCode sourceCode, Node instanceNode) {
            return EXPLICIT_PROPERTY.recursiveCompile(sourceCode, instanceNode, this::parsePropertyChain);
        }
    }
}
