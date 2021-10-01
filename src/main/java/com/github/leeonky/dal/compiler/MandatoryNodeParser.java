package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;

import static com.github.leeonky.dal.compiler.ExpressionParser.*;
import static com.github.leeonky.dal.compiler.NodeParser.JUDGEMENT;
import static com.github.leeonky.dal.compiler.NodeParser.SINGLE_EVALUABLE;

public interface MandatoryNodeParser {
    MandatoryNodeParser
            OPERAND = new OperandNodeParser(),
            JUDGEMENT_OR_OPERAND = JUDGEMENT.combine(OPERAND),
            EXPRESSION = sourceCode -> BINARY_OPERATOR_EXPRESSION.combine(SCHEMA_EXPRESSION)
                    .defaultPrevious().recursive().fetch(sourceCode, OPERAND.fetch(sourceCode)),
            ARITHMETIC_EXPRESSION = sourceCode -> BINARY_ARITHMETIC_EXPRESSION
                    .defaultPrevious().recursive().fetch(sourceCode, OPERAND.fetch(sourceCode)),
            JUDGEMENT_EXPRESSION_OPERAND = JUDGEMENT.combine(ARITHMETIC_EXPRESSION),
            SCHEMA = sourceCode -> sourceCode.fetchSchemaToken().toSchemaNode();

    Node fetch(SourceCode sourceCode);

    class OperandNodeParser implements MandatoryNodeParser {
        @Override
        public Node fetch(SourceCode sourceCode) {
            Node node = sourceCode.popUnaryOperator().map(operator -> (Node) new Expression(operator, fetch(sourceCode)))
                    .orElseGet(() -> parsePropertyChain(sourceCode, singleEvaluableNode(sourceCode)));
            if (node.isListMapping())
                throw new SyntaxException("element property needed", sourceCode.getPosition());
            return node;
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
