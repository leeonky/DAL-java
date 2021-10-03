package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;

import java.util.Optional;

import static com.github.leeonky.dal.compiler.ExpressionParser.*;
import static com.github.leeonky.dal.compiler.NodeParser.*;

public interface MandatoryNodeParser {
    MandatoryNodeParser
            SINGLE_EVALUABLE = sourceCode -> CONST.combines(PROPERTY, PARENTHESES).fetch(sourceCode)
            .orElseGet(() -> {
                if (sourceCode.isBeginning())
                    return InputNode.INSTANCE;
                throw new SyntaxException("expect a value or expression", sourceCode.getPosition());
            }),
            OPERAND = new MandatoryNodeParser() {
                @Override
                public Node fetch(SourceCode sourceCode) {
                    return sourceCode.popUnaryOperator().map(operator -> (Node) new Expression(operator, fetch(sourceCode)))
                            .orElseGet(() -> SINGLE_EVALUABLE.recursive(EXPLICIT_PROPERTY).fetch(sourceCode)).avoidListMapping();
                }
            },
            JUDGEMENT_OR_OPERAND = JUDGEMENT.combine(OPERAND),
            EXPRESSION = sourceCode -> BINARY_OPERATOR_EXPRESSION.combine(SCHEMA_EXPRESSION)
                    .defaultPreviousRecursive().fetch(sourceCode, OPERAND.fetch(sourceCode)),
            ARITHMETIC_EXPRESSION = sourceCode -> BINARY_ARITHMETIC_EXPRESSION
                    .defaultPreviousRecursive().fetch(sourceCode, OPERAND.fetch(sourceCode)),
            JUDGEMENT_EXPRESSION_OPERAND = JUDGEMENT.combine(ARITHMETIC_EXPRESSION),
            SCHEMA = sourceCode -> sourceCode.fetchSchemaToken().toSchemaNode();

    Node fetch(SourceCode sourceCode);

    default MandatoryNodeParser recursive(ExpressionParser expressionParser) {
        return sourceCode -> {
            Node node = fetch(sourceCode);
            Optional<Node> optionalNode = expressionParser.fetch(sourceCode, node);
            while (optionalNode.isPresent()) {
                node = optionalNode.get();
                optionalNode = expressionParser.fetch(sourceCode, node);
            }
            return node;
        };
    }
}
