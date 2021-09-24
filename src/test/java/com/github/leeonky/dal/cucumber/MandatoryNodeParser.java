package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;

import static com.github.leeonky.dal.cucumber.NodeParser.CONST;
import static com.github.leeonky.dal.cucumber.NodeParser.PROPERTY;

public interface MandatoryNodeParser {

    MandatoryNodeParser OPERAND = new OperandNodeParser();

    Node fetch(SourceCode sourceCode);

    class OperandNodeParser implements MandatoryNodeParser {
        @Override
        public Node fetch(SourceCode sourceCode) {
            return sourceCode.popUnaryOperator().map(operator -> (Node) new Expression(operator, fetch(sourceCode)))
                    .orElseGet(() -> CONST.combine(PROPERTY).fetch(sourceCode).orElseGet(() -> {
                        if (sourceCode.isBeginning())
                            return InputNode.INSTANCE;
                        throw new SyntaxException("expect a value or expression", sourceCode.getPosition());
                    }));
        }
    }
}
