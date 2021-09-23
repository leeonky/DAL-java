package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;

import static com.github.leeonky.dal.cucumber.NodeParser.CONST;
import static com.github.leeonky.dal.cucumber.NodeParser.PROPERTY;

public interface MandatoryNodeParser {

    MandatoryNodeParser LEFT_OPERAND = new LeftOperandNodeParser();

    Node fetch(SourceCode sourceCode);

    class LeftOperandNodeParser implements MandatoryNodeParser {
        @Override
        public Node fetch(SourceCode sourceCode) {
            return CONST.combine(PROPERTY).fetch(sourceCode).orElse(InputNode.INSTANCE);
        }
    }
}
