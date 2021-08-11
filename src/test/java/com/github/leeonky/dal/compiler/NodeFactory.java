package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.token.Token;

public interface NodeFactory {

    static NodeFactory createConstNodeFactory() {
        return compiler -> {
            if (compiler.tokenStream.hasTokens() && compiler.tokenStream.currentType() == Token.Type.CONST_VALUE) {
                Token token = compiler.tokenStream.pop();
                return new ConstNode(token.getValue())
                        .setPositionBegin(token.getPositionBegin());
            }
            return null;
        };
    }

    Node fetchNode(Compiler compiler);
}
