package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.compiler.DALProcedure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.OperatorParser;

import java.util.Optional;

public class HeaderNode extends DALNode {
    private final DALNode property;
    private final Optional<DALOperator> operator;

    public HeaderNode(DALNode property, Optional<DALOperator> operator) {
        this.property = property;
        this.operator = operator;
    }

    public DALNode getProperty() {
        return property;
    }

    public OperatorParser<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator,
            DALProcedure> headerOperator() {
        return procedure -> operator;
    }

    @Override
    public String inspect() {
        String property = this.property.inspect();
        return operator.map(operator -> operator.inspect(property, "").trim()).orElse(property);
    }
}
