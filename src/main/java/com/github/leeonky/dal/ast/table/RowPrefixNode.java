package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.compiler.DALProcedure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.OperatorParser;

import java.util.Optional;

public class RowPrefixNode extends DALNode {
    private final Optional<Clause<DALRuntimeContext, DALNode>> rowSchema;
    private final Optional<DALOperator> rowOperator;
    private final RowKey rowKey;

    public RowPrefixNode(Optional<Integer> index, Optional<Clause<DALRuntimeContext, DALNode>> rowSchema,
                         Optional<DALOperator> rowOperator) {
        rowKey = new RowKey(index);
        this.rowSchema = rowSchema;
        this.rowOperator = rowOperator;
    }

    public boolean hasIndex() {
        return rowKey.index.isPresent();
    }

    @Override
    public String inspect() {
        String indexAndSchema = (rowKey.inspect() + " " + rowSchema.map(clause ->
                clause.expression(null).inspect()).orElse("")).trim();
        return rowOperator.map(dalOperator -> dalOperator.inspect(indexAndSchema, "").trim()).orElse(indexAndSchema);
    }

    public DALExpression indexAndSchema(DALNode input, DALOperator defaultOperator, DALNode data) {
        DALNode inputWithRowKey = rowKey.inputWithRowKey(input);
        return new DALExpression(rowSchema.map(clause -> clause.expression(inputWithRowKey)).orElse(inputWithRowKey),
                rowOperator.orElse(defaultOperator), data);
    }

    public OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> rowOperator() {
        return procedure -> rowOperator;
    }

    public RowKey.RowKeyType getRowKeyType() {
        return rowKey.toType();
    }
}