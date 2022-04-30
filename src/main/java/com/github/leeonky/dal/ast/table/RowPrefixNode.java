package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.compiler.DALProcedure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.OperatorParser;

import java.util.Optional;

import static com.github.leeonky.dal.ast.table.RowKeyType.*;

public class RowPrefixNode extends DALNode {
    private final Optional<DALNode> key;
    private final Optional<Clause<DALRuntimeContext, DALNode>> rowSchema;
    private final Optional<DALOperator> rowOperator;

    public RowPrefixNode(Optional<DALNode> key, Optional<Clause<DALRuntimeContext, DALNode>> rowSchema,
                         Optional<DALOperator> rowOperator) {
        this.rowSchema = rowSchema;
        this.rowOperator = rowOperator;
        this.key = key;
    }

    @Override
    public String inspect() {
        String indexAndSchema = (key.map(DALNode::inspect).orElse("") + " " + rowSchema.map(clause ->
                clause.expression(null).inspect()).orElse("")).trim();
        return rowOperator.map(dalOperator -> dalOperator.inspect(indexAndSchema, "").trim()).orElse(indexAndSchema);
    }

    public DALExpression indexAndSchema(RowKeyType rowKeyType, DALNode input, DALOperator defaultOperator, DALNode data) {
        DALNode inputWithRowKey = rowKeyType.inputWithRowKey(input, key);
        return new DALExpression(rowSchema.map(clause -> clause.expression(inputWithRowKey)).orElse(inputWithRowKey),
                rowOperator.orElse(defaultOperator), data);
    }

    public OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> rowOperator() {
        return procedure -> rowOperator;
    }

    public RowKeyType getRowKeyType() {
        final RowKeyType keyType;
        keyType = key.map(dalNode -> {
            if (dalNode instanceof ConstNode)
                return INDEX_ROW_KEY;
            else if (dalNode instanceof DALExpression)
                return PROPERTY_ROW_KEY;
            else
                return NO_ROW_KEY;
        }).orElse(NO_ROW_KEY);
        return keyType;
    }
}