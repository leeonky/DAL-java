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

public class TableRowPrefixNode extends DALNode {
    private static final RowType NO_ROW_KEY = new NoRowType(), INDEX_ROW_KEY = new IndexRowType(),
            PROPERTY_ROW_KEY = new PropertyRowType();
    private final Optional<DALNode> indexOrKey;
    private final Optional<Clause<DALRuntimeContext, DALNode>> rowSchema;
    private final Optional<DALOperator> rowOperator;

    public TableRowPrefixNode(Optional<DALNode> indexOrKey, Optional<Clause<DALRuntimeContext, DALNode>> rowSchema,
                              Optional<DALOperator> rowOperator) {
        this.rowSchema = rowSchema;
        this.rowOperator = rowOperator;
        this.indexOrKey = indexOrKey;
    }

    @Override
    public String inspect() {
        String indexAndSchema = (indexOrKey.map(DALNode::inspect).orElse("") + " " + rowSchema.map(clause ->
                clause.expression(null).inspect()).orElse("")).trim();
        return rowOperator.map(dalOperator -> dalOperator.inspect(indexAndSchema, "").trim()).orElse(indexAndSchema);
    }

    public DALExpression indexAndSchema(RowType rowType, DALNode input, DALOperator defaultOperator, DALNode expectedRow) {
        DALNode inputWithRowKey = rowType.inputWithRowKey(input, indexOrKey);
        return new DALExpression(rowSchema.map(clause -> clause.expression(inputWithRowKey)).orElse(inputWithRowKey),
                rowOperator.orElse(defaultOperator), expectedRow);
    }

    public OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> rowOperator() {
        return procedure -> rowOperator;
    }

    public RowType getRowKeyType() {
        final RowType keyType;
        keyType = indexOrKey.map(dalNode -> {
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