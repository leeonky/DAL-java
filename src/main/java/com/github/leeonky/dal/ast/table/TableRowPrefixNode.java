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
    private static final RowType DEFAULT_INDEX = new DefaultIndexRowType(), SPECIFY_INDEX = new SpecifyIndexRowType(),
            SPECIFY_PROPERTY = new SpecifyPropertyRowType();
    private final Optional<DALNode> indexOrProperty;
    private final Optional<Clause<DALRuntimeContext, DALNode>> rowSchema;
    private final Optional<DALOperator> rowOperator;

    public TableRowPrefixNode(Optional<DALNode> indexOrProperty, Optional<Clause<DALRuntimeContext, DALNode>> rowSchema,
                              Optional<DALOperator> rowOperator) {
        this.rowSchema = rowSchema;
        this.rowOperator = rowOperator;
        this.indexOrProperty = indexOrProperty;
    }

    @Override
    public String inspect() {
        String indexAndSchema = (indexOrProperty.map(DALNode::inspect).orElse("") + " " + rowSchema.map(clause ->
                clause.expression(null).inspect()).orElse("")).trim();
        return rowOperator.map(dalOperator -> dalOperator.inspect(indexAndSchema, "").trim()).orElse(indexAndSchema);
    }

    public DALExpression makeExpressionWithOptionalIndexAndSchema(RowType rowType, DALNode input,
                                                                  DALOperator defaultOperator, DALNode expectedRow) {
        DALNode rowAccessor = rowType.rowAccessor(input, indexOrProperty);
        return new DALExpression(rowSchema.map(clause -> clause.expression(rowAccessor)).orElse(rowAccessor),
                rowOperator.orElse(defaultOperator), expectedRow);
    }

    public OperatorParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> rowOperator() {
        return procedure -> rowOperator;
    }

    public RowType resolveRowType() {
        final RowType rowType;
        rowType = indexOrProperty.map(dalNode -> {
            if (dalNode instanceof ConstNode)
                return SPECIFY_INDEX;
            else if (dalNode instanceof DALExpression)
                return SPECIFY_PROPERTY;
            else
                return DEFAULT_INDEX;
        }).orElse(DEFAULT_INDEX);
        return rowType;
    }
}