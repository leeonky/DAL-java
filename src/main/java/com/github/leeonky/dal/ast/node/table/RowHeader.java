package com.github.leeonky.dal.ast.node.table;

import com.github.leeonky.dal.ast.node.DALExpression;
import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.ast.node.InputNode;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.compiler.DALProcedure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.Operator;
import com.github.leeonky.interpreter.OperatorParser;

import java.util.Optional;

import static com.github.leeonky.dal.ast.node.DALExpression.expression;
import static com.github.leeonky.util.function.Extension.getFirstPresent;

public class RowHeader extends DALNode {
    public static final RowType DEFAULT_INDEX = new DefaultIndexRowType();
    public static final RowType SPECIFY_INDEX = new SpecifyIndexRowType();
    public static final RowType SPECIFY_PROPERTY = new SpecifyPropertyRowType();
    private final Optional<DALNode> indexOrProperty;
    private final Optional<Clause<DALNode>> clause;
    private final Optional<DALOperator> rowOperator;

    public RowHeader(Optional<DALNode> indexOrProperty, Optional<Clause<DALNode>> clause,
                     Optional<DALOperator> rowOperator) {
        this.rowOperator = rowOperator;
        this.indexOrProperty = indexOrProperty;
        this.clause = clause;
    }

    @Override
    public String inspect() {
        String indexAndSchema = (indexOrProperty.map(DALNode::inspect).orElse("") + " " + clause.map(clause ->
                clause.expression(null).inspect()).orElse("")).trim();
        return rowOperator.map(dalOperator -> dalOperator.inspect(indexAndSchema, "").trim()).orElse(indexAndSchema);
    }

    public DALNode makeExpressionWithOptionalIndexAndSchema(RowType rowType, DALNode input,
                                                            DALOperator defaultOperator, DALNode expectedRow) {
        DALNode rowAccessor = rowType.constructAccessingRowNode(input, indexOrProperty);
        return expression(clause.map(clause -> clause.expression(rowAccessor)).orElse(rowAccessor),
                rowOperator.orElse(defaultOperator), expectedRow);
    }

    public OperatorParser<DALRuntimeContext, DALNode, DALOperator, DALProcedure, DALExpression> operator() {
        return procedure -> rowOperator;
    }

    public RowType resolveRowType() {
        return indexOrProperty.map(DALNode::guessTableHeaderType).orElse(DEFAULT_INDEX);
    }

    public Optional<Integer> position() {
        return getFirstPresent(() -> indexOrProperty.map(DALNode::getPositionBegin),
                () -> clause.map(c -> ((DALExpression) c.expression(InputNode.INPUT_NODE)).operator().getPosition()),
                () -> rowOperator.map(Operator::getPosition));
    }
}