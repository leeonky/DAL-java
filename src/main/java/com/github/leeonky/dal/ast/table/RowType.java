package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;

import java.util.Collections;
import java.util.Comparator;
import java.util.Optional;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.SymbolNode.Type.BRACKET;
import static com.github.leeonky.dal.ast.table.SpecifyIndexRowType.indexToExpression;
import static com.github.leeonky.interpreter.IfThenFactory.when;
import static java.util.stream.Collectors.toList;

abstract class RowType {

    public abstract RowType merge(RowType another);

    protected RowType mergeBy(SpecifyIndexRowType specifyIndexRowType) {
        throw new IllegalArgumentException();
    }

    protected RowType mergeBy(DefaultIndexRowType defaultIndexRowType) {
        throw new IllegalArgumentException();
    }

    protected RowType mergeBy(SpecifyPropertyRowType specifyPropertyRowType) {
        throw new IllegalArgumentException();
    }

    public abstract DALNode constructVerificationNode(Data actual, Stream<Clause<DALRuntimeContext, DALNode>> rowClauses,
                                                      Comparator<Object> comparator);

    public DALNode constructAccessingRowNode(DALNode input, Optional<DALNode> indexOrKey) {
        return input;
    }
}

class EmptyTableRowType extends RowType {

    @Override
    public RowType merge(RowType another) {
        return another;
    }

    @Override
    protected RowType mergeBy(SpecifyIndexRowType specifyIndexRowType) {
        return this;
    }

    @Override
    protected RowType mergeBy(DefaultIndexRowType defaultIndexRowType) {
        return this;
    }

    @Override
    protected RowType mergeBy(SpecifyPropertyRowType specifyPropertyRowType) {
        return this;
    }

    @Override
    public DALNode constructVerificationNode(Data actual, Stream<Clause<DALRuntimeContext, DALNode>> rowClauses,
                                             Comparator<Object> comparator) {
        return actual.isList() ? new ListScopeNode(rowClauses.collect(toList()), true, comparator)
                : new ObjectScopeNode(Collections.emptyList());
    }
}

class SpecifyIndexRowType extends RowType {
    @Override
    public RowType merge(RowType another) {
        return another.mergeBy(this);
    }

    @Override
    protected RowType mergeBy(SpecifyIndexRowType specifyIndexRowType) {
        return specifyIndexRowType;
    }

    @Override
    protected RowType mergeBy(SpecifyPropertyRowType specifyPropertyRowType) {
        return specifyPropertyRowType;
    }

    @Override
    public DALNode constructVerificationNode(Data actual, Stream<Clause<DALRuntimeContext, DALNode>> rowClauses,
                                             Comparator<Object> comparator) {
        if (actual.isList())
            return new ListScopeNode(rowClauses.map(rowClause -> rowClause.expression(null))
                    .collect(toList()), true, ListScopeNode.Type.FIRST_N_ITEMS, comparator);
        return new ObjectScopeNode(rowClauses.map(rowClause -> rowClause.expression(null)).collect(toList()));
    }

    @Override
    public DALNode constructAccessingRowNode(DALNode input, Optional<DALNode> indexOrKey) {
        return indexOrKey.flatMap(SpecifyIndexRowType::indexToExpression).orElseThrow(IllegalStateException::new);
    }

    static Optional<DALNode> indexToExpression(DALNode node) {
        return when(node instanceof ConstNode).optional(() -> new DALExpression(InputNode.INSTANCE,
                new DALOperator.PropertyImplicit(), new SymbolNode(((ConstNode) node).getValue(), BRACKET)));
    }
}

class DefaultIndexRowType extends RowType {
    @Override
    public RowType merge(RowType another) {
        return another.mergeBy(this);
    }

    @Override
    protected RowType mergeBy(DefaultIndexRowType defaultIndexRowType) {
        return defaultIndexRowType;
    }

    @Override
    public DALNode constructVerificationNode(Data actual, Stream<Clause<DALRuntimeContext, DALNode>> rowClauses,
                                             Comparator<Object> comparator) {
        return new ListScopeNode(rowClauses.collect(toList()), true, comparator);
    }
}

class SpecifyPropertyRowType extends RowType {

    @Override
    public RowType merge(RowType another) {
        return another.mergeBy(this);
    }

    @Override
    public DALNode constructVerificationNode(Data actual, Stream<Clause<DALRuntimeContext, DALNode>> rowClauses,
                                             Comparator<Object> comparator) {
        return new ObjectScopeNode(rowClauses.map(rowNode -> rowNode.expression(null)).collect(toList()));
    }

    @Override
    protected RowType mergeBy(SpecifyPropertyRowType specifyPropertyRowType) {
        return specifyPropertyRowType;
    }

    @Override
    protected RowType mergeBy(SpecifyIndexRowType specifyIndexRowType) {
        return this;
    }

    @Override
    public DALNode constructAccessingRowNode(DALNode input, Optional<DALNode> indexOrKey) {
        return indexOrKey.map(node -> indexToExpression(node).orElse(node)).orElseThrow(IllegalStateException::new);
    }
}
