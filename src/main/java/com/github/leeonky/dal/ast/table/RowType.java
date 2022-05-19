package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.SymbolNode.Type.BRACKET;
import static java.util.stream.Collectors.toList;

abstract class RowType {

    public abstract RowType merge(RowType another);

    protected RowType mergeBy(IndexRowType indexRowType) {
        throw new IllegalArgumentException();
    }

    protected RowType mergeBy(NoRowType noRowType) {
        throw new IllegalArgumentException();
    }

    protected RowType mergeBy(PropertyRowType propertyRowType) {
        throw new IllegalArgumentException();
    }

    public DALNode convertToVerificationNode(Data actual, DALOperator operator, List<TableRowNode> rows,
                                             Comparator<Object> comparator) {
        return constructNode(actual, rows.stream().map(rowNode -> rowNode.constructVerificationClause(operator, this)), comparator);
    }

    protected abstract DALNode constructNode(Data actual, Stream<Clause<DALRuntimeContext, DALNode>> rowClauses,
                                             Comparator<Object> comparator);

    public DALNode inputWithRowKey(DALNode input, Optional<DALNode> indexOrKey) {
        return input;
    }
}

class EmptyTableRowType extends RowType {

    @Override
    public RowType merge(RowType another) {
        return another;
    }

    @Override
    protected RowType mergeBy(IndexRowType indexRowType) {
        return this;
    }

    @Override
    protected RowType mergeBy(NoRowType noRowType) {
        return this;
    }

    @Override
    protected RowType mergeBy(PropertyRowType propertyRowType) {
        return this;
    }

    @Override
    protected DALNode constructNode(Data actual, Stream<Clause<DALRuntimeContext, DALNode>> rowClauses,
                                    Comparator<Object> comparator) {
        return actual.isList() ? new ListScopeNode(rowClauses.collect(toList()), true, comparator)
                : new ObjectScopeNode(Collections.emptyList());
    }
}

class IndexRowType extends RowType {
    @Override
    public RowType merge(RowType another) {
        return another.mergeBy(this);
    }

    @Override
    protected RowType mergeBy(IndexRowType indexRowType) {
        return indexRowType;
    }

    @Override
    protected DALNode constructNode(Data actual, Stream<Clause<DALRuntimeContext, DALNode>> rowClauses,
                                    Comparator<Object> comparator) {
//            TODO to list or object
        return new ListScopeNode(rowClauses.map(rowNode -> rowNode.expression(null))
                .collect(toList()), true, ListScopeNode.Type.FIRST_N_ITEMS, comparator);
    }

    @Override
    public DALNode inputWithRowKey(DALNode input, Optional<DALNode> indexOrKey) {
        return indexOrKey.map(node -> ((ConstNode) node).getValue()).map(i -> new DALExpression(
                        InputNode.INSTANCE, new DALOperator.PropertyImplicit(), new SymbolNode(i, BRACKET)))
                .orElseThrow(IllegalStateException::new);
    }
}

class NoRowType extends RowType {
    @Override
    public RowType merge(RowType another) {
        return another.mergeBy(this);
    }

    @Override
    protected RowType mergeBy(NoRowType noRowType) {
        return noRowType;
    }

    @Override
    protected DALNode constructNode(Data actual, Stream<Clause<DALRuntimeContext, DALNode>> rowClauses,
                                    Comparator<Object> comparator) {
        return new ListScopeNode(rowClauses.collect(toList()), true, comparator);
    }
}

class PropertyRowType extends RowType {

    @Override
    public RowType merge(RowType another) {
        return another.mergeBy(this);
    }

    @Override
    protected DALNode constructNode(Data actual, Stream<Clause<DALRuntimeContext, DALNode>> rowClauses,
                                    Comparator<Object> comparator) {
        return new ObjectScopeNode(rowClauses.map(rowNode -> rowNode.expression(null)).collect(toList()));
    }

    @Override
    protected RowType mergeBy(PropertyRowType propertyRowType) {
        return propertyRowType;
    }

    @Override
    public DALNode inputWithRowKey(DALNode input, Optional<DALNode> indexOrKey) {
        return indexOrKey.orElseThrow(IllegalStateException::new);
    }
}
