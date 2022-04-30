package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.Clause;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.SymbolNode.Type.BRACKET;
import static java.util.stream.Collectors.toList;

abstract class RowKeyType {

    public abstract RowKeyType merge(RowKeyType rowKeyType);

    protected RowKeyType mergeBy(IndexRowKeyType indexRowKeyType) {
        throw new IllegalArgumentException();
    }

    protected RowKeyType mergeBy(NoRowKeyType noRowKeyType) {
        throw new IllegalArgumentException();
    }

    protected RowKeyType mergeBy(PropertyRowKeyType propertyRowKeyType) {
        throw new IllegalArgumentException();
    }

    public DALNode transformToVerificationNode(Data actual, DALOperator operator, List<RowNode> rows, Comparator<Object> comparator) {
        return makeNode(actual, rows.stream().map(rowNode -> rowNode.verificationClause(operator, this)), comparator);
    }

    protected abstract DALNode makeNode(Data actual, Stream<Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode>> rowClauses,
                                        Comparator<Object> comparator);

    public DALNode inputWithRowKey(DALNode input, Optional<DALNode> keyNode) {
        return input;
    }
}

class EmptyTableRowKeyType extends RowKeyType {

    @Override
    public RowKeyType merge(RowKeyType rowKeyType) {
        return rowKeyType;
    }

    @Override
    protected RowKeyType mergeBy(IndexRowKeyType indexRowKeyType) {
        return this;
    }

    @Override
    protected RowKeyType mergeBy(NoRowKeyType noRowKeyType) {
        return this;
    }

    @Override
    protected RowKeyType mergeBy(PropertyRowKeyType propertyRowKeyType) {
        return this;
    }

    @Override
    protected DALNode makeNode(Data actual, Stream<Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode>> rowClauses,
                               Comparator<Object> comparator) {
        return actual.isList() ? new ListScopeNode(rowClauses.collect(toList()), true, comparator)
                : new ObjectScopeNode(Collections.emptyList());
    }
}

class IndexRowKeyType extends RowKeyType {
    @Override
    public RowKeyType merge(RowKeyType rowKeyType) {
        return rowKeyType.mergeBy(this);
    }

    @Override
    protected RowKeyType mergeBy(IndexRowKeyType indexRowKeyType) {
        return indexRowKeyType;
    }

    @Override
    protected DALNode makeNode(Data actual, Stream<Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode>> rowClauses,
                               Comparator<Object> comparator) {
//            TODO to list or object
        return new ListScopeNode(rowClauses.map(rowNode -> rowNode.expression(null))
                .collect(toList()), true, ListScopeNode.Type.FIRST_N_ITEMS, comparator);
    }

    @Override
    public DALNode inputWithRowKey(DALNode input, Optional<DALNode> keyNode) {
        return keyNode.map(node -> ((ConstNode) node).getValue()).map(i -> new DALExpression(
                        InputNode.INSTANCE, new DALOperator.PropertyImplicit(), new SymbolNode(i, BRACKET)))
                .orElseThrow(IllegalStateException::new);
    }
}

class NoRowKeyType extends RowKeyType {
    @Override
    public RowKeyType merge(RowKeyType rowKeyType) {
        return rowKeyType.mergeBy(this);
    }

    @Override
    protected RowKeyType mergeBy(NoRowKeyType noRowKeyType) {
        return noRowKeyType;
    }

    @Override
    protected DALNode makeNode(Data actual, Stream<Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode>> rowClauses,
                               Comparator<Object> comparator) {
        return new ListScopeNode(rowClauses.collect(toList()), true, comparator);
    }
}

class PropertyRowKeyType extends RowKeyType {

    @Override
    public RowKeyType merge(RowKeyType rowKeyType) {
        return rowKeyType.mergeBy(this);
    }

    @Override
    protected DALNode makeNode(Data actual, Stream<Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode>> rowClauses,
                               Comparator<Object> comparator) {
        return new ObjectScopeNode(rowClauses.map(rowNode -> rowNode.expression(null)).collect(toList()));
    }

    @Override
    protected RowKeyType mergeBy(PropertyRowKeyType propertyRowKeyType) {
        return propertyRowKeyType;
    }

    @Override
    public DALNode inputWithRowKey(DALNode input, Optional<DALNode> keyNode) {
        return keyNode.orElseThrow(IllegalStateException::new);
    }
}
