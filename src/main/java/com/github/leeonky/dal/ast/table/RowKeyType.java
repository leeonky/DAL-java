package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.Clause;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.SymbolNode.Type.BRACKET;
import static java.util.stream.Collectors.toList;

abstract class RowKeyType {
    static final RowKeyType NO_ROW_KEY = new NoRowKeyType(), INDEX_ROW_KEY = new IndexRowKeyType(),
            PROPERTY_ROW_KEY = new PropertyRowKeyType(), EMPTY_TABLE_ROW_KEY = new EmptyTableRowKeyType();

    public abstract RowKeyType merge(RowKeyType rowKeyType);

    protected abstract RowKeyType mergeBy(IndexRowKeyType indexRowKeyType);

    protected abstract RowKeyType mergeBy(NoRowKeyType noRowKeyType);

    public DALNode transformToVerificationNode(Data actual, DALOperator operator, List<RowNode> rows, Comparator<Object> comparator) {
        return makeNode(actual, rows.stream().map(rowNode -> rowNode.verificationClause(operator, this)), comparator);
    }

    protected abstract DALNode makeNode(Data actual, Stream<Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode>> rowClauses,
                                        Comparator<Object> comparator);

    public DALNode inputWithRowKey(DALNode input, Optional<DALNode> keyNode) {
        return input;
    }

    protected abstract RowKeyType mergeBy(PropertyRowKeyType propertyRowKeyType);
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
    protected DALNode makeNode(Data actual, Stream<Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode>> rowClauses,
                               Comparator<Object> comparator) {
//            TODO to list or object
        return new ListScopeNode(rowClauses.collect(toList()), true, comparator);
    }

    //    TODO need test
    @Override
    protected RowKeyType mergeBy(PropertyRowKeyType propertyRowKeyType) {
        return null;
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
    protected RowKeyType mergeBy(NoRowKeyType noRowKeyType) {
        throw new IllegalArgumentException();
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

    //    TODO need test
    @Override
    protected RowKeyType mergeBy(PropertyRowKeyType propertyRowKeyType) {
        return null;
    }
}

class NoRowKeyType extends RowKeyType {
    @Override
    public RowKeyType merge(RowKeyType rowKeyType) {
        return rowKeyType.mergeBy(this);
    }

    @Override
    protected RowKeyType mergeBy(IndexRowKeyType indexRowKeyType) {
        throw new IllegalArgumentException();
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

    //    TODO need test
    @Override
    protected RowKeyType mergeBy(PropertyRowKeyType propertyRowKeyType) {
        return null;
    }
}

class PropertyRowKeyType extends RowKeyType {

    @Override
    public RowKeyType merge(RowKeyType rowKeyType) {
        return rowKeyType.mergeBy(this);
    }

    //    TODO test
    @Override
    protected RowKeyType mergeBy(IndexRowKeyType indexRowKeyType) {
        return null;
    }

    //    TODO test
    @Override
    protected RowKeyType mergeBy(NoRowKeyType noRowKeyType) {
        return null;
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
