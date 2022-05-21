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
    public DALNode constructVerificationNode(Data actual, Stream<Clause<DALRuntimeContext, DALNode>> rowClauses,
                                             Comparator<Object> comparator) {
//            TODO to list or object
        return new ListScopeNode(rowClauses.map(rowClause -> rowClause.expression(null))
                .collect(toList()), true, ListScopeNode.Type.FIRST_N_ITEMS, comparator);
    }

    @Override
    public DALNode constructAccessingRowNode(DALNode input, Optional<DALNode> indexOrKey) {
        return indexOrKey.map(node -> ((ConstNode) node).getValue()).map(i -> new DALExpression(
                        InputNode.INSTANCE, new DALOperator.PropertyImplicit(), new SymbolNode(i, BRACKET)))
                .orElseThrow(IllegalStateException::new);
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
    public DALNode constructAccessingRowNode(DALNode input, Optional<DALNode> indexOrKey) {
        return indexOrKey.orElseThrow(IllegalStateException::new);
    }
}
