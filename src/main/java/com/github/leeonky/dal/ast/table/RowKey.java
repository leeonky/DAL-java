package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.Clause;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.table.RowKey.RowKeyType.INDEX_ROW_KEY;
import static com.github.leeonky.dal.ast.table.RowKey.RowKeyType.NO_ROW_KEY;
import static java.util.stream.Collectors.toList;

public class RowKey {
    private final Optional<Integer> index;

    public RowKey(Optional<Integer> index) {
        this.index = index;
    }

    public DALNode inputWithRowKey(DALNode input) {
        return index.map(i -> (DALNode) new DALExpression(InputNode.INSTANCE,
                new DALOperator.PropertyImplicit(), new SymbolNode(i, SymbolNode.Type.BRACKET))).orElse(input);
    }

    public String inspect() {
        return index.map(Object::toString).orElse("");
    }

    public RowKeyType toType() {
        return index.isPresent() ? INDEX_ROW_KEY : NO_ROW_KEY;
    }

    static abstract class RowKeyType {
        static final RowKeyType NO_ROW_KEY = new NoRowKeyType(),
                INDEX_ROW_KEY = new IndexRowKeyType(),
                EMPTY_TABLE_ROW_KEY = new EmptyTableRowKeyType();

        public abstract RowKeyType merge(RowKeyType rowKeyType);

        protected abstract RowKeyType mergeBy(IndexRowKeyType indexRowKeyType);

        protected abstract RowKeyType mergeBy(NoRowKeyType noRowKeyType);

        public DALNode transformToVerificationNode(DALOperator operator, List<RowNode> rows, Comparator<Object> comparator) {
            return makeNode(rows.stream().map(rowNode -> rowNode.verificationClause(operator)), comparator);
        }

        protected abstract DALNode makeNode(Stream<Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode>> rowClauses,
                                            Comparator<Object> comparator);
    }

    private static class EmptyTableRowKeyType extends RowKeyType {

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
        protected DALNode makeNode(Stream<Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode>> rowClauses,
                                   Comparator<Object> comparator) {
            return new ListScopeNode(rowClauses.collect(toList()), true, comparator);
        }
    }

    private static class IndexRowKeyType extends RowKeyType {
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
        protected DALNode makeNode(Stream<Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode>> rowClauses,
                                   Comparator<Object> comparator) {
            return new ListScopeNode(rowClauses.map(rowNode -> rowNode.expression(null))
                    .collect(toList()), true, ListScopeNode.Type.FIRST_N_ITEMS, comparator);
        }
    }

    private static class NoRowKeyType extends RowKeyType {
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
        protected DALNode makeNode(Stream<Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode>> rowClauses,
                                   Comparator<Object> comparator) {
            return new ListScopeNode(rowClauses.collect(toList()), true, comparator);
        }
    }
}