package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.*;

import java.util.Optional;

import static com.github.leeonky.dal.ast.table.RowKey.RowKeyType.INDEX_ROW_KEY;
import static com.github.leeonky.dal.ast.table.RowKey.RowKeyType.NO_ROW_KEY;

public class RowKey {
    final Optional<Integer> index;

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
//            TODO need raise error
            return null;
        }
    }

    private static class NoRowKeyType extends RowKeyType {
        @Override
        public RowKeyType merge(RowKeyType rowKeyType) {
            return rowKeyType.mergeBy(this);
        }

        @Override
        protected RowKeyType mergeBy(IndexRowKeyType indexRowKeyType) {
//            TODO need raise error
            return null;
        }

        @Override
        protected RowKeyType mergeBy(NoRowKeyType noRowKeyType) {
            return noRowKeyType;
        }
    }
}