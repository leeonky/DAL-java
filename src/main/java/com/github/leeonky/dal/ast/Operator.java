package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.Comparer;

public enum Operator {
    EQUAL {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Comparer.equals(v1, v2);
        }
    }, GREATER {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Comparer.compare(v1, v2) > 0;
        }
    }, LESS {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Comparer.compare(v1, v2) < 0;
        }
    }, GREATER_OR_EQUAL {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Comparer.compare(v1, v2) >= 0;
        }
    }, LESS_OR_EQUAL {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Comparer.compare(v1, v2) <= 0;
        }
    }, NOT_EQUAL {
        @Override
        public Object calculate(Object v1, Object v2) {
            return !Comparer.equals(v1, v2);
        }
    };

    public abstract Object calculate(Object v1, Object v2);
}
