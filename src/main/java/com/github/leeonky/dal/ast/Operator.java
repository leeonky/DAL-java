package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.Calculator;

public enum Operator {
    EQUAL {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.equals(v1, v2);
        }
    }, GREATER {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) > 0;
        }
    }, LESS {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) < 0;
        }
    }, GREATER_OR_EQUAL {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) >= 0;
        }
    }, LESS_OR_EQUAL {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) <= 0;
        }
    }, NOT_EQUAL {
        @Override
        public Object calculate(Object v1, Object v2) {
            return !Calculator.equals(v1, v2);
        }
    }, PLUS {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.plus(v1, v2);
        }
    };

    public abstract Object calculate(Object v1, Object v2);
}
