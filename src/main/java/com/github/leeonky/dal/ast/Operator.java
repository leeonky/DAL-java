package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.Calculator;

public abstract class Operator {

    private int position;

    public abstract Object calculate(Object v1, Object v2);

    @Override
    public boolean equals(Object obj) {
        return getClass().isInstance(obj);
    }

    public int getPosition() {
        return position;
    }

    public void setPosition(int position) {
        this.position = position;
    }

    public static class Equal extends Operator {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.equals(v1, v2);
        }
    }

    public static class Less extends Operator {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) < 0;
        }
    }

    public static class GreaterOrEqual extends Operator {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) >= 0;
        }
    }

    public static class LessOrEqual extends Operator {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) <= 0;
        }
    }

    public static class NotEqual extends Operator {
        @Override
        public Object calculate(Object v1, Object v2) {
            return !Calculator.equals(v1, v2);
        }
    }

    public static class Plus extends Operator {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.plus(v1, v2);
        }
    }

    public static class Greater extends Operator {
        @Override
        public Object calculate(Object v1, Object v2) {
            return Calculator.compare(v1, v2) > 0;
        }
    }
}
