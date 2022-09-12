package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.ast.node.DALExpression;
import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.ast.node.SymbolNode;
import com.github.leeonky.dal.compiler.Notations;
import com.github.leeonky.dal.runtime.AssertionFailure;
import com.github.leeonky.dal.runtime.DalException;

import static com.github.leeonky.dal.ast.opt.Factory.executable;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class Base {
    protected DAL dal = new DAL();

    public static DALNode createPropertyNode(DALNode instanceNode, Object name) {
        return new DALExpression(instanceNode, executable(Notations.EMPTY), new SymbolNode(name, SymbolNode.Type.BRACKET));
    }

    protected void assertPass(Object input, String expression) {
        dal.evaluate(input, expression);
    }

    protected void assertFailed(Object input, String expression) {
        AssertionFailure assertionFailure = null;
        try {
            dal.evaluate(input, expression);
        } catch (AssertionFailure failure) {
            assertionFailure = failure;
        }
        assertThat(assertionFailure).isNotNull();
    }

    protected void assertRuntimeException(Object input, String sourceCode, int position, String message) {
        RuntimeException runtimeException = assertThrows(RuntimeException.class, () -> dal.evaluate(input, sourceCode));

        assertThat(runtimeException)
                .hasFieldOrPropertyWithValue("position", position)
                .hasMessage(message);
    }

    protected void assertErrorContains(Object input, String expression, String errorMessage) {
        assertThat(assertThrows(DalException.class, () -> {
            dal.evaluate(input, expression);
        })).hasMessage(errorMessage);
    }
}
