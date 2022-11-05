package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.util.TextUtil;
import com.github.leeonky.interpreter.StringWithPosition;

import static java.lang.String.format;

//TODO refactor
public class CheckingContext {
    //    TODO rename
    private final Data expected;
    private final Data actual;
    private final Data transformedExpected;
    private final Data transformedActual;
    private final int position;

    public Data getActual() {
        return actual;
    }

    public Data getExpected() {
        return expected;
    }

    public CheckingContext(Data expected, Data actual, Data transformedExpected, Data transformedActual, int positionBegin) {
        this.expected = expected;
        this.actual = actual;
        this.transformedExpected = transformedExpected;
        this.transformedActual = transformedActual;
        position = positionBegin;
    }

    public boolean objectNotEquals() {
        return !Calculator.equals(transformedActual, transformedExpected);
    }

    public Object getExpectInstance() {
        return expected.getInstance();
    }

    public boolean expectNull() {
        return expected.isNull();
    }

    public String notationEqualTo() {
        return verificationMessage("Expected to be equal to: ");
    }

    public String notationMatch() {
        return verificationMessage("Expected to match: ", transformedActual.getInstance() == actual.getInstance() ? ""
                : " converted from: " + actual.dumpDetail());
    }

    public String verificationMessage(String prefix) {
        return verificationMessage(prefix, "");
    }

    public String verificationMessage(String prefix, String actualPostfix) {
        String actual = transformedActual.dumpDetail() + actualPostfix;
        String expected = transformedExpected.dumpDetail();
        int position = TextUtil.differentPosition(expected, actual);
        String firstPart = new StringWithPosition(expected).position(position).result(prefix);
        return new StringWithPosition(actual).position(position).result(firstPart + "\nActual: ");
    }

    public String cannotCompare() {
        return format("Cannot compare between %s\nand %s", actual.dumpDetail(), expected.dumpDetail());
    }

    Checker defaultMatchesChecker() {
        if (expectNull())
            return Checker.MATCH_NULL_CHECKER;
        return Checker.MATCH_CHECKER;
    }

    public int getPosition() {
        return position;
    }

    public Data getTransformedExpected() {
        return transformedExpected;
    }

    public Data getTransformedActual() {
        return transformedActual;
    }
}
