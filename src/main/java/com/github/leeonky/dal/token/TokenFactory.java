package com.github.leeonky.dal.token;

import static com.github.leeonky.dal.parser.NewTokenFactory.*;
import static com.github.leeonky.dal.parser.SourceCodeGetter.ALL_CHARACTERS;
import static com.github.leeonky.dal.parser.SourceCodeGetter.leftTrim;
import static com.github.leeonky.dal.parser.SourceCodeMatcher.*;
import static com.github.leeonky.dal.parser.TokenStartEnd.*;

public interface TokenFactory {
    static TokenFactory createNumberTokenFactory() {
        return startWith(included(DIGITAL))
                .take(ALL_CHARACTERS)
                .endWith(END_OF_CODE.or(before(DELIMITER)))
                .createAs(CONST_NUMBER_TOKEN);
    }

    static TokenFactory createBeanPropertyTokenFactory() {
        return startWith(excluded(CHARACTER('.')))
                .take(leftTrim(ALL_CHARACTERS))
                .endWith(END_OF_CODE.or(before(DELIMITER)))
                .createAs(PROPERTY_TOKEN);
    }

    static TokenFactory createOperatorTokenFactory() {
        return startWith(included(OPERATOR.except(CHARACTER('/').when(AFTER_TOKEN_MATCHES))))
                .take(ALL_CHARACTERS)
                .endWith(END_OF_CODE.or(before(not(OPERATOR))).or(before(CHARACTER('/').when(AFTER_OPERATOR_MATCHES))))
                .createAs(OPERATOR_TOKEN);
    }

    static TokenFactory createSingleQuotedStringTokenFactory() {
        return startWith(excluded(CHARACTER('\'')))
                .take(ALL_CHARACTERS
                        .escape("\\'", '\'')
                        .escape("\\\\", '\\'))
                .endWith(excluded(CHARACTER('\'')).orThrow("string should end with `'`"))
                .createAs(CONST_STRING_TOKEN);
    }

    static TokenFactory createDoubleQuotedStringTokenFactory() {
        return startWith(excluded(CHARACTER('"')))
                .take(ALL_CHARACTERS
                        .escape("\\\"", '"')
                        .escape("\\t", '\t')
                        .escape("\\n", '\n')
                        .escape("\\\\", '\\'))
                .endWith(excluded(CHARACTER('"')).orThrow("string should end with `\"`"))
                .createAs(CONST_STRING_TOKEN);
    }

    static TokenFactory createRegexTokenFactory() {
        return startWith(excluded(CHARACTER('/').when(AFTER_TOKEN_MATCHES)))
                .take(ALL_CHARACTERS
                        .escape("\\\\", '\\')
                        .escape("\\/", '/'))
                .endWith(excluded(CHARACTER('/')).orThrow("string should end with `/`"))
                .createAs(REGEX_TOKEN);
    }

    Token fetchToken(SourceCode sourceCode, Token previous);
}
