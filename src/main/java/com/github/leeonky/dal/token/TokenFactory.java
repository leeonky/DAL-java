package com.github.leeonky.dal.token;

import com.github.leeonky.dal.parser.ParsingContext;

import static com.github.leeonky.dal.parser.NewTokenFactory.*;
import static com.github.leeonky.dal.parser.ParsingContext.*;
import static com.github.leeonky.dal.parser.SourceCodeMatcher.not;
import static com.github.leeonky.dal.parser.TokenContentInString.ALL_CHARACTERS;
import static com.github.leeonky.dal.parser.TokenContentInString.leftTrim;
import static com.github.leeonky.dal.parser.TokenContentInToken.byFactory;
import static com.github.leeonky.dal.parser.TokenStartEnd.before;

public interface TokenFactory {
    static TokenFactory createNumberTokenFactory() {
        return startWith(included(DIGITAL))
                .endWith(END_OF_CODE.or(before(DELIMITER)))
                .createAs(CONST_NUMBER_TOKEN);
    }

    static TokenFactory createBeanPropertyTokenFactory() {
        return startWith(excluded(CHARACTER('.')))
                .take(leftTrim(ALL_CHARACTERS))
                .endWith(END_OF_CODE.or(before(DELIMITER)).or(before(CHARACTER('.'))))
                .createAs(PROPERTY_TOKEN);
    }

    static TokenFactory createOperatorTokenFactory() {
        return startWith(included(OPERATOR.except(CHARACTER('/').when(AFTER_TOKEN_MATCHES))))
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

    static TokenFactory createBeginBracketTokenFactory() {
        return equalToCharacter('(').createAs(BEGIN_BRACKET_TOKEN);
    }

    static TokenFactory createEndBracketTokenFactory() {
        return equalToCharacter(')').createAs(END_BRACKET_TOKEN);
    }

    static TokenFactory createBracketPropertyTokenFactory() {
        return startWith(excluded(CHARACTER('[').except(AFTER_TOKEN_MATCHES)))
                .take(byFactory(createNumberTokenFactory())
                        .or(createSingleQuotedStringTokenFactory())
                        .or(createDoubleQuotedStringTokenFactory()))
                .endWith(excluded(CHARACTER(']')).orThrow("should end with `]`"))
                .createAs(BRACKET_PROPERTY_TOKEN);
    }

    Token fetchToken(ParsingContext context);
}
