Feature: data remark

  Background:
    Given the following java class:
    """
    public class Bean {
      public Bean(String s) {
        this.value = s;
      }
      public String value;
    }
    """
    Given the following java class:
    """
    public class BeanRef {
      public Bean bean;
      public BeanRef(Bean b){
        this.bean = b;
      }
      public BeanRef(){
        this.bean = new Bean("bean");
      }
    }
    """

  Rule: N/A for parentheses ()

    Scenario: N/A for parentheses
      When use a instance of java class "BeanRef" to evaluate:
      """
      (bean)(a bean)
      """
      Then failed with the message:
      """
      Should end with `)`
      """
      And got the following notation:
      """
      (bean)(a bean)
               ^
      """

  Rule: same precedence with property . []

    Scenario Outline: <position> property <opt>
      When register DAL:
      """
      dal.getRuntimeContextBuilder().registerDataRemark(Bean.class, rd-> rd.data().map(i->((Bean)i).value+rd.remark()));
      """
      Then the following verification for the instance of java class "BeanRef" should pass:
      """
      <code>
      """
      Then the result should:
      """
      = <value>
      """
      Examples:
        | code                          | value          | position | opt |
        | bean(a remark)                | 'beana remark' | after    |     |
        | .bean(a remark)               | 'beana remark' | after    | .   |
        | ['bean'](a remark)            | 'beana remark' | after    | []  |
        | bean(a remark).toUpperCase    | 'BEANA REMARK' | before   | .   |
        | bean(a remark)['toUpperCase'] | 'BEANA REMARK' | before   | []  |

  Rule: higher precedence than unary ! -

    Scenario: with not !
      When register DAL:
      """
      dal.getRuntimeContextBuilder().registerDataRemark(Bean.class, rd-> rd.data().map(i->Boolean.parseBoolean(rd.remark())));
      """
      When use a instance of java class "BeanRef" to evaluate:
      """
      !bean(true)
      """
      Then the result should:
      """
      = false
      """

    Scenario: with negative -
      When register DAL:
      """
      dal.getRuntimeContextBuilder().registerDataRemark(Bean.class, rd-> rd.data().map(i->Integer.parseInt(rd.remark())));
      """
      When use a instance of java class "BeanRef" to evaluate:
      """
      (-bean(100))
      """
      Then the result should:
      """
      = -100
      """

  Rule: higher precedence than arithmetic + - * /

    Scenario Outline: <position> <opt>
      When register DAL:
      """
      dal.getRuntimeContextBuilder().registerDataRemark(Bean.class, rd-> rd.data().map(i->Integer.parseInt(rd.remark())));
      """
      When use a instance of java class "BeanRef" to evaluate:
      """
      <operand1> <opt> <operand2>
      """
      Then the result should:
      """
      = <value>
      """
      Examples:
        | operand1 | opt | operand2 | value | position |
        | 10       | +   | bean(12) | 22    | after    |
        | 10       | -   | bean(8)  | 2     | after    |
        | 10       | *   | bean(3)  | 30    | after    |
        | 24       | /   | bean(4)  | 6     | after    |
        | bean(12) | +   | 2        | 14    | before   |
        | bean(8)  | -   | 2        | 6     | before   |
        | bean(3)  | *   | 2        | 6     | before   |
        | bean(6)  | /   | 3        | 2     | before   |

  Rule: higher precedence than comparison > >= < <= !=

    Scenario Outline: <position> <opt>
      When register DAL:
      """
      dal.getRuntimeContextBuilder().registerDataRemark(Bean.class, rd-> rd.data().map(i->Integer.parseInt(rd.remark())));
      """
      When use a instance of java class "BeanRef" to evaluate:
      """
      <operand1> <opt> <operand2>
      """
      Then the result should:
      """
      = <value>
      """
      Examples:
        | operand1 | opt | operand2 | value | position |
        | 10       | >   | bean(10) | false | after    |
        | 11       | >   | bean(10) | true  | after    |
        | 12       | >=  | bean(13) | false | after    |
        | 13       | >=  | bean(13) | true  | after    |
        | 14       | >=  | bean(13) | true  | after    |
        | 15       | <   | bean(16) | true  | after    |
        | 16       | <   | bean(16) | false | after    |
        | 17       | <=  | bean(18) | true  | after    |
        | 18       | <=  | bean(18) | true  | after    |
        | 19       | <=  | bean(18) | false | after    |
        | 20       | !=  | bean(20) | false | after    |
        | 21       | !=  | bean(20) | true  | after    |
        | bean(11) | >   | 10       | true  | before   |
        | bean(11) | >   | 11       | false | before   |
        | bean(13) | >=  | 12       | true  | before   |
        | bean(13) | >=  | 13       | true  | before   |
        | bean(13) | >=  | 14       | false | before   |
        | bean(15) | <   | 15       | false | before   |
        | bean(15) | <   | 16       | true  | before   |
        | bean(18) | <=  | 17       | false | before   |
        | bean(18) | <=  | 18       | true  | before   |
        | bean(18) | <=  | 19       | true  | before   |
#        | bean(20) | !=  | 20       | false | before   | considered as (bean(20)!)=
#        | bean(20) | !=  | 21       | true  | before   | considered as (bean(20)!)=

  Rule: higher precedence than comparison logical && || and or

    Scenario Outline: <position> <opt>
      When register DAL:
      """
      dal.getRuntimeContextBuilder().registerDataRemark(Bean.class, rd-> rd.data().map(i->Boolean.parseBoolean(rd.remark())));
      """
      When use a instance of java class "BeanRef" to evaluate:
      """
      <operand1> <opt> <operand2>
      """
      Then the result should:
      """
      = <value>
      """
      Examples:
        | operand1    | opt  | operand2          | value | position |
        | true        | &&   | bean(true)        | true  | after    |
        | true        | &&   | bean(false)       | false | after    |
        | true        | and  | bean(true)        | true  | after    |
        | true        | and  | bean(false)       | false | after    |
        | false       | and  | bean(not boolean) | false | after    |
        | false       | \|\| | bean(true)        | true  | after    |
        | false       | \|\| | bean(false)       | false | after    |
        | false       | or   | bean(true)        | true  | after    |
        | false       | or   | bean(false)       | false | after    |
        | true        | or   | bean(not boolean) | true  | after    |
        | bean(true)  | &&   | true              | true  | before   |
        | bean(false) | &&   | true              | false | before   |
        | bean(true)  | and  | true              | true  | before   |
        | bean(false) | and  | true              | false | before   |
        | bean(true)  | \|\| | true              | true  | before   |
        | bean(false) | \|\| | false             | false | before   |
        | bean(true)  | or   | true              | true  | before   |
        | bean(false) | or   | false             | false | before   |

  Rule: higher precedence than verification = :

    Background:
      When register DAL:
      """
      dal.getRuntimeContextBuilder().registerDataRemark(Bean.class, rd-> rd.data().map(i->Integer.parseInt(rd.remark())));
      """

    Scenario: before verification = :
      When use a instance of java class "BeanRef" to evaluate:
      """
      bean(99)= 100
      """
      Then failed with the message:
      """
      Expected to be equal to: java.lang.Integer
      <100>
       ^
      Actual: java.lang.Integer
      <99>
       ^
      """
      When use a instance of java class "BeanRef" to evaluate:
      """
      bean(99): 100
      """
      Then failed with the message:
      """
      Expected to match: java.lang.Integer
      <100>
       ^
      Actual: java.lang.Integer
      <99>
       ^
      """

    Scenario: N/A after verification = :
      When use a instance of java class "BeanRef" to evaluate:
      """
      100= bean(99)
      """
      Then failed with the message:
      """
      Expected to be equal to: java.lang.String
                                         ^
      <bean(99)>
      Actual: java.lang.Integer
                        ^
      <100>
      """
      When use a instance of java class "BeanRef" to evaluate:
      """
      100: bean(99)
      """
      Then failed with the message:
      """
      Cannot compare between java.lang.Integer
      <100>
      and java.lang.String
      <bean(99)>
      """
