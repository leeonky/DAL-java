Feature: commons

  Scenario: support type local meta property
    Given the following java class:
    """
    public class Bean {
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty(Bean.class, "beanMeta", meta-> "hello");
    """
    Then the following verification for the instance of java class "Bean" should pass:
    """
    ::beanMeta= hello
    """
