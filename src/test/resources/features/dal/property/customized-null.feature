Feature: customized null

  Scenario: support customized null
    Given the following java class:
    """
    public class Bean {
    }
    """
    Given the following java class:
    """
    public class ABean {
      public Bean bean = new Bean();
    }
    """
    And register the following PropertyAccessor for java class "Bean":
    """
    public class BeanPropertyAccessor extends JavaClassPropertyAccessor<Bean> {
      private static BeanClass<Bean> type = BeanClass.create(Bean.class);
      public BeanPropertyAccessor() {
        super(type);
      }
      public boolean isNull(Bean instance) {
          return true;
      }
    }
    """
    Then the following verification for the instance of java class "ABean" should pass:
    """
    bean= null
    """

  Rule: exception in isNull method
    Background:
      Given the following java class:
      """
      public class Bean {
      }
      """
      Given the following java class:
      """
      public class ABean {
        public Bean bean = new Bean();
      }
      """
      And register the following PropertyAccessor for java class "Bean":
      """
      public class BeanPropertyAccessor extends JavaClassPropertyAccessor<Bean> {
        private static BeanClass<Bean> type = BeanClass.create(Bean.class);
        public BeanPropertyAccessor() {
          super(type);
        }
        public boolean isNull(Bean instance) {
            throw new java.lang.RuntimeException("Error");
        }
      }
      """

    Scenario: show position when get property value
      When use a instance of java class "ABean" to evaluate:
      """
        bean.class
      """
      Then failed with the message:
      """
      Error
      """
      And got the following notation:
      """
        bean.class
        ^
      """

    Scenario: show position when verify object
      When use a instance of java class "ABean" to evaluate:
      """
        bean= {
          class: {...}
        }
      """
      Then failed with the message:
      """
      Error
      """
      And got the following notation:
      """
        bean= {
        ^
          class: {...}
        }
      """
      When use a instance of java class "ABean" to evaluate:
      """
        bean: {
          class: {...}
        }
      """
      Then failed with the message:
      """
      Error
      """
      And got the following notation:
      """
        bean: {
        ^
          class: {...}
        }
      """

    Scenario: show position as input
      When use a instance of java class "Bean" to evaluate:
      """
        = { }
      """
      Then failed with the message:
      """
      Error
      """
      And got the following notation:
      """
        = { }
      ^
      """

    Scenario: show in assert null
      When use a instance of java class "ABean" to evaluate:
      """
        {}.bean= null
      """
      Then failed with the message:
      """
      Error
      """
      And got the following notation:
      """
        {}.bean= null
           ^
      """
      When use a instance of java class "ABean" to evaluate:
      """
        null= .bean
      """
      Then failed with the message:
      """
      Error
      """
      And got the following notation:
      """
        null= .bean
               ^
      """

    Scenario: do not call isNull when not get property
      When the following verification for the instance of java class "ABean" should pass:
      """
        bean::object.class.simpleName= Bean
      """
