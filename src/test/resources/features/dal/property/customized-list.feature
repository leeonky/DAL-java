Feature: customized list

  Scenario: support customized list
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
    And register the following BeanDALCollectionFactory for java class "Bean":
    """
    public class BeanDALCollectionFactory implements DALCollectionFactory<Bean, String> {
      public DALCollection<String> create(Bean bean) {
        return new CollectionDALCollection<>(java.util.Arrays.asList("hello", "world"));
      }
    }
    """
    Then the following verification for the instance of java class "ABean" should pass:
    """
    bean= [hello world]
    """

  Rule: exception in list method

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
      And register the following BeanDALCollectionFactory for java class "Bean":
      """
      public class BeanDALCollectionFactory implements DALCollectionFactory<Bean, String> {
        public DALCollection<String> create(Bean bean) {
          throw new java.lang.RuntimeException("Error");
        }
      }
      """

    Scenario: show position when get item
      When use a instance of java class "ABean" to evaluate:
      """
        bean[0]
      """
      And got the following notation:
      """
        bean[0]
            ^
      """

    Scenario: show position when verify list
      When use a instance of java class "ABean" to evaluate:
      """
        bean= []
      """
      Then failed with the message:
      """
      Error
      """
      And got the following notation:
      """
        bean= []
        ^
      """

    Scenario: show position when verify list as table
      When use a instance of java class "ABean" to evaluate:
      """
        bean=  | any |
               | any |
      """
      Then failed with the message:
      """
      Error
      """
      And got the following notation:
      """
        bean=  | any |
        ^
               | any |
      """
      When use a instance of java class "ABean" to evaluate:
      """
        bean=  >>| any | any |
      """
      Then failed with the message:
      """
      Error
      """
      And got the following notation:
      """
        bean=  >>| any | any |
        ^
      """

    Scenario: show position as input
      When use a instance of java class "Bean" to evaluate:
      """
        =  | any |
           | any |
      """
      Then failed with the message:
      """
      Error
      """
      And got the following notation:
      """
        =  | any |
      ^
           | any |
      """

    Scenario: do not call list when not get item
      When use a instance of java class "ABean" to evaluate:
      """
        bean.class.simpleName= Bean
      """
