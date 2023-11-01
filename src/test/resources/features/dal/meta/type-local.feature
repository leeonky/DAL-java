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

  Scenario: call meta property as super type
    Given the following java class:
    """
    public class Bean {
    }
    """
    Given the following java class:
    """
    public class SubBean extends Bean {
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty(Bean.class, "beanMeta", meta-> "hello");
    """
    Then the following verification for the instance of java class "SubBean" should pass:
    """
    ::beanMeta= hello
    """

  Scenario: override meta property in sub
    Given the following java class:
    """
    public class Bean {
    }
    """
    Given the following java class:
    """
    public class SubBean extends Bean {
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty(Bean.class, "beanMeta", meta-> "Super");
    dal.getRuntimeContextBuilder().registerMetaProperty(SubBean.class, "beanMeta", meta-> "Sub");
    """
    Then the following verification for the instance of java class "SubBean" should pass:
    """
    ::beanMeta= Sub
    """

  Scenario: override global meta
    Given the following java class:
    """
    public class Bean {
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty("beanMeta", meta-> "Global");
    dal.getRuntimeContextBuilder().registerMetaProperty(Bean.class, "beanMeta", meta-> "Local");
    """
    Then the following verification for the instance of java class "Bean" should pass:
    """
    ::beanMeta= Local
    """

  Scenario: call super
    Given the following java class:
    """
    public class Bean {
    }
    """
    Given the following java class:
    """
    public class SubBean extends Bean {
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty(Bean.class, "beanMeta", meta-> "Super");
    dal.getRuntimeContextBuilder().registerMetaProperty(SubBean.class, "beanMeta", meta-> meta.callSuper());
    """
    Then the following verification for the instance of java class "SubBean" should pass:
    """
    ::beanMeta= Super
    """

  Scenario: call super chain
    Given the following java class:
    """
    public class Bean {
    }
    """
    Given the following java class:
    """
    public class SubBean extends Bean {
    }
    """
    Given the following java class:
    """
    public class SubSubBean extends SubBean {
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty(Bean.class, "beanMeta", meta-> "Super");
    dal.getRuntimeContextBuilder().registerMetaProperty(SubBean.class, "beanMeta", meta-> meta.callSuper());
    dal.getRuntimeContextBuilder().registerMetaProperty(SubSubBean.class, "beanMeta", meta-> meta.callSuper());
    """
    Then the following verification for the instance of java class "SubSubBean" should pass:
    """
    ::beanMeta= Super
    """

  Scenario: no super
    Given the following java class:
    """
    public class Bean {
    }
    """
    Given the following java class:
    """
    public class SubBean extends Bean {
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty(Bean.class, "beanMeta", meta-> meta.callSuper());
    dal.getRuntimeContextBuilder().registerMetaProperty(SubBean.class, "beanMeta", meta-> meta.callSuper());
    """
    When use a instance of java class "SubBean" to evaluate:
    """
    ::beanMeta= ''
    """
    Then failed with the message:
    """
    Local meta property `beanMeta` has no super in type #package#Bean
    """
    And got the following notation:
    """
    ::beanMeta= ''
      ^
    """

  Scenario: call super with same type new value
    Given the following java class:
    """
    public class Bean {
    }
    """
    Given the following java class:
    """
    public class SubBean extends Bean {
      public int value = 1;
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty(Bean.class, "beanMeta", meta-> meta.data().instance());
    dal.getRuntimeContextBuilder().registerMetaProperty(SubBean.class, "beanMeta", meta-> {
    return meta.callSuper(()->new SubBean(){{value=2;}});
    });
    """
    Then the following verification for the instance of java class "SubBean" should pass:
    """
    ::beanMeta.value= 2
    """

  Scenario: raise error when use different instance type in call super
    Given the following java class:
    """
    public class Bean {
    }
    """
    Given the following java class:
    """
    public class SubBean extends Bean {
      public int value = 1;
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty(SubBean.class, "beanMeta", meta-> meta.callSuper(()->"a string"));
    """
    When use a instance of java class "SubBean" to evaluate:
    """
    ::beanMeta= ''
    """
    Then failed with the message:
    """
    Do not allow change data type in callSuper, expect #package#SubBean but java.lang.String
    """
    And got the following notation:
    """
    ::beanMeta= ''
      ^
    """

  Scenario: raise error when not catch error
    Given the following java class:
    """
    public class Data {
      public void test() {
        throw new java.lang.RuntimeException("error");
      }
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty("meta", meta-> {
      return meta.data().instance();
    });
    """
    When use a instance of java class "Data" to evaluate:
    """
    test::meta: hello
    """
    Then failed with the message:
    """
    Get property `test` failed, property can be:
      1. public field
      2. public getter
      3. public no args method
      4. Map key value
      5. customized type getter
      6. static method extension
    java.lang.RuntimeException: error
    """
    And got the following notation:
    """
    test::meta: hello
    ^
    """

  Scenario: support call global
    Given the following java class:
    """
    public class Data {
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty(Data.class, "meta", meta-> {
      return meta.callGlobal();
    }
    );
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty("meta", meta-> {
      return "hello";
    });
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    ::meta: hello
    """

  Scenario: support call global with any type data
    Given the following java class:
    """
    public class Data {
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty(Data.class, "meta", meta-> {
      return meta.callGlobal(()-> "hello");
    }
    );
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty("meta", meta-> {
      return meta.data().instance();
    });
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    ::meta: hello
    """

  Scenario: support call other meta
    Given the following java class:
    """
    public class Data {
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty(Data.class, "meta", meta-> {
      return meta.callMeta("another");
    }
    );
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty("another", meta-> {
      return meta.data().instance();
    });
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    ::meta.class.simpleName= Data
    """

  Scenario: support call other meta with any data
    Given the following java class:
    """
    public class Data {
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty(Data.class, "meta", meta-> {
      return meta.callMeta("another", ()-> "hello");
    }
    );
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty("another", meta-> {
      return meta.data().instance();
    });
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    ::meta= hello
    """

  Scenario: support type local meta property in list mapping
    Given the following java class:
    """
    public class Bean {
      public String value;
      public Bean setValue(String v) {
        this.value = v;
        return this;
      }
      public List<Bean> beans() {
        return Arrays.asList(new Bean().setValue("hello"), new Bean().setValue("world"));
      }
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerMetaProperty(Bean.class, "meta", meta-> meta.data().getValue("value").instance());
    """
    Then the following verification for the instance of java class "Bean" should pass:
    """
    beans::meta[]= [hello world]
    """
