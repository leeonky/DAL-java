Feature: data remark

  Scenario: support remark after property with ()
    Given the following java class:
    """
    public class Bean {
      public String value = "hello";
    }
    """
    Given the following java class:
    """
    public class BeanRef {
      public Bean bean = new Bean();
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerDataRemark(Bean.class, rd-> rd.data().map(i->rd.remark()+i));
    """
    When the following verification for the instance of java class "Bean" should pass:
    """
    : {
      bean(World)= helloWorld
      ['bean'](World)= helloWorld
    }
    """

#    remark on property
#    remark on group
#    remark on table header property
#    remark on row header with default index
#    remark on row header with row index
#    remark on row header with row key
#    raise error when no remark register

