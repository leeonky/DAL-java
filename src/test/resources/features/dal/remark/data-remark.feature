Feature: data remark

  Scenario: support remark after property with ()
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
    }
    """
    Given the following java class:
    """
    public class Beans {
      public Bean hello = new Bean("hello");
      public List<BeanRef> beanRefs = Arrays.asList(new BeanRef(new Bean("b1")), new BeanRef(new Bean("b2")));
      public List<Bean> beans = Arrays.asList(new Bean("b1"), new Bean("b2"));
    }
    """
    And register DAL:
    """
    dal.getRuntimeContextBuilder().registerDataRemark(Bean.class, rd-> rd.data().map(i->((Bean)i).value+rd.remark()));
    """
    When the following verification for the instance of java class "Beans" should pass:
    """
    : {
#      hello(World)= helloWorld,
#      ['hello'](World)= helloWorld
#
#      beanRefs: | bean(remark) |
#                | b1remark     |
#                | b2remark     |
#
#      beanRefs: >>| bean(remark) | b1remark | b2remark |
#
#      {}:           | toUpperCase |
#       hello(World) | HELLOWORLD  |

      beans:    | toUpperCase |
        (hello) | B1HELLO     |
        (World) | B2WORLD     |

#      beans: | >>          | (hello) | (World) |
#             | toUpperCase | B1WORLD | B2WORLD |
#

#      beans:     | toUpperCase |
#        0(World) | B1WORLD     |
#        1(World) | B2WORLD     |
    }
    """

#    remark on row header with default index
#    remark on row header with row index
#    raise error when no remark register
#    precedence

