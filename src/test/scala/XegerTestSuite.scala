import org.scalatest.FunSuite
import regex.Xeger

/**
 * Created by Joseph Kiran on 6/10/2014.
 */
class XegerTestSuite extends FunSuite{

  test("Xeger produces required number of values"){
    assert(Xeger.generate("""[A-Za-z0-9]{20}""", 100).size === 100)
  }
}
