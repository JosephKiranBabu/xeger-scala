package regex

/**
 * Created by Joseph Kiran on 6/10/2014.
 */
import java.util.Random
import dk.brics.automaton.RegExp
import dk.brics.automaton.State
import dk.brics.automaton.Transition
import scala.beans.BeanProperty
import scala.annotation.tailrec

/**
 * An object that will generate text from a regular expression. In a way, it's the opposite of a regular expression
 * matcher: an instance of this class will produce text that is guaranteed to match the regular expression passed in.
 */
object Xeger {

  @BeanProperty val random = new Random()

  /**
   *Generates a random String based on the given regular expression
   *
   * @param regex
   * @return
   */
  def generate(regex: String, size:Int): List[String] = {
    val automaton = new RegExp(regex).toAutomaton()
    val builder = new StringBuilder()
    @tailrec
    def loop(i:Int, list:List[String]):List[String] = {
      if (i == size) list
      else {
        generate(builder, automaton.getInitialState)
        loop(i+1, list:+builder.toString)
      }
    }
    loop(0,Nil)
  }

  /**
   * Generates a random String that is guaranteed to match the regular expression.
   *
   * @param builder
   * @param state
   */
  private def generate(builder: StringBuilder, state: State) {
    val transitions = state.getSortedTransitions(false)
    if (transitions.size == 0) {
      assert(state.isAccept)
      return
    }
    val nroptions = if (state.isAccept) transitions.size else transitions.size - 1
    val option = Xeger.getRandomInt(0, nroptions, random)
    if (state.isAccept && option == 0) {
      return
    }
    val transition = transitions.get(option - (if (state.isAccept) 1 else 0))
    appendChoice(builder, transition)
    generate(builder, transition.getDest)
  }

  /**
   *
   * @param builder
   * @param transition
   */
  private def appendChoice(builder: StringBuilder, transition: Transition) {
    val c = Xeger.getRandomInt(transition.getMin, transition.getMax, random).toChar
    builder.append(c)
  }

  /**
   * Generates a random number within the given bounds.
   *
   * @param min The minimum number (inclusive).
   * @param max The maximum number (inclusive).
   * @param random The object used as the randomizer.
   * @return A random number in the given range.
   */
  def getRandomInt(min: Int, max: Int, random: Random): Int = {
    val maxForRandom = max - min + 1
    random.nextInt(maxForRandom) + min
  }
}