package tacp.jvmMonitors

/* A particular resource is shared by threads, each of which has an
 * integer-valued identity.  A new thread may start to use the resource only
 * when the sum of the identities of the threads currently using it is
 * divisible by 3. */

/** The interface for corresponding lock object. */
trait Mod3{
  def enter(id: Int): Unit
  def exit(id: Int): Unit
}

/** An implementation using a JVM monitor. */
class Mod3M extends Mod3{
  /** Sum of identites of threads currently in critical region. */
  private var current = 0

  /** Enter the critical region. */
  def enter(id: Int) = synchronized{
    while(current%3 != 0) wait()
    current += id
    if(current%3 == 0) notify()
  }

  /** Leave the critical region. */
  def exit(id: Int) = synchronized{
    current -= id
    if(current%3 == 0) notify()
  }
}
