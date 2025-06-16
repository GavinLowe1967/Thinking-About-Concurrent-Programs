package tacp.util.profiling

/** An object to perform meory profiling. */
object MemoryProfiler{
  /** Get the size, in bytes, ob obj. 
    * Note: this does not recursively follow references. */
  def getSize(obj: Object): Long = InstrumentationAgent.getObjectSize(obj)

  // ======== Support for traversals

  /** Objects seen so far. */
  private var seen = new AnyRefSet(512, 32)

  /** Add obj to the seen set.  Return true if it wasn't there already. */
  private def add(obj: Object): Boolean = seen.add(obj)

  def size = seen.size

  /** Inaccessible fields seen so far. */
  private val inaccessible = new scala.collection.mutable.HashSet[String]

  // inaccessible.add(
  //   "cachedConstructor[class java.lang.reflect.Field]"

  /** Reset the seen set, allowing objects to be revisited. */
  def reset = { seen.reset; inaccessible.clear() }

  private val formatter = java.text.NumberFormat.getNumberInstance

  /** Print a Long with commas. */
  private def printLong(n: Long): String = formatter.format(n)

  /* Class variables, corresponding to the correspionding parameters of
   * traverse. */
  private var maxPrint = -1
  private var maxPrintArray = -1
  private var ignore = List[String]()

  /** Traverse obj with name name. 
    * 
    * The traversal ignores any object seen on a previous traversal since the
    * last call of reset.
    * @param ignore list of class names to ignore.
    * @param maxPrint the maximum depth at which to print.
    * @param maxPrintArray the maximum number of entries in an array to print. 
    * @return the total number of objects encountered.
    */
  def traverse(name: String, obj: Object, ignore: List[String] = List(), 
               maxPrint: Int = 1, maxPrintArray: Int = 2): Long = {
    this.maxPrint = maxPrint; this.maxPrintArray = maxPrintArray
    this.ignore = ignore
    if(obj != null) traverse1(name, obj,  0)
    else{ print(s"$name: null"); 0 }
  }

  /* Note: traverse1 is recursive, and uses a lot of call stack space,
   * potentially giving a stack overflow.  It might be better to use an
   * explicit stack and recursion. */

  /** Recursively traverse obj, with name name, printing out details of memory
    * usage.  Objects in seen are ignored. 
    * @param indent the amount of indentation to use. 
    * @param static is this a static field? 
    * @return the total number of objects encountered. */
  private def traverse1(
    name: String, obj: Object, indent: Int, static: Boolean = false)
      : Long = {
    val theIndent = ". "*indent; val theClass = obj.getClass
    val className0 = theClass.toString.split(Array('.')).last
    var total = 0L
    if(indent <= maxPrint){
      val objString = 
        try{ obj.toString.take(140) } catch { case _: Throwable => "???" }
      print(s"$theIndent$name: ${objString} [$theClass]")
      if(static) print(" (static)")
    }
    if(!ignore.contains(className0) && add(obj)){
      val theSize = getSize(obj); total += theSize
      if(indent <= maxPrint) println(s" -> ${printLong(theSize)}")

      if(theClass.isArray && theClass.getName.length != 2){
        // primitive type arrays have length two, skip them (they are
        // included in the shallow size).
        /* Process the ith field in the array. */
        def processField(i: Int) = {
          val nextObj = java.lang.reflect.Array.get(obj, i)
          if(nextObj != null){
            val c = traverse1(s"$name($i)", nextObj, 
              if(i < maxPrintArray) indent+1 else maxPrint+1)
            synchronized{ total += c }
          }
          else if(i < maxPrintArray && indent < maxPrint) 
            println(s"$theIndent. $name($i): null")
        }
        val length = java.lang.reflect.Array.getLength(obj)
        if(indent <= maxPrint) println(theIndent+"length "+length)
        for(i <- 0 until length) processField(i)
      }
      else{
        // Traverse up class hierarchy from theClass
        var currentClass: Class[_] = theClass 
        do {
          /* Process a single field. */
          def processField(field: java.lang.reflect.Field) = {
            if (!field.getType.isPrimitive) {
              // The try ... catch below is a hack, but I don't know how to do
              // better.
              try{
                field.setAccessible(true)
                val static =
                  java.lang.reflect.Modifier.isStatic(field.getModifiers)
                val tempObject = field.get(obj)
                if (tempObject != null){
                  val c = traverse1(field.getName, tempObject, indent+1, static)
                  synchronized{ total += c }
                }
              }
              catch{ 
                case e: java.lang.reflect.InaccessibleObjectException =>
                  val key = field.getName+"["+field.getClass+"]"
                  if(inaccessible.add(key))
                     println(s"$theIndent$key inaccessible")
              }
            }
          } // end of processField
          val fields = currentClass.getDeclaredFields
          for(field <- fields) processField(field)
          currentClass = currentClass.getSuperclass
        } while (currentClass != null) ;
      } // end of else
    } // end of outer if
    else if(indent <= maxPrint) println("(ignored)")
    if(indent <= maxPrint) println(theIndent+printLong(total))
    total
  }
}

// ==================================================================

/** A set of AnyRefs, based on object equality.
  * @param shards the number of shards to use, a power of 2.
  * @param initLength the initial length of each shard, a power of 2. */
class AnyRefSet(shards: Int, initLength: Int = 32){
  /** Bit shift to produce a value in [0..shards).  */
  protected val shardShift = {
    var s = shards; var ss = 0 // Inv shards = s * 2^ss
    while(s > 1){ 
      assert((s&1) == 0, "shards is not a power of 2"); s = s >>> 1; ss += 1 
    }
    32-ss
  }

  // Check initLength is a power of 2
  var l = initLength
  while(l > 1){ assert((l&1) == 0, "initLength is not a power of 2"); l = l/2 }

  /** The shard to use for a value with hash h; the most significant
    * bits of h. */
  @inline protected def shardFor(h: Int) = h >>> shardShift

  /** Hash value to use for x. */
  @inline def hashOf[A](x: A): Int = 
    scala.util.hashing.byteswap32(System.identityHashCode(x))
  // scala.util.hashing.byteswap32(x.hashCode)

  /** Maximum load factor before resizing. */
  private val MaxLoad = 0.3 // 0.68 

  // -------- The main data

  /** Array holding the objects. */
  private val objects = Array.ofDim[AnyRef](shards, initLength)

  /** Locks.  lock(i) protects objects(i). */
  private val locks = Array.fill(shards)(new AnyRef)

  /** The number of elements in each shard. */
  private val sizes = Array.fill(shards)(0)

  /** Bit mask for each shard; masks(i) = objects(i).length-1. */
  private val masks = Array.fill(shards)(initLength-1)

  /** Threshold for resizing of each shard. */
  private val thresholds = Array.fill(shards)((initLength*MaxLoad).toInt)

  // -------- The public operations.

  /** Add obj to the set.  
    * @return true if obj was not previously in the set. */
  def add(obj: AnyRef): Boolean = {
    assert(obj != null)
    val h = hashOf(obj); val sh = shardFor(h)
    locks(sh).synchronized{
      val shard = objects(sh); val mask = masks(sh); var i = h&mask
      if(sizes(sh) >= thresholds(sh)){ resize(sh); add(obj) }
      else{
        //var count = 0
        // if(sh == 0) println("Adding ")
        while(!shard(i).eq(obj) && shard(i) != null){
          //   if(count > 100 && sh%16 == 0 && count%20 == 0)
          //     println(sh+" "+i+" "+count+" "+h+" "+mask+" "+obj.hashCode)
          i = (i+1)&mask
          // count += 1; if(count >= 500) sys.exit // assert(count < 1000)
        } // ; if(count == 100) sys.exit
        if(shard(i) == null){ shard(i) = obj; sizes(sh) += 1; true }
        else false
      }
    }
  }

  /** Resize shard sh. 
    * Pre: this thread has the lock for sh. */
  private def resize(sh: Int) = {
    val oldObjs = objects(sh); val oldSize = oldObjs.length; 
    val newSize = oldSize*2; val mask = newSize-1
    // println("resize "+sh+" from "+oldSize)
    val newObjs = new Array[AnyRef](newSize)
    for(i <- 0 until oldSize){
      val obj = oldObjs(i)
      if(obj != null){ // insert obj in newObjs
        var j = hashOf(obj)&mask
        while(newObjs(j) != null) j = (j+1)%mask
        newObjs(j) = obj
      }
    }
    objects(sh) = newObjs; masks(sh) = mask
    thresholds(sh) = (newSize*MaxLoad).toInt
  }

  /** Total number of objects stored. */
  def size = sizes.sum

  /** Reset to empty state. */
  def reset = {
    for(sh <- 0 until shards){
      objects(sh) = new Array[AnyRef](initLength); sizes(sh) = 0
      masks(sh) = initLength-1; thresholds(sh) = (initLength*MaxLoad).toInt
    }
  }

}
