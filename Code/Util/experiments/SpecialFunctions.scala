package tacp.util.experiments

/** Some functions used to calculate confidence intervals.
  * 
  * Adapted from code by Bryan Lewis, Department of Mathematics and Computer
  * Science, Kent State University. */
object SpecialFunctions{
  /** An approximation of gamma(x). */
  def gamma(x0: Double) = {
    require(x0 > 0.0)
    var f = 10E99; var g = 1.0; var x = x0
    while (x < 3){ g = g * x; x = x + 1 }
    val x2 = x*x
    f = (1 - 2/(7*x2) * (1 - 2/(3*x2))) / (30*x2)
    f = (1-f)/(12*x) + x*(Math.log(x)-1)
    (Math.exp(f)/g) * Math.sqrt(2*Math.PI/x)
  }

  /** A continued fraction representation of the beta function. */
  def betacf(a: Double, b: Double, x: Double) : Double = {
    val maxIterations = 50; var m = 1
    val eps = 3E-5; var am = 1.0; var bm = 1.0; var az = 1.0
    val qab = a+b; val qap = a+1; val qam = a-1
    var bz = 1 - qab*x/qap
    var aold = 0.0
    // double em, tem, d, ap, bp, app, bpp;
    while(m < maxIterations && Math.abs(az-aold) >= eps*Math.abs(az)){
      val tem = m+m;
      val d1 = m * (b-m) * x /((qam+tem) * (a+tem))
      val ap = az+d1*am; val bp = bz+d1*bm
      val d = -(a+m) * (qab+m) * x / ((a+tem) * (qap+tem))
      val app = ap+d*az; val bpp = bp+d*bz
      aold = az; am = ap/bpp; bm = bp/bpp; az = app/bpp; bz = 1
      m += 1
    }
    az
  }

  /** The incomplete beta function from 0 to x with parameters a, b. */
  def betai(a: Double, b: Double, x: Double) : Double = {
    require(0.0 <= x && x <= 1.0, s"x = $x")
    val bt = 
      if(x == 0 || x== 1) 0
      else gamma(a+b)*Math.pow(x,a)*Math.pow(1-x,b)/(gamma(a)*gamma(b))
    if(x<(a+1)/(a+b+2)) bt*betacf(a,b,x)/a
    else 1-bt*betacf(b,a,1-x)/b
  }


  def main(args: Array[String]) = {
    // Test against specialFunctions.java
    assert(specialFunctions.gamma(1.2) == gamma(1.2))
    assert(specialFunctions.gamma(4.3) == gamma(4.3))
    assert(specialFunctions.betacf(1.4, 1.2, 1.2) == betacf(1.4, 1.2, 1.2))
    assert(specialFunctions.betacf(0.4, 0.3, 0.2) == betacf(0.4, 0.3, 0.2))
    assert(specialFunctions.betai(1.4, 1.2, 0.2) == betai(1.4, 1.2, 0.2))
    assert(specialFunctions.betai(0.4, 0.3, 0.5) == betai(0.4, 0.3, 0.5))
    println("Done")
  }


}
