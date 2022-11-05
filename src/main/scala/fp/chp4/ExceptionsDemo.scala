object ExceptionsDemo:

  def mean(xs: Seq[Double]): Double = 
    if xs.isEmpty then throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length 

  def mean2(xs: Seq[Double], onEmpty: Double): Double = 
    if xs.isEmpty then onEmpty 
    else xs.sum / xs.length 


    
