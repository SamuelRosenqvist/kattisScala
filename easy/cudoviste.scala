//https://open.kattis.com/problems/cudoviste
object cudoviste{
    def main(args: Array[String]): Unit = {
        val free  = "."
        val house = "#"
        val car   = "X"

        val cars_crushed = Array.fill[Int](5)(0)

        val r = args(0).toInt
        val c = args(1).toInt
        val matrix = args.drop(2).grouped(c).toArray

        val spots = getSpaces(r,c,matrix)

        spots.foreach((e: Array[String]) =>
            if(!e.contains(house)){
                cars_crushed(e.filter(_==car).length) += 1
            }
        )
        
        cars_crushed.foreach(println)

    }

    def getSpaces(r: Int, c: Int, m: Array[Array[String]]): Array[Array[String]] = {
        var e = (r-1)*(c-1)
        var t = 0
        val n = Array.fill[Array[String]](e)(Array("0"))
        for(x <- 0 until c-1; y <- 0 until r-1){
            n(t) = Array(   m(x)(y), m(x)(y+1),
                            m(x+1)(y), m(x+1)(y+1))
            t+=1
        }
        return n
        }
    
}