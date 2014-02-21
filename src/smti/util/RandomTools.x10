package smti.util;
/** RandomTools 
 * 	This class has some random tools
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 10, 2013  First Version
 */
import x10.util.Random; 

public class RandomTools { 
	var seed: Long; 
	val r = new Random();
	
	public def this(seedIn: Long){
		 seed=seedIn;
		 r.setSeed(seedIn);
	} 
	
	public def setSeed(s:Long){
		r.setSeed(s);
		seed=s;
	}
	
	public def randomPermut( sz : Long , baseValue : Int ) : Rail[Int]
	{
		val size =sz as Int;
		val vec  = new Rail[Int] (size, (k:Long) => baseValue + k as Int);
				
		for(var i:Int = size - 1n; i > 0n ; i--)
		{
			val j = r.nextInt( i + 1n );
			val z = vec(i);
			vec(i) = vec(j);
			vec(j) = z;
		}
		return vec;		
	}
	
	/**
	 * 	randomArrayPermut
	 * 	Generate a random permutation of a given vector of size elements
	 * 	@param vec Vector with initial values
	 * 	@param size 
	 * 	@return
	 */
	public def randomArrayPermut( vec : Rail[Int] ) : Rail[Int]{self==vec}{
	
		for(var i:Long = vec.size - 1; i > 0 ; i--)
		{
			val j = randomInt(i + 1);
			val z = vec(i);
			vec(i) = vec(j);
			vec(j) = z;
		}
		return vec;	
	}
	
	
	public def randomInt(limit : Long)=r.nextInt(limit as Int);		
	public def randomLong()=r.nextLong();		
	public def randomDouble()=r.nextDouble();
	
}