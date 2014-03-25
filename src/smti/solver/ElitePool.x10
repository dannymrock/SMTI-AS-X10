package smti.solver; 
import smti.util.Logger;
import smti.util.Monitor;
import smti.util.Unit;
import smti.util.Utils;
import x10.util.Random;
/**
 * Maintain a poolSize set of best partial solutions. These
 * can be queried by other places.
 * 
 */

public class ElitePool(sz:Long, poolSize:Int/*, seed:Long*/) {
	var nbEntries : Int = 0n;
	val bestPartialSolutions = new Rail(poolSize, CSPSharedUnit(sz,0n as Int,null,0n as Int)); // dummy value
	var random:Random = new Random();
	val monitor = new Monitor("ElitePool");
	
	
	public def setSeed(seed:Long){
		//monitor.atomicBlock(()=> {
		random=new Random(seed);
		//});
	}
	
	
	/**
	 * Insert a copy of variables in the best partial solutions. Intended to be 
	 * invoked by solvers running at remote places.
	 * Note: Check that all calls are from remote places. If so the copy of
	 * variables will already have happened.
	 */
	public def tryInsertVector(cost:Int, variables:Rail[Int]{self.size==sz}, place:Int) {
		monitor.atomicBlock(()=>tryInsertVector0(cost,variables,place));		
	}

	protected def tryInsertVector0( cost : Int , variables : Rail[Int]{self.size==sz}, place : Int ):Unit {
		var victim:Int;
		if( nbEntries < poolSize ){
			victim = nbEntries++;
		}else{
			// No place available select a victim
			
			victim = -1n;
		
			for (i in 0n..(nbEntries-1n)){
				if (cost < bestPartialSolutions(i).cost){
					victim = i;
				} else if (cost == bestPartialSolutions(i).cost && compareVectors(variables, bestPartialSolutions(i).vector)){
					victim = -1n;
					break;
				}
			}
		}
		if (victim >= 0n) {
			//Console.OUT.println("insert vector with cost "+cost);	
			bestPartialSolutions(victim) = new CSPSharedUnit(variables.size, cost, Utils.copy(variables), place);
		}
		
		return Unit();
	}
	
	
	public static def compareVectors (vec1 : Rail[Int], vec2 : Rail[Int]):Boolean{
		for (i in 0..( vec1.size-1))
			if(vec1(i) != vec2(i)) return false;
		return true;
	}
	
	public def printVectors(){
		for(i in 0..(nbEntries-1)) {
			Console.OUT.print(i+". Cost = "+bestPartialSolutions(i).cost+" place "+bestPartialSolutions(i).place);
			Utils.show(" Vector",bestPartialSolutions(i).vector);
		}
	}
	
	/**
	 * Get some vector from the best solutions.
	 */
	public def getRemoteData():Maybe[CSPSharedUnit(sz)]=
		monitor.atomicBlock(()=> {
			//if (here.id==0)Console.OUT.println(here+"aqui");
			if (nbEntries < 1n) return null;
			val index = random.nextInt(nbEntries);
			//if (index >= nbEntries) Console.OUT.println("Golden: index is " + index + " needed to be < " + nbEntries);
			//if (here.id==0)Console.OUT.println(here+"alli");
			return new Maybe(bestPartialSolutions(index));
		});
	  
		
	public def clear(){
	    monitor.atomicBlock(()=> {
	        nbEntries = 0n;
	        Unit()
	    });
	}
	
}

