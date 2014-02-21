package smti.solver;
import smti.util.*;
/** CSPStats
 * 	This class implements a container for the CSP solver statistics. 
 * 
 * <p> Methods may be invoked concurrently, at the place recording
 * stats for the overall execution. A monitor is used internally to provide
 * atomic access to the mutable state on this calss.
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 12, 2013 -> First Version
 */

public class CSPStats{
	/** Final Cost of solution */
	var cost : Int=-1n;	
	/** Team id solution */
	var team : Int=-1n;
	/** explorer id solution */
	var explorer : Int=-1n;
	/** time to reach the solution */
	var time : Double=0.0d;
	/** Number of iterations */
	var iters : Int=0n;
	/** Number of local minimum */
	var locmin : Int=0n;
	/** Number of swaps */
	var swaps : Int=0n;
	/** Number of resets */
	var reset : Int=0n;
	/** number of same variables */
	var same : Int=0n;
	/** number of restarts */  
	var restart : Int=0n;
	/** Number time to change vector due to communication */ 
	var change : Int=0n;
	/** number of restarts */  
	var forceRestart:Int = 0n;
    /** Variables for SMTI */
    /** number of BP */
    var bp:Int = 0n;
    /** number of singles */
    var singles:Int = 0n;
    /** acc perfect mariages */
    var accPM:Int = 0n;
    
	 
	transient val monitor:Monitor  = new Monitor("CSPStats");
	
	/**
	 * 	Set statistics to the object
	 * 	@param p place
	 * 	@param t time
	 * 	@param it iterations
	 * 	@param loc local minimum
	 * 	@param sw swaps
	 * 	@param re resets
	 * 	@param sa same variableplace
	 * 	@param rs restarts
	 */
	public def setStats(co : Int, p : Int, e : Int, t:Double, it:Int, loc:Int, sw:Int, re:Int, sa:Int, rs:Int, ch:Int, 
			fr : Int, bp:Int, sg:Int){
	    //monitor.atomicBlock(()=> {
	    	//Console.OUT.println(here+" set stats for: "+p);
	        this.cost = co;
	        this.team = p;
	        this.explorer = e;
	        this.time = t;
	        this.iters = it;
	        this.locmin = loc;
	        this.swaps = sw;
	        this.reset = re;
	        this.same = sa;
	        this.restart = rs;
	        this.change = ch;
	        this.forceRestart = fr;
	        this.bp = bp;
	        this.singles = sg;
	      //  Unit()
	    //});
	}
	
	
	
	/**
	 *  Accumulate statistics in this object, Is used for average calculation
	 * 	@param stats Object with solver data to accumulate 
	 */
	public def accStats(stats:CSPStats){
	    //monitor.atomicBlock(() => {
	        this.time += stats.time;
	        this.iters += stats.iters;
	        this.locmin += stats.locmin;
	        this.swaps += stats.swaps;
	        this.reset += stats.reset;
	        this.same += stats.same;
	        this.restart += stats.restart;
	        this.change += stats.change;
	        this.forceRestart += stats.forceRestart;
	        this.bp += stats.bp;
	        this.singles += stats.singles;
	        
	        if(stats.bp == 0n && stats.singles == 0n)
	        	accPM++;
	       // Unit()
	   // });
	}
	
	/**
	 * 	Print the stat values
	 * 	@param count Number of this iteration
	 */
	public def print(count:Int, oF:Int){
		val sameIter : Double = same /iters;
		//val changeF : Double = (change as Double)/count;
		if (oF == 0n){
			Console.OUT.println(count+"\t"+time+"\t"+iters+"\t"+team+"\t"+explorer+"\t"+locmin+"\t"+swaps
					+"\t"+reset+"\t"+sameIter+"\t"+restart+"\t"+bp+"\t"+singles+"\t"+change+"\t"+forceRestart+"\t"+((bp == 0n && singles == 0n)?1n:0n));
		}else{
			Console.OUT.printf("|  %3d  | %8.4f | %8d | %2d-%2d | %8d |",count, time, iters, team, explorer, locmin);
			Console.OUT.printf(" %8d | %8d | %5.1f | %3d | %3d | %3d |",swaps,reset,sameIter,restart, bp, singles);
			Console.OUT.printf(" %4d | %3d | %3d |\n", change, forceRestart,((bp == 0n && singles == 0n)?1:0));
		}
	}

	/**
	 * 	Print the stat averages
	 * 	@param no total number of iterations
	 */
	public def printAVG(no:Int, oF:Int){ 
	   // val no = no1 as Double;
		val sameIter : Double = (same as Double)/iters;
		val changeF : Double = (change as Double)/no;
		if (oF == 0n){
			Console.OUT.print("AVG\t"+time/no+"\t"+iters/no+"\t"+locmin/no+"\t"+swaps/no+"\t"+reset/no
					+"\t"+sameIter+"\t"+restart/no+"\t"+bp/(no as float)+"\t"+singles/(no as Double)
					+"\t"+changeF+"\t"+forceRestart/no+"\t"+accPM);
		}else{
			Console.OUT.printf("|avg-%3d| %8.4f | %8d |  N/A  | %8d |", no, time/no, iters/no, locmin/no);
			Console.OUT.printf(" %8d | %8d | %5.1f | %3d | %3.1f | %3.1f | ",swaps/no,reset/no,sameIter,restart/no,
				bp/(no as float), singles/(no as Double));
			Console.OUT.printf("%4.1f | %3d |", changeF, forceRestart/no);
			Console.OUT.printf(" %3d |\n",accPM);
		}
	}
	
	public def clear():void{ 
		cost = -1n;	
		team = -1n;
		explorer = -1n;
		time = 0.0d;
		iters = 0n;
		locmin = 0n;
		swaps = 0n;
		reset = 0n;
		same = 0n;
		restart = 0n;
		change = 0n;
		forceRestart = 0n;
		
		bp = 0n;
		singles = 0n;
		accPM = 0n;
	}
	
}