
package smti.solver;
import smti.util.*;
/** PlaceMultiWalk is the parallel implementation of Random Walk Adaptive Search solver
 * 	in the X10 language. This implementation use distributed isolated instances
 * 	of the solver, each one with a diferent seeds in order to have differents 
 * 	scanning walks in the search space.
 * 
 *  This implementation distribute the solver instances across places.
 * 
 * 	@author Danny Munera
 *  @version 0.1 	9 April, 2013  -> First Version
 * 					10 April, 2013 -> Changes queens by costas problem
 * 					12 April, 2013 -> TLP support
 */
import x10.util.Random;

import x10.array.*;
import x10.compiler.Inline;
import x10.util.concurrent.AtomicBoolean; 
import x10.util.Team;

/**
 * Each place has solvers, a PlaceLocalHandle[PlaceMultiWalk(sz)].
 * The standard way for code at place p to see the state at place q is
 * to execute 
 * <verbatim>
 * at(q) async { 
 *    val thisSolver = solvers(); 
 *    ... now thisSolver.csp gets you the local model,
 *    ...  thisSolver.solver gets you the local solver,
 *    ... etc
 *  >
 * </verbatim>
 */
public class PlacesMultiWalks(sz:Long,poolSize:Int) implements ParallelSolverI {  
    property sz()=sz;
    // Shared state, accessible from any place, via at(
    var csp_:SMTIModel(sz);
    var solver:ASSolverPermut(sz);
    var time:Long;
	
    val updateI : Int;
    //val commOption : Int;
	
    var bcost : Int;
    val stats = new CSPStats();
    val accStats = new CSPStats();
    /** Comunication Variables*/
    var commM : CommManager(sz);
    //Hybrid approach
    val nbExplorerPT : Int;
    val nTeams : Int;
    
    val bestC = new Rail[Int](sz,0n); 
    
    var seed:Long;
	
    /**
     * 	Constructor of the class
     */
    public def this(vectorSize:Long, upI : Int, interTI : Long , thread : Int , ps : Int, npT : Int ){
    	property(vectorSize,ps);
    	updateI = upI; 
    	//commOption = commOpt;
    	nbExplorerPT = npT; // will be a parameter 
    	nTeams = Place.MAX_PLACES as Int / nbExplorerPT ;
    	
    	
    	
    }
    //var solvers:PlaceLocalHandle[ParallelSolverI(sz)];
    
    
    
    public def installSolver(st:PlaceLocalHandle[ParallelSolverI(sz)]):void{
   
    	Logger.debug(()=>{"Installing solver"});
 
    	val ss = st() as ParallelSolverI(sz);
    	val size = sz as Int;
    	var nsize:Int = size;
    	solver = new ASSolverPermut(sz, nsize, /*seed,*/ ss);
    	commM = new CommManager(sz, 0n , st, updateI,0n, poolSize, nTeams );
    }
    	
    
    /** 
     * 	Solve the csp problem with MAX_PLACES instance of AS solver
     * 	The first one that reach a valid solution sends a kill to the others
     * 	to finish the process.
     * 
     * 	@param size size of the csp problem
     * 	@param cspProblem code with the problem to be solved (1 for Magic Square Problems, other number for Queens Problem)
     * 	@return cost of the solution
     */
    public def solve(st:PlaceLocalHandle[ParallelSolverI(sz)], cspGen:()=>SMTIModel(sz), seed_ :Long ):void { 
    	val solvers = st;
    	assert solvers() == this : "Whoa, basic plumbing problem -- I am not part of solvers!";
    	
    	this.seed = seed_;
    	val random = new Random(seed);
    	//val random = new Random(here.id);
    	
    	var cost:Int = x10.lang.Int.MAX_VALUE;
    	
    	
    	solver.setSeed(random.nextLong()); 
    	commM.setSeed(random.nextLong());
    	
    	//Logger.info(()=>{"   Seed in solver:"+seed});
    	
    	
    	csp_ = cspGen(); // use the supplied generator to generate the problem
    	    	
    	Logger.debug(()=>"  PlacesMultiWalks: Start solve process: solver.solve() function ");
    	
    	time = -System.nanoTime();
    	cost = solver.solve(csp_);
    	time += System.nanoTime();
    	
    	// Logger.debug(()=>"  PlacesMultiWalks: end solve process: solver.solve() function ");
    	if (cost == 0n){ //TODO: Define a new condition (It's possible to finish without cost=0)
    		// A solution has been found! Huzzah! 
    		// Light the candles! Kill the blighters!
    		val home = here.id;
    		
    		val winner = at(Place.FIRST_PLACE) solvers().announceWinner(solvers, home);
    		
    		//winPlace = here;
    		bcost = cost;
    		
    		if (winner) {
    			setStats_(solvers);
    			//Utils.show("Solution is " + (csp_.verified()? "ok" : "WRONG") , csp_.variables);
    			//csp_.displaySolution2(solver.bestConf as Valuation(sz));
    			//Console.OUT.println("Solution is " + (csp_.verified(solver.bestConf as Valuation(sz))? "perfect" : "not perfect"));
    			csp_.verified(solver.bestConf as Valuation(sz));
    		}
    	}
    }
	
    @Inline public def getIPVector(csp_:SMTIModel(sz), myCost:Int):Boolean 
								 = commM.getIPVector(csp_, myCost);
    public def communicate(totalCost:Int, variables:Rail[Int]{self.size==sz}){
	commM.communicate(totalCost, variables);
    }
	
    @Inline public def intraTI():Int = commM.intraTI;
	
	//val monitor = new Monitor("PlacesMultiWalks"); 
	public def kill() {
		if (solver != null) {
			solver.kill = true; //solver.kill.set(true); //
			Logger.debug(()=>{"Kill=true"});
		}else{
			Logger.info(()=>{"Solver is not yet started. Kill is not set"});
			
		}
	}
    val winnerLatch = new AtomicBoolean(false);
    public def announceWinner(ss:PlaceLocalHandle[ParallelSolverI(sz)], p:Long):Boolean {
    	//Logger.debug(()=> "  PlacesMultiWalks: announceWinner " );
    	val result = winnerLatch.compareAndSet(false, true);
    	
    	Logger.debug(()=> "  PlacesMultiWalks: announceWinner result=" + result + " for " + p + " this=" + this );
    	if (result) {
    		for (k in Place.places()) 
    			if (p != k.id) 
    				at(k)  ss().kill(); // at(k) async ss().kill();  // Testing the use of this async v1
    	}
    	Logger.debug(()=> "  PlacesMultiWalks: announceWinner all kill messages are sent" );
    	
    	return result;
    }
    /**
     * Called by winning place to set the stats at place zero so they
     * can be printed out.
     */
    public def setStats_(ss:PlaceLocalHandle[ParallelSolverI(sz)]  ){
    	val winPlace = here.id;
    	val time = time/1e9;
    	val iters = solver.nbIterTot;
    	val locmin = solver.nbLocalMinTot;
    	val swaps = solver.nbSwapTot;
    	val reset = solver.nbResetTot;
    	val same = solver.nbSameVarTot;
    	val restart = solver.nbRestart;
    	val change = solver.nbChangeV;
        val bp = solver.bestCost/sz;
        val singles = solver.bestCost - bp;
    	
    	at (Place.FIRST_PLACE) async 
    	ss().setStats(0n, winPlace as Int, 0n, time, iters, locmin, swaps, reset, same, restart, change,0n, 
    			bp as Int, singles as Int);
    }
    public def setStats(co : Int, p : Int, e : Int, t:Double, it:Int, loc:Int, sw:Int, re:Int, sa:Int, rs:Int, ch:Int, 
    		fr : Int, bp:Int, sg:Int) {
    	stats.setStats(co, p, e, t, it, loc, sw, re, sa, rs, ch, fr, bp, sg);
    	accStats(stats);
    }
    
	public def printStats(count:Int, oF:Int):void {
	    stats.print(count,oF);
	}
	public def printAVG(count:Int, oF:Int):void {
	    accStats.printAVG(count,oF);
	}
	public def tryInsertVector(cost:Int, variables:Rail[Int]{self.size==sz}, place:Int) {
		commM.ep.tryInsertVector(cost, variables, place);
	}
	public def getPoolData():Maybe[CSPSharedUnit(sz)]=commM.ep.getRemoteData();
	
    public def clear(){
		winnerLatch.set(false);
		commM.restartPool();
		stats.clear();
		bestC.clear();
		solver.clear();
    }
    public def accStats(c:CSPStats):void {
		accStats.accStats(c);
    }
	
	public def getCurrentData():Maybe[CSPSharedUnit(sz)]{
		return null;
	}
	
	public def getCost():Int{
		return solver.bestCost;
	}
	
	public def verifyWinner(ss:PlaceLocalHandle[ParallelSolverI(sz)]):void{
		// detect if no winner has been found
		// search best solution in all places
		// set stats objects
		//var minBP:Int = sz as Int + 1n;
		var minCost:Int = x10.lang.Int.MAX_VALUE;
		var bestPlace:Place = here; 
		
		if (stats.explorer == -1n){
			Logger.info(()=>"No winner found");
			
			for (k in Place.places()){
				//val cBP = at(k) ss().getBP();
				val cCost = at(k) ss().getCost();

				if(cCost < minCost){
					minCost = cCost;
					bestPlace = k;
				}
				// if(cBP < minBP){
				// 	minBP = cBP;
				// 	minCost = cCost;
				// 	bestPlace = k;
				// } else if(cBP == minBP){
				// 	if( cCost <= minCost){
				// 		minBP = cBP;
				// 		minCost = cCost;
				// 		bestPlace = k;
				// 	}
				// }
			}
			val p = bestPlace; val bp = minCost/sz; val cost = minCost;
			Logger.debug(()=>"best "+p+" BP= "+bp+" singles= "+(cost-bp));
			
			
			at (bestPlace){
				//csp_.displaySolution(solver.bestConf as Valuation(sz));
				ss().setStats_(ss);
				//Utils.show("Solution is " + (csp_.verified()? "ok" : "WRONG") , csp_.variables);
				//Console.OUT.println("Solution is " + (csp_.verified(solver.bestConf as Valuation(sz))? "perfect" : "not perfect"));
				//csp_.displaySolution();
				Console.OUT.println("");
			}
		}
	}
	
}
public type PlacesMultiWalks(s:Long)=PlacesMultiWalks{self.sz==s};
