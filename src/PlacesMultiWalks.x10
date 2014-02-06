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
    
    /** 
     * 	Solve the csp problem with MAX_PLACES instance of AS solver
     * 	The first one that reach a valid solution sends a kill to the others
     * 	to finish the process.
	 * 
	 * 	@param size size of the csp problem
	 * 	@param cspProblem code with the problem to be solved (1 for Magic Square Problems, other number for Queens Problem)
	 * 	@return cost of the solution
	 */
    public def solve(st:PlaceLocalHandle[ParallelSolverI(sz)], cspGen:()=>SMTIModel(sz) ):void { 
    	val solvers = st;
    	assert solvers() == this : "Whoa, basic plumbing problem -- I am not part of solvers!";
    	val size = sz as Int;
    	var extTime : Long = -System.nanoTime();
    	
    	val random = new Random();
    	
    	//val seed = random.nextLong();
    	//Console.OUT.println("seed:"+seed);
    	var nsize:Int = size;
    	
    	csp_ = cspGen(); // use the supplied generator to generate the problem
    	
    	//conf = new ASSolverConf(sz, 1n /*ASSolverConf.USE_PLACES*/, solvers, updateI,0n, commOption, poolSize, nTeams );
    	commM = new CommManager(sz, 0n , solvers, updateI,0n, poolSize, nTeams );
    	val ss = st() as ParallelSolverI(sz);
    	//solver = new ASSolverPermut(sz, nsize, seed, ss);
    	
    	//Do I need to get more elegant way to obtain different seeds here? 
    	//I'm currently using the solver id (place id) as seed
    	solver = new ASSolverPermut(sz, nsize, here.id, ss);
    	
    	var cost:Int = x10.lang.Int.MAX_VALUE;
    	
    	/***/
    	//taking the time only to solve the problem, not included the time to signal the others explorers
    	
    	Logger.debug(()=>"  PlacesMultiWalks: Start solve process: solver.solve() function ");
    	
    	time = -System.nanoTime();
    	cost = solver.solve(csp_);
    	
    	
    	if (cost == 0n){ //TODO: Define a new condition (It's possible to finish without cost=0)
    		// A solution has been found! Huzzah! 
    		// Light the candles! Kill the blighters!
    		val home = here.id;
    		
    		val winner = at(Place.FIRST_PLACE) solvers().announceWinner(solvers, home);
    		
    		//winPlace = here;
    		bcost = cost;
    		
    		if (winner) {
    			csp_.displaySolution(solver.bestConf as Valuation(sz));
    			time += System.nanoTime();
    			setStats1(solvers);
    			//Utils.show("Solution is " + (csp_.verified()? "ok" : "WRONG") , csp_.variables);
    			Console.OUT.println("Solution is " + (csp_.verified(solver.bestConf as Valuation(sz))? "perfect" : "not perfect"));
    			//csp_.displaySolution();
    		}
    	}
    	extTime += System.nanoTime();
    	time += System.nanoTime();
    	//stats.time = extTime/1e9;
    	//val stats_=stats;
    	//Logger.debug(()=> "updating accStats");
    	// accumulate results in place 0, need a better way at scale.
    	//at (Place.FIRST_PLACE)  st().accStats(stats_);
    	//if (!winner) 
    	//at (Place.FIRST_PLACE) st().selBestSol();
    	if (!solver.kill){
    		//copy vector
    		Rail.copy(solver.bestConf, bestC);
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
		solver.kill=true;
	}
    val winnerLatch = new AtomicBoolean(false);
    public def announceWinner(ss:PlaceLocalHandle[ParallelSolverI(sz)], p:Long):Boolean {
    	val result = winnerLatch.compareAndSet(false, true);
    	
    	Logger.debug(()=> "  PlacesMultiWalks: announceWinner result=" + result + " for " + p + " this=" + this );
    	if (result) {
    		for (k in Place.places()) 
    			if (p != k.id) 
    				at(k) async ss().kill();
    	}
    	return result;
    }
    /**
     * Called by winning place to set the stats at place zero so they
     * can be printed out.
     */
    public def setStats1(ss:PlaceLocalHandle[ParallelSolverI(sz)]  ){
    	val winPlace = here.id;
    	val time = time/1e9;
    	val iters = solver.nbIterTot;
    	val locmin = solver.nbLocalMinTot;
    	val swaps = solver.nbSwapTot;
    	val reset = solver.nbResetTot;
    	val same = solver.nbSameVarTot;
    	val restart = solver.nbRestart;
    	val change = solver.nbChangeV;
        val bp = solver.bestnbBP;
        val singles = solver.bestnbSG;
    	
    	at (Place.FIRST_PLACE) 
    	ss().setStats(0n, winPlace as Int, 0n, time, iters, locmin, swaps, reset, same, restart, change,0n, bp, singles);
    }
    public def setStats(co : Int, p : Int, e : Int, t:Double, it:Int, loc:Int, sw:Int, re:Int, sa:Int, rs:Int, ch:Int, 
    		fr : Int, bp:Int, sg:Int) {
    	stats.setStats(co, p, e, t, it, loc, sw, re, sa, rs, ch, fr, bp, sg);
    	accStats(stats);
    }
    
	public def printStats(count:Int):void {
	    stats.print(count);
	}
	public def printAVG(count:Int):void {
	    accStats.printAVG(count);
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
    }
    public def accStats(c:CSPStats):void {
		accStats.accStats(c);
    }
	
	public def getCurrentData():Maybe[CSPSharedUnit(sz)]{
		return null;
	}
	
	public def getBP():Int{
		return solver.bestnbBP;
	}
	public def getCost():Int{
		return solver.bestCostSMTI;
	}
	
	public def verifyWinner(ss:PlaceLocalHandle[ParallelSolverI(sz)]):void{
		// detect if no winner has been found
		// search best solution in all places
		// set stats objects
		var minBP:Int = sz as Int + 1n;
		var minCost:Int = x10.lang.Int.MAX_VALUE;
		var bestPlace:Place = here; 
		
		if (stats.explorer == -1n){
			Logger.info(()=>"no winner found");
			
			for (k in Place.places()){
				val cBP = at(k) ss().getBP();
				val cCost = at(k) ss().getCost();
				
				if(cBP < minBP){
					minBP = cBP;
					minCost = cCost;
					bestPlace = k;
				} else if(cBP == minBP){
					if( cCost < minCost){
						minBP = cBP;
						minCost = cCost;
						bestPlace = k;
					}
				}
			}
			val p = bestPlace; val bp = minBP; val cost = minCost;
			Logger.info(()=>"best place="+p+" BP= "+bp+" singles"+(cost-bp));
			
			
			at (bestPlace){
				//csp_.displaySolution(solver.bestConf as Valuation(sz));
				ss().setStats1(ss);
				//Utils.show("Solution is " + (csp_.verified()? "ok" : "WRONG") , csp_.variables);
				//Console.OUT.println("Solution is " + (csp_.verified(solver.bestConf as Valuation(sz))? "perfect" : "not perfect"));
				//csp_.displaySolution();
			}
		}
	}
	
}
public type PlacesMultiWalks(s:Long)=PlacesMultiWalks{self.sz==s};
