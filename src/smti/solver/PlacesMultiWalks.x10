
package smti.solver;
import smti.util.Logger;
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
	
    val intraTIRecv : Int;
    val intraTISend : Int;
    //val commOption : Int;
	
    var bcost : Int;
    val stats = new CSPStats();
    val sampleAccStats = new CSPStats();
    val genAccStats = new CSPStats();
    /** Comunication Variables*/
    var commM : CommManager(sz);
    //Hybrid approach
    val nbExplorerPT : Int;
    val nTeams : Int;
    
    val bestC = new Rail[Int](sz,0n); 
    
    var seed:Long;
	
    val changeProb:Int;
    
    
    //InterTeam Communication
    var interTeamKill:Boolean = false;
    val interTeamInterval:Long;
    val minDistance:Double;
    /**
     * 	Constructor of the class
     */
    public def this(vectorSize:Long, intraTIRecv : Int, intraTISend : Int, interTI : Long, ps : Int,
    		npT : Int, changeProb:Int, minDistance:Double){
    	property(vectorSize,ps);
    	this.intraTIRecv = intraTIRecv;
    	this.intraTISend = intraTISend;
    	//commOption = commOpt;
    	nbExplorerPT = npT; // will be a parameter 
    	nTeams = Place.MAX_PLACES as Int / nbExplorerPT ;
    	this.changeProb = changeProb;
    	interTeamInterval = interTI;
    	this.minDistance = minDistance;
    }
    //var solvers:PlaceLocalHandle[ParallelSolverI(sz)];
    
    
    
    public def installSolver(st:PlaceLocalHandle[ParallelSolverI(sz)]):void{
   
    	Logger.debug(()=>{"Installing solver"});
 
    	val ss = st() as ParallelSolverI(sz);
    	val size = sz as Int;
    	var nsize:Int = size;
    	solver = new ASSolverPermut(sz, nsize, /*seed,*/ ss);
    	commM = new CommManager(sz, 0n , st, intraTIRecv, intraTISend ,0n, poolSize, nTeams, changeProb);
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
    	
    	commM.setSeed(random.nextLong());
    	solver.setSeed(random.nextLong()); 
    	
    	
    	
    	//Logger.info(()=>{"   Seed in solver:"+seed});
    	
    	// verify if inter team comm is able, if the number of teams is greater than 1 and 
    	//        if place(here) is a head node 
    	if (interTeamInterval > 0 && nTeams > 1n && here.id < nTeams){
    		val delay = random.nextLong(interTeamInterval);
    		async{
    			System.sleep(delay);
    			interTeamActivity(st, random.nextLong());
    		} 
    	}
    	
    	
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
    			interTeamKill = true;
    			setStats_(solvers);
    			Console.OUT.println("\nerrors "+ err);
    			//Utils.show("Solution is " + (csp_.verified()? "ok" : "WRONG") , csp_.variables);
    			//csp_.displaySolution2(solver.bestConf as Valuation(sz));
    			//Console.OUT.println("Solution is " + (csp_.verified(solver.bestConf as Valuation(sz))? "perfect" : "not perfect"));
    			csp_.verify(solver.bestConf as Valuation(sz));
    		}
    	}
    }
	
    @Inline public def getIPVector(csp_:SMTIModel(sz), myCost:Int):Boolean 
								 = commM.getIPVector(csp_, myCost);
    public def communicate(totalCost:Int, variables:Rail[Int]{self.size==sz}){
	commM.communicate(totalCost, variables);
    }
	
    @Inline public def intraTIRecv():Int = commM.intraTIRecv;
    @Inline public def intraTISend():Int = commM.intraTISend;
    
	//val monitor = new Monitor("PlacesMultiWalks"); 
	public def kill() {
		if (solver != null) {
			solver.kill = true; //solver.kill.set(true); //
			interTeamKill = true;
			Logger.debug(()=>{"Kill=true"});
		}else{
			Logger.debug(()=>{"Solver is not yet started. Kill is not set"});
			
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
    				at(k) ss().kill(); // at(k) async ss().kill();  // Testing the use of this async v1
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
        val singles = solver.bestCost % sz;
        val bp = (solver.bestCost-singles)/sz;
        val fr = solver.nbForceRestart;
    	
    	at (Place.FIRST_PLACE) /*async*/ 
    	ss().setStats(0n, winPlace as Int, 0n, time, iters, locmin, swaps, reset, same, restart, change,fr, 
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
	    sampleAccStats.printAVG(count,oF);
	}
	
	
	public def printGenAVG(count:Int, oF:Int):void {
		genAccStats.printAVG(count,oF);
	}
	
	public def tryInsertVector(cost:Int, variables:Rail[Int]{self.size==sz}, place:Int) {
		commM.ep.tryInsertVector(cost, variables, place);
	}
	public def getRandomConf():Maybe[CSPSharedUnit(sz)]=commM.ep.getRandomConf();
	
	public def getBestConf():Maybe[CSPSharedUnit(sz)]=commM.ep.getBestConf();
	
    public def clear(){
		winnerLatch.set(false);
		commM.restartPool();
		stats.clear();
		bestC.clear();
		solver.clear();
		interTeamKill = false;
    }
    
    public def clearSample(){
    	sampleAccStats.clear();
    }
    public def accStats(c:CSPStats):void {
		genAccStats.accStats(c);
		sampleAccStats.accStats(c);
    }
	
	// public def getCurrentData():Maybe[CSPSharedUnit(sz)]{
	// 	var sol:CSPSharedUnit(sz) = new CSPSharedUnit(sz,solver.totalCost,csp_.variables, 
	// 			here.id as Int);
	// 	return new Maybe(sol);
	// }
	
	public def getCost():Int{
		return solver.bestCost;
	}
	
	public def verifyWinner(ss:PlaceLocalHandle[ParallelSolverI(sz)]):void{
		// detect if no winner has been found
		// search best solution in all places
		// set stats objects
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
			}
			//val s = minCost % sz; val bp = (minCost-s)/sz;
			//Console.OUT.println("Cost = "+minCost+" Singles= "+s+ "BP= "+bp);
			at (bestPlace){
				ss().setStats_(ss);
				//Console.OUT.println("");
			}
		}
	}
	
	var err:Int=0n;
	/**
	 * Inter Team Communication Functions
	 **/
	public def interTeamActivity(st:PlaceLocalHandle[ParallelSolverI(sz)], seed:Long){
		while (!interTeamKill) {
			if (!System.sleep(interTeamInterval)){ 
				//Logger.info(()=>"interTeamActivity error: cannot execute sleep");
				//Console.OUT.println(here+" interTeamActivity error: cannot execute sleep");
				err++;
				continue;
			}
			//while(commM.ep.countInsert % 10n != 0n);
			
			//Runtime.probe();		// Give a chance to the other activities
			// woken up
			Logger.info(()=>{" interTeamActivity - run : woken up (every "+interTeamInterval+" ms)"});
			//val random = new Random(seed);
			//if (random.nextInt(100n) < 16) 
			interTeamComm(st, seed);
		}
	}
	
	public def interTeamComm(ss:PlaceLocalHandle[ParallelSolverI(sz)], seed:Long){
		Logger.debug(()=>{"MW - interTeamComm : entering..."+nTeams});

		val r = new Random(seed);
		//Compare against a random team  (head node)
		//The Head node for each team is the node with id==team_number
		var remote : Long = r.nextLong(nTeams); 
		while (here.id == remote){
			remote = r.nextLong(nTeams);
		}

		val vremote = remote;
	
		Logger.info(()=>"MW - interTeamComm : Comparing "+here.id+" vs "+vremote);
		// get current configuration and cost from local and  remote Team
		//if(interTeamKill) return;
		
		val localConf = getBestConf();
		val remoteConf = at(Place(remote)) ss().getBestConf();

		//compute distance between Teams
		if( localConf==null || remoteConf==null) {
			Logger.debug(()=>"MW - interTeamComm : null configurations, return");	
			return;
		}
		val dis = distance(localConf().vector, remoteConf().vector);
		val rem = remote;
		Logger.info(()=>{"MW - interTeamComm : distance between "+here.id+" and "+rem+" is= "+dis});

		if (dis < minDistance){ //put parameter
			Logger.info(()=>"MW - interTeamComm : force Restart");
			val teamToRest = localConf().cost < remoteConf().cost ? remote : here.id;
			for (var i:Long = teamToRest; i < Place.MAX_PLACES; i += nTeams){
				//Restart the members of the team "res"
				at(Place(i)) ss().forceRestart();
			}
		}
	}

	def distance(conf1 : Valuation(sz), conf2 : Valuation(sz)) : Double {
		var count : Int = 0n;
		for (i in 0n..(sz as Int - 1n)){
			//Logger.debug("comparing: "+conf1(i)+" - "+conf2(i));
			if(conf1(i) == conf2(i)) count++; 
		}
		val dis = 1.0 - ( count as Double / sz );
		return dis;
	} 
	
	
	public def forceRestart():void{
		if (here.id < nTeams) commM.restartPool();
		solver.forceRestart();
	}
	
	
}
public type PlacesMultiWalks(s:Long)=PlacesMultiWalks{self.sz==s};
