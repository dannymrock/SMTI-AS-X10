/**	This class containts all the basic configuration info, for the
 * 	Adaptive search solver x10 implementation ASSolverPermut
 * 	
 * 	@autor Danny Munera
 * 	
 * Every place has an ASSolverPermutRW. This points to an ASSolverConf.
 * comm is stored in ASSolverPermutRW.
 */
public class ASSolverConf(sz:Long, poolSize:Int) {
	
	public static USE_ACTIVITIES  = 0n; 
	public static USE_PLACES  = 1n;
	
	public static NO_COMM = 0n;
	public static ALL_TO_ZERO = 1n;
	public static ALL_TO_ALL = 2n;
	public static ALL_TO_NEIGHBORS = 3n;
	public static TEAM = 4n;
	
	/** Solver use activities or places */
	var solverMode : Int;
	
	/** Number of iterations between each communication activity */
	var intraTI : Int;
	
	/** Number of iterations between each communication activity */
	var interTI : Int;
	
	/** inter-places reset enable */
	var commOption : Int;
	
	/** probability of change vector if bad cost */
	//val pChange : Int;
	
	var delta : Int=0n;
	
	val nbTeams : Int;
	val myTeamId : Int;
	val random = new x10.util.Random();
	
	
	/**
	 * The reference to all team members, for communication.
	 */
	val solvers:PlaceLocalHandle[ParallelSolverI(sz)];
	
	def this( sz:Long, solverModeIn : Int , ss: PlaceLocalHandle[ParallelSolverI(sz)], 
	        intraTeamI : Int, interTeamI : Int , cOption : Int , ps : Int, nT : Int){
		property(sz, ps);
		solvers = ss;
	    solverMode = solverModeIn;
		intraTI = intraTeamI;
		interTI = interTeamI;
		commOption = cOption;
		nbTeams = nT;
		myTeamId = here.id as Int % nbTeams;
		
		
		
		
		//Console.OUT.println("I'm "+here.id+ " and my group is "+myGroupId);
	}
	
	public def setValues(toSet: ASSolverConf{self.sz==this.sz}){
		this.solverMode = toSet.solverMode;
		this.intraTI = toSet.intraTI;
		this.interTI = toSet.interTI;
	}
	/**
	 * 	communicate the vector if Searching thread totalCost is better than worstCost in the pool
	 * 
	 */
	public def communicate(totalCost : Int, variables : Rail[Int]{self.size==sz} ) {
	    if (commOption==NO_COMM) { 
	        return;
	    }
	    Logger.debug(()=>" communicate: entering.");
	    if (solverMode == USE_PLACES) {
	        /************************** Comm Places *******************************/
	        //Console.OUT.println("Solver Mode USE_PLACES, communication interval= "+commI);
	        val placeid = here.id as Int;
	        val ss = solvers;
	        //val variables = csp.variables; 
	        
	        // All-to-one place 0
	        if (commOption == ALL_TO_ZERO){
	            //Console.OUT.println("All-to-one");
	        	finish at(Place(0)) async ss().tryInsertVector( totalCost , variables, placeid);
	        }else if(commOption == ALL_TO_ALL){
	            // All-to-All	
	            //Console.OUT.println("All-to-all");
	            for (p in Place.places()) 
	                if (here != p) 
	                    finish at(p) async ss().tryInsertVector( totalCost , variables, placeid);
	        }else if (commOption == ALL_TO_NEIGHBORS){ 
	            //Neighbors
	            //Console.OUT.println("Neighbors");
	            val placeup = here.id + 1;
	            val placedown = here.id  - 1;
	            if (placeup < Place.MAX_PLACES){
	                finish at(Place(placeup)) async ss().tryInsertVector( totalCost , variables, placeid);
	            }
	            if (placedown >= 0L){
	            	finish at(Place(placeup)) async ss().tryInsertVector( totalCost , variables, placeid);
	            }
	        } else if(commOption == TEAM){
	        	//val r = arrayRefs(myGroupId);
	        	finish at(Place(myTeamId)) async ss().tryInsertVector( totalCost , variables, placeid);
	        }
	        
	        //Debug
	        // if(here.id  == myGryoupId){ //group heed
	        //   	Console.OUT.println("I'm "+myGroupId+" head group, here my pool Vectors");
	        //   	at(arrayRefs(myGroupId))arrayRefs(myGroupId)().printVectors();
	        // }
	        /*********************************************************/
	    }else if (solverMode == USE_ACTIVITIES){
	        //Console.OUT.println("Solver Mode USE_ACTIVITIES, communication interval= "+commI);
	    }else{
	        Console.OUT.println("ERROR: Unknown solver mode");
	    }
	    return;
	}
	
	
	//public def getRandomVector( ) : Rail[Int]{ 
		//val vectorOut = (at(commRef)commRef().getVector());
		//return vectorOut;
	//}
	
	def communicationTarget():Place {
	    val P=Place.MAX_PLACES;
	    if (commOption == ALL_TO_ZERO)  return Place.FIRST_PLACE;
	    if (commOption == ALL_TO_NEIGHBORS) {
	    	return Place(here.id);
	        //if (here.id+1 < P) return Place(here.id+1);
	        //if (here.id-1 >= 0) return Place(here.id-1);
	    }
	    if (commOption == ALL_TO_ALL) return Place(here.id);
	    if (commOption == TEAM) return Place(myTeamId);
	   
	    return Place.FIRST_PLACE;
	}
	/**
	 *  get Inter Place Vector.This should be considered to have 
	 * modified csp_ in place, if the return value is 1n (success).
	 * If the return value is -1n (fail), csp_ will not be changed.
	 * 
	 */
	public def getIPVector(csp_ : SMTIModel(sz), myCost : Int):Boolean { // csp renamed csp_ to avoid issue with codegen in managed backend
		if (commOption == NO_COMM) return false;
		Logger.debug(()=> " getIPVector: entering.");
	    val place:Place=communicationTarget();
		val ss=solvers;
		val a : Maybe[CSPSharedUnit(sz)];
		finish a = at(place) ss().getPoolData();
		//if (place.id==0)Console.OUT.println(here+" comm to "+place+" and get "+a().cost);
		if ( a!=null && (myCost + delta) > a().cost ){					 
		    csp_.setVariables(a().vector);
		    return true; 
		}
		return false;
	}
		
}
public type ASSolverConf(s:Long)=ASSolverConf{self.sz==s};
