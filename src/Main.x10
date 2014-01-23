import x10.util.Team;

/** Main
 * 	Main class of the project. 
 * 
 * 	@author Danny Munera
 *  @version 0.1 	9 April, 2013 	-> First Version
 */

import x10.io.File;
import x10.io.FileWriter;
import x10.util.OptionsParser;
import x10.util.Option;
import x10.util.Random;

public class Main {
	public static def main(args:Rail[String]):void{
	
		val r = new Random();
		val opts = new OptionsParser(args, new Rail[Option](0L), [
		    Option("p", "", "Problem Selection (magic-square, costas, all-interval or langford)"),
		    Option("s", "", "Size of the problem"),
		    Option("b", "", "Number of benchmark tests"),
		    Option("m", "", "Solver mode distribution 0 for Places \"n\" for Activities (n number of activities). Default 0."),
		    Option("t", "", "Using threads."),
		    Option("c", "", "Communication option: 0 no comm 1 for \"place 0\", 2 for all-to-all and 3 for neighbors"),
		    Option("i", "", "Intra-team Communication Interval (iterations) . Default 0 - no communication."),
		    Option("j", "", "Inter-team Communication Interval (tim miliseconds) . Default 0 - no communication."),
		    Option("n", "", "nodes_per_team parameter. Default 4."),
		    Option("k", "", "poolsize."),
		    Option("d", "", "minimum permisible distance.")
		    ]);
		
		val cspProblem  = opts("-p", "MSP");
		val size        = opts("-s", 10n);
		val testNo      = opts("-b", 10n);
		val solverMode  = opts("-m", 0n);
		val threads     = opts("-t", 0n);
		val comm        = opts("-c", 0n);
		val intraTI     = opts("-i", 0n);
		val interTI     = opts("-j", 0);
		val nodesPTeam  = opts("-n", 1n);
		val poolSize    = opts("-k", 4n);
		val minDistance = opts("-d", 0.3);
		var vectorSize:Long=0;
		//at(Main.param) Main.param().poolSize = poolSize;

		Console.OUT.println("CSP Problem: "+cspProblem+" Size: "+size+"\nNumber of repetitions: "+testNo+
							"\nSolverMode: "+(solverMode==0n ?"Only Places":"Hybrid (Places and Activities)")+
							"\nCommunication strategy: "+comm+"\nIntra-Team Comm. inteval: "+intraTI+" iterations"+
							"\nInter-Team Comm. inteval: "+interTI+" ms"+"\nMinimum permissible distance: "+minDistance+
							"\nPool Size: "+poolSize);
		
				
		Logger.debug(()=>{"Stable Marriage Problem"});
		vectorSize=size;
				
		/*
		 *  Creating objects for solver execution
		 */
		val accStats = new CSPStats();
		val vectorSz = vectorSize;
		val solvers:PlaceLocalHandle[ParallelSolverI(vectorSz)];
		
		Console.OUT.println("Using multi-walks with "+Place.MAX_PLACES+" Places");
		Console.OUT.println("There are "+Place.MAX_PLACES/nodesPTeam+" teams each one with "+nodesPTeam+" explorer places. "+
			Place.MAX_PLACES+" explorers in total (places)");
			
		solvers = PlaceLocalHandle.make[ParallelSolverI(vectorSz)](PlaceGroup.WORLD, 
				()=>new PlacesMultiWalks(vectorSz, intraTI, interTI, threads, poolSize, nodesPTeam) as ParallelSolverI(vectorSz));
			
				
		Console.OUT.println("|Count| Time (s) |  Iters   | Place |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| Change|  FR |");
		Console.OUT.println("|-----|----------|----------|-------|----------|----------|----------|-------|-----|-------|-----|");
		
		/*
		 *  Execution loop
		 */
		for (var j : Int = 1n; j <= testNo ; j++ ){
			
			//Solve the problem
			//val stats:CSPStats;
			val random = new Random();
			val seed = random.nextLong();
			val cspGen:()=>SMTIModel(vectorSz);
			
			val mPref = SMTIModel.createPrefs(size as Long, r.nextLong());
			val wPref = SMTIModel.createPrefs(size as Long, r.nextLong());
			cspGen=():SMTIModel(vectorSz)=> new SMTIModel(size as Long, seed, mPref, wPref) as SMTIModel(vectorSz) ;
			
			Logger.debug(()=>" Start broadcatFlat: solvers().solve function ");
			
			finish for (p in Place.places()) at (p) async{
				solvers().solve(solvers, cspGen);
			}
			
			//Logger.debug(()=>" End broadcastFlat: solvers().solve function");
			
			Console.OUT.printf("\r");
			solvers().printStats(j);
			solvers().printAVG(j);
			Console.OUT.flush();
			
			Logger.debug(()=>" Start broadcatFlat: solvers().clear function ");
			
			
			finish for (p in Place.places()) at (p) async{	
				solvers().clear();
			}

			Logger.debug(()=>" Start broadcatFlat: solvers().clear function ");
		}
		Console.OUT.printf("\r");
		Console.OUT.println("|-----|----------|----------|-------|----------|----------|----------|-------|-----|-------|-----|");
		solvers().printAVG(testNo);
		//accStats.printAVG(testNo);
		Console.OUT.printf("\n");
		
		return;
	}

	
}
