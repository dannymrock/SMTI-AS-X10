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
		    Option("d", "", "probability p1 - Deletions"),
		    Option("s", "", "Size of the problem"),
		    Option("b", "", "Number of benchmark tests"),
		    Option("m", "", "Solver mode distribution 0 for Places \"n\" for Activities (n number of activities). Default 0."),
		    Option("t", "", "probability p2 - ties."),
		    Option("c", "", "Communication option: 0 no comm 1 for \"place 0\", 2 for all-to-all and 3 for neighbors"),
		    Option("i", "", "Intra-team Communication Interval (iterations) . Default 0 - no communication."),
		    Option("j", "", "Inter-team Communication Interval (tim miliseconds) . Default 0 - no communication."),
		    Option("n", "", "nodes_per_team parameter. Default 4."),
		    Option("k", "", "poolsize."),
		    Option("y", "", "seed. Default random"),
		    Option("x", "", "minimum permisible distance."),
		    Option("o", "", "output format: machine 0, info 1")
		    ]);
		
		val p1			= opts("-d", 50n);
		val size		= opts("-s", 10n);
		val testNo		= opts("-b", 10n);
		val solverMode	= opts("-m", 0n);
		val p2			= opts("-t", 50n);
		val comm		= opts("-c", 0n);
		val intraTI		= opts("-i", 0n);
		val interTI		= opts("-j", 0);
		val nodesPTeam	= opts("-n", 1n);
		val poolSize	= opts("-k", 4n);
		val inSeed		= opts("-y", 0);
		val minDistance	= opts("-x", 0.3);
		val outFormat	= opts("-o", 1n);
		
		var vectorSize:Long=0;
		
		//at(Main.param) Main.param().poolSize = poolSize;
		
		if (outFormat == 0n){
			Console.OUT.println("Parameters\tP1="+p1+"\tP2="+p2+"\tsize="+size+"\tsamples="+testNo
					+"\tmode="+(solverMode==0n ?"onlyPlaces":"hybrid")+"\tcomm="+comm+"\tintra-Team="
					+intraTI+"\tinter-Team="+interTI+"\tminDistance="+minDistance+"\tpoolsize="+poolSize
					+"\tplaces="+Place.MAX_PLACES+"\tnpT="+nodesPTeam);
		}else{
			Console.OUT.println("Prob P1 deletions: "+p1+"\n Prob P2 - ties:"+p2+"\n Size: "+size+"\nNumber of repetitions: "+testNo+
							"\nSolverMode: "+(solverMode==0n ?"Only Places":"Hybrid (Places and Activities)")+
							"\nCommunication strategy: "+comm+"\nIntra-Team Comm. inteval: "+intraTI+" iterations"+
							"\nInter-Team Comm. inteval: "+interTI+" ms"+"\nMinimum permissible distance: "+minDistance+
							"\nPool Size: "+poolSize);

			Console.OUT.println("Using multi-walks with "+Place.MAX_PLACES+" Places");
			Console.OUT.println("There are "+Place.MAX_PLACES/nodesPTeam+" teams each one with "+nodesPTeam+" explorer places. "+
					Place.MAX_PLACES+" explorers in total (places)");
			
		}
				
		vectorSize=size;
				
		/*
		 *  Creating objects for solver execution
		 */
		val accStats = new CSPStats();
		val vectorSz = vectorSize;
		val solvers:PlaceLocalHandle[ParallelSolverI(vectorSz)];
			
		solvers = PlaceLocalHandle.make[ParallelSolverI(vectorSz)](PlaceGroup.WORLD, 
				()=>new PlacesMultiWalks(vectorSz, intraTI, interTI, 0n, poolSize, nodesPTeam) as ParallelSolverI(vectorSz));
			
		if (outFormat == 0n){
			Console.OUT.println("seed\tcount\ttime(s)\titers\tplace\tlocMin\tswaps\tresets\tsa/it\treSta\tbp\tsingles\tChanges\tfr\tps\tsolution");
		}else{
			Console.OUT.println("| Count | Time (s) |  Iters   | Place |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| BP  | Sng | Cng  |  FR |  PS |");
			Console.OUT.println("|-------|----------|----------|-------|----------|----------|----------|-------|-----|-----|-----|------|-----|-----|");
		}
		/*
		 *  Execution loop
		 */
		
		val mPref:Rail[Rail[Int]] = new Rail[Rail[Int]](size, (Long) => new Rail[Int](size,0n));
		val wPref:Rail[Rail[Int]] = new Rail[Rail[Int]](size, (Long) => new Rail[Int](size,0n));
		
		for (var j : Int = 1n; j <= testNo ; j++ ){
			
			//Solve the problem
			//val stats : CSPStats;
			
			// val random = new Random();		
			// val seed = (inSeed == 0) ? random.nextLong():inSeed;
			
			val seed = (inSeed == 0) ? j as Long:inSeed;
			
			
			Logger.debug(()=>{"Problem seed: "+seed});
			
			val cspGen : ()=>SMTIModel(vectorSz);
			
			// Passing mPref and wPref by reference
			SMTIModel.createPrefs(p1, p2, size, seed, mPref, wPref);
			
			cspGen=():SMTIModel(vectorSz)=> new SMTIModel(size as Long, seed, mPref, wPref) 
													as SMTIModel(vectorSz);
			
			Logger.debug(()=>" Start broadcatFlat: solvers().solve function ");
			
			finish for (p in Place.places()) at (p) async{
				solvers().solve(solvers, cspGen);
			}
			
			// Detect if there is no winner
			solvers().verifyWinner(solvers);
			
			//Logger.debug(()=>" End broadcastFlat: solvers().solve function");
			if(outFormat == 0n){
				Console.OUT.print(seed+"\t");
				solvers().printStats(j,outFormat);
			}else{
				Console.OUT.printf("\r");
				solvers().printStats(j,outFormat);
				solvers().printAVG(j,outFormat);
				Console.OUT.flush();
			}
			Logger.debug(()=>" Start broadcatFlat: solvers().clear function ");
			
			
			finish for (p in Place.places()) at (p) async{	
				solvers().clear();
			}

			Logger.debug(()=>" Start broadcatFlat: solvers().clear function ");
		}
		if(outFormat == 0n){
			solvers().printAVG(testNo,outFormat);
		}else{
			Console.OUT.printf("\r");
			Console.OUT.println("|-------|----------|----------|-------|----------|----------|----------|-------|-----|-----|-----|------|-----|-----|");
			Console.OUT.println("| Count | Time (s) |  Iters   | Place |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| BP  | Sng | Cng  |  FR |  PS |");
			Console.OUT.println("|-------|----------|----------|-------|----------|----------|----------|-------|-----|-----|-----|------|-----|-----|");
			solvers().printAVG(testNo,outFormat);
			//accStats.printAVG(testNo);
			Console.OUT.printf("\n");
		}
		return;
	}
}
