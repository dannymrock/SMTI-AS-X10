package smti.solver;
import smti.util.Logger;
import x10.util.Team;

/** Main
 * 	Main class of the project. 
 * 
 * 	@author Danny Munera
 *  @version 0.1 	9 April, 2013 	-> First Version
 */

import x10.util.OptionsParser;
import x10.util.Option;
import x10.util.Random;
import x10.compiler.Pragma;
import x10.io.File;
import x10.io.FileReader;
import x10.io.FileWriter;


public class Main {
	
	
	var fp  : File;
	public static def main(args:Rail[String]):void{
		var totalTime:Long = -System.nanoTime();
		//val r = new Random();
		val opts = new OptionsParser(args, new Rail[Option](0L), [
		    Option("l", "", "restart Limit"),
		    Option("s", "", "Size of the problem"),
		    Option("b", "", "Number of benchmark tests"),
		    Option("m", "", "Solver mode distribution 0 for Places \"n\" for Activities (n number of activities). Default 0."),
		    Option("t", "", "probability p2 - ties."),
		    Option("c", "", "Probability to change vector in Intra-Team Communication "),
		    Option("R", "", "Intra-team Communication Interval for Receive (iterations) . Default 0 - no communication."),
		    Option("S", "", "Intra-team Communication Interval for Send (iterations) . Default 0 - no communication."),
		    Option("I", "", "Inter-team Communication Interval (tim miliseconds) . Default 0 - no communication."),
		    Option("n", "", "nodes_per_team parameter. Default 4."),
		    Option("k", "", "poolsize."),
		    Option("y", "", "seed. Default random"),
		    Option("d", "", "minimum permisible distance."),
		    Option("p", "", "path"),
		    Option("t", "", "target default 0"),
		    Option("o", "", "output format: machine 0, info 1")
		    ]);
		
		val restLimit	= opts("-l", 1000000000n);
		val size		= opts("-s", 10n);
		val testNo		= opts("-b", 10n);
		val solverMode	= opts("-m", 0n);
		val p2			= opts("-t", 50n);
		val changeProb	= opts("-c", 100n);
		val intraTIRecv	= opts("-R", 0n);
		val intraTISend = opts("-S", 0n);
		val interTI		= opts("-I", 0);
		val nodesPTeam	= opts("-n", 1n);
		val poolSize	= opts("-k", 4n);
		val inSeed		= opts("-y", 0);
		val minDistance	= opts("-d", 0.3);
		var path:String	= opts("-p", "");
		val outFormat	= opts("-o", 1n);
        val target	    = opts("-t", 0n);
		
		var vectorSize:Long=0;
		
		//at(Main.param) Main.param().poolSize = poolSize;
		
		Console.OUT.println("Target: "+(target >= 0n ? "to get equal":"to exceed")+" cost = "+ Math.abs(target));
		
		if (outFormat == 0n){
			Console.OUT.println("Path,size,samples,mode,probChangeVector,intra-Team Recv,intra-Team Send,inter-Team,minDistance,poolsize,places,nodes_per_team,seed");
			Console.OUT.println(path+","+size+","+testNo+","+(solverMode==0n ?"seq":"parallel")+","+changeProb+","+
					intraTIRecv+","+intraTISend+","+interTI+","+minDistance+","+poolSize+","+Place.MAX_PLACES+","+nodesPTeam
					+","+inSeed+"\n");
		}
		else if(outFormat == 1n){
			Console.OUT.println("Size: "+size+"\nNumber of repetitions: "+testNo+
							"\nSolverMode: "+(solverMode==0n ?"seq":"parallel")+
							"\nProbability to Change Vector: "+changeProb+"\nIntra-Team Comm. inteval Recv: "+intraTIRecv+" iterations"+
							"\nIntra-Team Comm. inteval Send: "+intraTISend+" iterations"+
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
		val vectorSz = vectorSize;

		val solvers:PlaceLocalHandle[ParallelSolverI(vectorSz)];	
		solvers = PlaceLocalHandle.make[ParallelSolverI(vectorSz)](PlaceGroup.WORLD, 
				()=>new PlacesMultiWalks(vectorSz, intraTIRecv, intraTISend, interTI, poolSize, nodesPTeam,
						changeProb, minDistance, target) as ParallelSolverI(vectorSz));
			
		if (outFormat == 0n){
			Console.OUT.println("file,count,time(s),iters,place,local_Min,swaps,resets,same/iter,restarts,blocking_pairs,singles,Changes,force_restart,solution,walltime");
		}
		else if(outFormat == 1n){
			Console.OUT.println("| Count | Time (s) |  Iters   | Place  |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| BP  | Sng | Cng  |  FR |  PS | walltime");
			Console.OUT.println("|-------|----------|----------|--------|----------|----------|----------|-------|-----|-----|-----|------|-----|-----|");
		}
		
		// Installing solvers
		var totalInTime :Long = -System.nanoTime();
		finish for (p in Place.places()) at (p) async{	
			solvers().installSolver(solvers);
		}
		val intime = totalInTime += System.nanoTime();
		Logger.debug(()=>{"install time: "+ intime/1e9});
		
		// accumulated times
		var totalExTimes :Long = 0;
		var totalLdTimes :Long = 0;
		var totalClearTimes :Long = 0;
		
		// seed
		val seed = inSeed;//(inSeed == 0) ? j as Long:inSeed;
		val random = new Random(seed);
		
		var j:Int = 0n; //counter of samples
		
		
		// Select files to solve
		//Logger.debug(()=>{"path:"+path});
		val fp = new File(path);
		val execList : Rail[String];
		if (fp.isDirectory()){
			Logger.debug(()=>{"solving all problems into this directory"});
			execList = fp.list();	
		}else{
			//Logger.debug(()=>{"Solving "+testNo+" times the problem "+path});
			execList = [fp.getName()];
			path = fp.getParentFile().getPath();
			//Console.OUT.println(path+" "+fp.getName());
		}
		
		var samplesNb:Int = 0n;
		// val mPref:Rail[Rail[Int]] = new Rail[Rail[Int]](vectorSz, (Long) => new Rail[Int](vectorSz,0n));
		// val wPref:Rail[Rail[Int]] = new Rail[Rail[Int]](vectorSz, (Long) => new Rail[Int](vectorSz,0n));
		
		for (file in execList){
			if(outFormat == 1n){
				Console.OUT.println("|--------------------------------------------------------------------------------------------------------------------|");
				Console.OUT.println("\n--   Solving "+file+" "+testNo+" times");
				Console.OUT.println("|--------------------------------------------------------------------------------------------------------------------|");
			}
			var loadTime:Long = -System.nanoTime();
			//Load first line wtith headers size p1 p2
			val filep = new File(path+"/"+file);//new File(file);//
			if (filep.isDirectory()) continue;
			val fr = filep.openRead();
			val fLine = fr.readLine(); //get first line
			val header = parseFirstLine(fLine);
			val sizeF = header(0); val p1F = header(1); val p2F = header(2);
			Logger.debug(()=>{"file: "+file+" size: "+sizeF+" p1: "+p1F+" p2: "+p2F});
			 		
			//Load Problem
			val mPref:Rail[Rail[Int]] = new Rail[Rail[Int]](sizeF, (Long) => new Rail[Int](sizeF,0n));
			val wPref:Rail[Rail[Int]] = new Rail[Rail[Int]](sizeF, (Long) => new Rail[Int](sizeF,0n));
			readMatrix(fr, sizeF,  mPref, wPref);
			fr.close();
			val cT= loadTime += System.nanoTime();
			totalLdTimes += loadTime;
			Logger.debug(()=>{"Time to load the problem="+cT/1e9});
			samplesNb++;
			//solve the problem "testNo" times
			for (j=1n ; j<=testNo; j++){
				var extTime:Long = -System.nanoTime();
				val cspGen : ()=>SMTIModel(vectorSz);
				val modelSeed = random.nextLong();
				cspGen=():SMTIModel(vectorSz)=> new SMTIModel(sizeF as Long, modelSeed, mPref, wPref,restLimit) as SMTIModel(vectorSz);
				if (solverMode == 0n){
					finish for (p in Place.places()) {
						val solverSeed = random.nextLong();	
						at (p) async
							solvers().solve(solvers, cspGen, solverSeed);	
					}
				}else{
					finish for(var i:Long=Place.MAX_PLACES-1; i>=0; i-=16) at	(Place(i)) async {
						val max = here.id; val min = Math.max(max-15, 0);
						val r = new Random(random.nextLong()+here.id);
						finish for(k in min..max){
							val solverSeed = r.nextLong();
							at(Place(k)) async	solvers().solve(solvers, cspGen, solverSeed);
						}
					}
				}
				Logger.debug(()=>" Main: End solve function  in all places ");
				
				// Detect if there is no winner
				solvers().verifyWinner(solvers);
				
				extTime += System.nanoTime();
				val extt = extTime;
				totalExTimes += extTime;
				Logger.debug(()=>{"ext Time="+extt/1e9});
				
				if(outFormat == 0n){
					Console.OUT.print(file+",");
					solvers().printStats(j,outFormat);
					Console.OUT.println(","+extTime/1e9);
				}
				else if (outFormat == 1n){
					Console.OUT.printf("\r");
					solvers().printStats(j,outFormat);
					Console.OUT.printf(" %3.2f \n",extTime/1e9);
					solvers().printAVG(j,outFormat);
					Console.OUT.flush();
				}
				var clearTime:Long = -System.nanoTime();
				finish for (p in Place.places()) at (p) async{	
					solvers().clear();
				}
				val cltime=clearTime += System.nanoTime();
				totalClearTimes += clearTime;
				Logger.debug(()=>{" cleartime="+cltime/1e9});
				Logger.debug(()=>" Start broadcatFlat: solvers().clear function ");
				
				//System.sleep(1000);
			}
			if(outFormat == 0n){
				Console.OUT.print(file+",");
				solvers().printAVG(testNo,outFormat);
			}
			else if (outFormat == 1n){
				Console.OUT.printf("\r");
				Console.OUT.println("|-------|----------|----------|--------|----------|----------|----------|-------|-----|-----|-----|------|-----|-----|");
				Console.OUT.println("| Count | Time (s) |  Iters   | Place  |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| BP  | Sng | Cng  |  FR |  PS |");
				Console.OUT.println("|-------|----------|----------|--------|----------|----------|----------|-------|-----|-----|-----|------|-----|-----|");
				solvers().printAVG(testNo,outFormat);
				//accStats.printAVG(testNo);
				Console.OUT.printf("\n");
			}
			// Clear sample accumulator
			solvers().clearSample();
		}	
		//Print general avg
		
		if(outFormat == 0n){
			Console.OUT.print("TOTAL,");
			solvers().printGenAVG(samplesNb*testNo,outFormat);
		}
		else if (outFormat == 1n){
			Console.OUT.println("|-------------------------------------------------------------------------------------------------------------------|");
			Console.OUT.println("\n   General Statistics for "+samplesNb+" problems, each one solved "+testNo+" times ");
			Console.OUT.println("|-------|----------|----------|--------|----------|----------|----------|-------|-----|-----|-----|------|-----|-----|");
			Console.OUT.println("| Count | Time (s) |  Iters   | Place  |  LocMin  |  Swaps   |  Resets  | Sa/It |ReSta| BP  | Sng | Cng  |  FR |  PS |");
			Console.OUT.println("|-------|----------|----------|--------|----------|----------|----------|-------|-----|-----|-----|------|-----|-----|");
			solvers().printGenAVG(samplesNb*testNo,outFormat);
			//accStats.printAVG(testNo);
			Console.OUT.printf("\n");
		}
		
		val avgld = totalLdTimes/(samplesNb as Double); 
		val avgclear = totalClearTimes/(samplesNb*testNo as Double); 
		val avgext=totalExTimes/(samplesNb*testNo as Double);
		totalTime += System.nanoTime();
		if(outFormat == 0n){
			Console.OUT.println("Total_Files,Repetition_per_File,Total_Time,AVG_Loading_Time,AVG_external_solving_Time,AVG_clear_Time");
			Console.OUT.println(samplesNb+","+testNo+","+(totalTime/1e9)+","+(avgld/1e9)+","+(avgext/1e9)+","+(avgclear/1e9));
		}
		else if (outFormat == 1n)
			Console.OUT.println("AVG Loading Time= "+(avgld/1e9)+" AVG external solving Time= "+(avgext/1e9)+" AVG clear Time= "+(avgclear/1e9));
		
		return;
	}
	
	static def readMatrix(fr:FileReader, sizeF:Int,  mP:Rail[Rail[Int]], wP:Rail[Rail[Int]]){
		try{
			var i : Int = 0n;
			//var charNo : Int = 0n;
			var j : Int;
			var buffer:String;
			var mline:Int=0n;
			var wline:Int=0n;

			for (line in fr.lines()) {
				i++;
				buffer = ""; j = 0n;
				if (i >= 2n && i < sizeF + 2){
					//Console.OUT.println("mp:"+i+" :"+line);
					// Read Men Pref Matrix
					for(char in line.chars() ){
					 	if( char == ' ' || char == '\n' ){
					 		if(!buffer.equals("")) {
					 			if (j < sizeF){
					 				mP(mline)(j++) = Int.parse(buffer);
					 				//Console.OUT.println("menPrefs "+(mline)+","+(j-1)+" = "+(mP(mline)(j-1)));
					 			}
					 		}
					 		buffer = "";
					 	}else{
					 		buffer += char;
					 	}		 				
					}
					mline++;
				}else if (i > sizeF + 2 && i <= sizeF * 2 + 2){
					//Console.OUT.println("wp:"+i+" :"+line);
					// Read Women Pref Matrix
					for(char in line.chars() ){
						if( char == ' ' || char == '\n' ){
							if(!buffer.equals("")) {
								if (j < sizeF){
									wP(wline)(j++)= Int.parse(buffer);
									//Console.OUT.println("womenPref "+(wline)+","+(j-1)+" = "+(wP(wline)(j-1)));
								}
							}
							buffer = "";
						}else{
							buffer += char;
						}		 				
					}
					wline++;
				}
			}
		}catch(Exception){
			Console.OUT.println("Error reading file");
			//EOF
		}
	}

	static def parseFirstLine(line : String):Rail[Int]{
		var i : Int;
		var j : Int = 0n;
		var buffer:String =  "";
		val x = new Rail[Int](3,0n);
		for(i = 0n ; i < line.length() ; i++){
			if( line(i) == ' ' || line(i) == '\n' ){
				x(j++) = Int.parse(buffer);
				//Console.OUT.println("x "+(j-1)+" = "+x(j-1));
				buffer = "";
			}else{
				buffer += line(i);
			}
		}
		x(j) = Int.parse(buffer);
		//Console.OUT.println("x "+j+" = "+x(j));
		return x;
	}
	
}
