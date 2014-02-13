import x10.util.Random;

/** ASSolverPermut is the implementation of Adaptive Search solver
 * 	in the x10 lenguage.
 *  Implementation specialized in Permuts Problems.
 * 
 *  This x10 code is an adaptation of the Adaptive Search Algoritm C implementation by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 9, 2013 -> first version
 * 				 April 12, 2013 -> Exahustive search implemented
 * 	
 */

public class ASSolverPermut(sz:Long, size:Int, seed:Long, solver:ParallelSolverI(sz)) {

    val mark = new Rail[Int] (size, 0n); 
	val solverP = new ASSolverParameters();

	//var nb_var_to_reset : Int; 
	
	var maxI : Int;		
	var minJ : Int;
	
	var bestCost : Int;
	var newCost : Int;
	var totalCost : Int;
	val random = new RandomTools(seed);
	var kill : Boolean=false;
	var forceRestart : Boolean = false;
	
	var listInb : Int;
	var listJnb : Int;
	
	val listIJ = new Rail[PairAS](size);
	val listI = new Rail[Int](size, 0n);
	
	var nbVarMarked : Int = 0n; 
	//val varRegion : Region(1);
	/** Number of iterations to update kill status */
	//val updateP : Int;
	
	/**	Statistics	*/
	var nbRestart : Int=0n;
	var nbForceRestart : Int=0n;
	var nbIter : Int;
	var nbReset : Int;
	var nbSwap : Int;
	var nbSameVar : Int;
	var nbLocalMin : Int;
	/** Number time to change vector due to communication */ 
	var nbChangeV : Int=0n;
	
	/** Total Statistics */
	var nbIterTot : Int;
	var nbResetTot : Int;	
	var nbSwapTot : Int;
	var nbSameVarTot : Int;
	var nbLocalMinTot : Int; 
	
	/** For Exhaustive search */
	var nbListIJ : Int;
	
	
	/** Diversification approach **/
	var alMaxI : Int;
	var alMinJ : Int;
	
	/**
	 * Optimization mode - SMTI 
	 */
	val bestConf = new Rail[Int](size, 0n);
	var bestCostSMTI:Int = x10.lang.Int.MAX_VALUE;
	var bestnbBP:Int = x10.lang.Int.MAX_VALUE;
	var bestnbSG:Int = x10.lang.Int.MAX_VALUE;
	
	/**
	 *  solve( csp : SMTIModel ) : Int
	 *  Solve a csp Problem through the Adaptive Search algoritm
	 * 	@param csp The model of the problem to solve
	 *  @return the final total cost after solving process (If success returns 0)
	 */ 
	public def solve( csp_ : SMTIModel{self.sz==this.sz} ) : Int { //
		var nbInPlateau:Int; 
		csp_.setParameters(solverP);
		
		//nb_var_to_reset = (((size * solverP.resetPercent) + (100) - 1) / (100));
		if (solverP.nbVarToReset == -1n){
			solverP.nbVarToReset = (((size * solverP.resetPercent) + (100n) - 1n) / (100n));
			if (solverP.nbVarToReset < 2n){
				solverP.nbVarToReset = 2n;
				Logger.debug(()=>{"increasing nb var to reset since too small, now = "+ solverP.nbVarToReset});
			}
		}
		
		csp_.initialize(solverP.baseValue); //Set_Init_Configuration Random Permut
		//Main.show("initial= ",csp.variables);
		
		mark.clear();
		listI.clear();
		
		nbRestart = 0n;
		nbSwap = 0n;
		nbIter = 0n;
		nbSameVar = 0n;
		nbLocalMin = 0n;
		nbReset = 0n;
		nbChangeV = 0n;
		
		nbInPlateau = 0n;
		
		nbIterTot = 0n;
		nbResetTot = 0n;	
		nbSwapTot = 0n;
		nbSameVarTot = 0n;
		nbLocalMinTot = 0n; 
		
		totalCost = csp_.costOfSolution(true);
		bestCost = totalCost;
		var bestOfBest: Int = x10.lang.Int.MAX_VALUE ;
		//var slope : Int = 0;
		//var antcost : Int = totalCost;
		
		// Copy the first match to bestConf vector
		Rail.copy(csp_.getVariables(),bestConf as Valuation(sz));
		bestCostSMTI = totalCost;
		bestnbBP = csp_.getnbBP();
		bestnbSG = csp_.getnbSingles();
		//Console.OUT.println("initial bestCost="+bestCostSMTI);
		
		while( totalCost != 0n ){
			if (bestCost < bestOfBest)
				bestOfBest = bestCost;
	
			nbIter++;
	  
			if (nbIter >= solverP.restartLimit){
				if(nbRestart < solverP.restartMax){
					//restart
					forceRestart = false;
					csp_.initialize(solverP.baseValue); //Set_Init_Configuration Random Permut
					nbRestart++;
					restartVar();
					bestCost = totalCost = csp_.costOfSolution(true);
					bestOfBest = x10.lang.Int.MAX_VALUE ;
					nbInPlateau = 0n;
					solver.clear();
					continue;
				}
				//Console.OUT.println("Not solution found");
				break; 
			}
			
			if( !solverP.exhaustive ){
				maxI = selectVarHighCost( csp_ );
				//Console.OUT.print("maxI= "+maxI);
				minJ = selectVarMinConflict( csp_ );
				//Console.OUT.println("  minJ= "+minJ);
			} else {
				selectVarsToSwap( csp_ );
				//Console.OUT.println("maxI= "+maxI+"  minJ= "+minJ);
			}
			
			//Console.OUT.println("----- iter no: "+nbIter+", cost: "+totalCost+", nb marked: "+nbVarMarked+" ---, nb_swap= "+nbSwap);
			
			if (totalCost != newCost) {
				if (nbInPlateau > 1n) {
			 		//Console.OUT.println("end of plateau, length: "+ nbInPlateau);
			 	}
			 	nbInPlateau = 0n;
			}
			if (newCost < bestCost) bestCost = newCost;
			
			nbInPlateau++;
			
			if (minJ == -1n) continue; //What??
			
	 		if (maxI == minJ) {
	 			//val res = solverC.communicate(totalCost, csp,commRefs);
	 			//if (minJ != alMinJ)
	 				//Console.OUT.println("lminJ = "+ minJ+ " alMinJ = "+alMinJ);
	 			
	 			nbLocalMin++;
				mark(maxI) = nbSwap + solverP.freezeLocMin; //Mark(maxI, freeze_loc_min);
				//Console.OUT.println("nbVarMarked "+nbVarMarked+"solverP.resetLimit= "+solverP.resetLimit);
	 			if (nbVarMarked + 1 >= solverP.resetLimit)
	 			{				
	 				// do reset or get some vector from the comm pool
	 				/*if (random.randomInt(100) < solverP.probChangeVector){
	 					val result = solverC.getIPVector(csp, totalCost );
	 					if (result == -1)
	 						doReset(solverP.nbVarToReset,csp);//doReset(nb_var_to_reset,csp);
	 					else{
	 						nbChangeV++;
	 						nbSwap += size ; //I don't know what happened here with costas reset
	 						mark.clear();
	 						totalCost = csp.costOfSolution(1);
	 					}
	 				}else{*/
	 				
	 				
		 				//Console.OUT.println("\tTOO MANY FROZEN VARS - RESET");
		 				doReset(solverP.nbVarToReset,csp_);//doReset(nb_var_to_reset,csp);
		 				//Main.show("after reset= ",csp.variables);
	 				//}
	 				
	 				
	 			}
			} else {
				mark(maxI) = nbSwap + solverP.freezeSwap; //Mark(maxI, ad.freeze_swap);
				mark(minJ) = nbSwap + solverP.freezeSwap; //Mark(minJ, ad.freeze_swap);
			
				csp_.swapVariables(maxI, minJ);//adSwap(maxI, minJ,csp);
				nbSwap++;
				csp_.executedSwap(maxI, minJ);
				totalCost = newCost;
				
				//slope = antcost - totalCost;
				//antcost = totalCost;
				//Console.OUT.println("slope in "+here.id+" : "+slope+ " total cost : "+totalCost);
			}
	 		
			// 	Utils.show("partial sol",csp_.getVariables());
			//csp_.displaySolution();
			
	 		/**
	 		 *  optimization
	 		 */
	 		
	 		if(totalCost < bestCostSMTI){
	 			Rail.copy(csp_.getVariables(),bestConf as Valuation(sz));
	 			bestCostSMTI = totalCost;
	 			bestnbBP = csp_.getnbBP();
	 			bestnbSG = csp_.getnbSingles();
	 		}
	 		
	 		
	 		// var cnbBP:Int = csp_.getnbBP(); 
	 		// if(cnbBP < bestnbBP){
	 		// 	Rail.copy(csp_.getVariables(),bestConf as Valuation(sz));
	 		// 	bestCostSMTI = totalCost;
	 		// 	bestnbBP = cnbBP;
	 		// 	bestnbSG = csp_.getnbSingles();
	 		// } else if(cnbBP == bestnbBP){
	 		// 	if( totalCost < bestCostSMTI){
	 		// 		Rail.copy(csp_.getVariables(),bestConf as Valuation(sz));
	 		// 		bestCostSMTI = totalCost;
	 		// 		bestnbSG = csp_.getnbSingles();
	 		// 	}
	 		// }
	 		
	 		
	 		
	 		
			// --- Interaction with other solvers -----
	 		Runtime.probe();		// Give a chance to the other activities
	 		if (kill)	{
	 		    Logger.debug(()=>" killed!");
	 		    break;		// Check if other place or activity have finished
	 		}
	 	
	 		
	 		if (solver.intraTI() != 0n) 
	 		    if( nbIter % solver.intraTI() == 0n){        //here.id as Int ){
	 		        //Console.OUT.println("In ");
	 		        //Chang//
	 		        solver.communicate( totalCost, csp_.variables); 
	 		        if (random.randomInt(100n) < solverP.probChangeVector){
	 		            val result = solver.getIPVector(csp_, totalCost );
	 		            if (result){
	 		                nbChangeV++;
	 		                nbSwap += size ; //I don't know what happened here with costas reset
	 		                mark.clear();
	 		                totalCost = csp_.costOfSolution(true);
	 		                //Console.OUT.println("Changing vector in "+ here);
	 		            }
	 		            
	 		        }	
	 		        //Console.OUT.println("Print Vectors("+here.id+") :");
	 		        //myComm.printVectors();
	 		        
	 		        
	 		}
	 		
	 		if (forceRestart){
	 			//restart
	 			Logger.debug(()=>"   ASSolverPermut : force Restart");
	 			forceRestart = false;
	 			csp_.initialize(solverP.baseValue); //Set_Init_Configuration Random Permut
	 			nbForceRestart++;
	 			restartVar();
	 			bestCost = totalCost = csp_.costOfSolution(true);
	 			bestOfBest = x10.lang.Int.MAX_VALUE ;
	 			nbInPlateau = 0n;
	 			solver.clear();
	 			continue;
	 		}
	 		// ----- end of interaction with other solvers -----
	 		
	 		//csp.util.Utils.show("new vector ",csp_.variables);
		} // while (totalCost != 0n)
		
		nbIterTot += nbIter;
		nbResetTot += nbReset;	
		nbSwapTot += nbSwap;
		nbSameVarTot += nbSameVar;
		nbLocalMinTot += nbLocalMin; 
		
		//csp_.displaySolution();
		//Logger.info(()=>{"   ASSolverPermut: Finish search with cost: "+totalCost});
		
		if (bestCostSMTI == 0n){
			Logger.info(()=>{"perfect marriage found "});
			//csp_.displaySolution(bestConf as Valuation(sz));
		}
		// else{
		// 	Logger.info(()=>{"Best marriage found - BP= "+bestnbBP+" Singles="+bestnbSG});
		// 	//csp_.displaySolution(bestConf as Valuation(sz));
		// }
		
		
		//creating an error
		//csp_.swapVariables(1n,150n);
		//csp_.swapVariables(1n,2n);
		//csp_.variables(1)=2n;
		
		return bestCostSMTI;
	}
	
	/**
	 * 	selectVarHighCost( csp : SMTIModel ) : Int
	 * 	Select the maximum cost variable of the problem 
	 *  Also computes the number of marked variables.
	 *  @param csp problem model
	 * 	@return the index of the variable with high individual cost
	 */
	public def selectVarHighCost( csp_ : SMTIModel ) : Int{
		
		var i: Int =-1n;
		var x: Int;
		var max: Int = 0n;
	
		listInb = 0n; //Number of elements
		nbVarMarked = 0n; 
		//Console.OUT.println("Aqui");
		while(++i < size)  {
			if (nbSwap < mark(i)) {
				nbVarMarked++;
				continue;
			}
			//Console.OUT.println("Aqui");
			x = csp_.costOnVariable(i);
			//Console.OUT.println("var: "+i+" cost= "+x);
			if (x >= max){
				if (x > max){
					max = x;
					listInb = 0n;
				}
				listI(listInb++) = i; 
			}
		}
		
		x = random.randomInt(listInb);
		//Console.OUT.println("listInb "+listInb+ " x "+x+" listI(x) "+listI(x));
		maxI = listI(x); //This maxI must be local or only returns the value
		nbSameVar += listInb;
		
		// get alternative maxI for communication pourposses
		x = random.randomInt(listInb);
		alMaxI = listI(x); // I hope listInb are > 1 
		
		return maxI;
	}
	
	/**
	 *	selectVarMinConflict( csp : SMTIModel) : Int
	 *	Computes swap and selects the minimum of cost if swap
	 * 	@param csp problem model
	 * 	@return the index of the variable with minimum individual cost if swap
	 */
	public def selectVarMinConflict( csp : SMTIModel) : Int {
		var j: Int;
		var x: Int;
		var flagOut:Boolean = false; 
		var lminJ : Int = -1n;
		
		//loop: 
		do {
			//Console.OUT.println(" --- maxI= "+maxI);
			flagOut = false;
			listJnb = 0n;
	 		newCost = totalCost;
	 		
	 		j = -1n;
	 	
	 		while((j = csp.nextJ(maxI, j, 0n)) as UInt < size as UInt) // false if j < 0
		 	{	
	 			
		 		//Console.OUT.println("swap "+j+"/"+maxI);
		 		x = csp.costIfSwap(totalCost, j, maxI);
		 		//Console.OUT.println("swap "+j+"/"+maxI+"  Cost= "+x);
		 		
		 		if (solverP.probSelectLocMin <= 100n && j == maxI) continue;
		 		
		 		//
		 		if (x < newCost){
		 			listJnb = 1n;
		 			newCost = x;
		 			lminJ = j;
		 			
		 			//For alternative move 
		 			alMinJ = j;
		 			
		 			if (solverP.firstBest) return lminJ;   
		 		} else if (x == newCost){
		 			if (random.randomInt(++listJnb) == 0n)
		 				lminJ = j;
		 			
		 			//Select alternative move
		 			if (random.randomInt(listJnb) == 0n)
		 				alMinJ = j;
		 			
		 		}
		 	}
	 	
		 	if (solverP.probSelectLocMin <= 100n) {
		 		if (newCost >= totalCost && 
		 			(random.randomInt(100n) < solverP.probSelectLocMin 
		 			        ||(listInb <= 1n && listJnb <= 1n))) {
		 			lminJ = maxI;
		 			return lminJ;
		 		}
		
		 		if (listJnb == 0n) {
		 			//Console.OUT.println("listInb= "+listInb);
		 			nbIter++;
		 			x = random.randomInt(listInb);
		 			maxI = listI(x);
		 			flagOut = true;
		 		}
		 	}
		} while(flagOut);
	 	//Console.OUT.println("list_J = "+ listJnb);
		
		//Chang//
		//Here communicate alternative vector with some probability
		/*if (lminJ != alMinJ && solverC.commOption != 0){//if (solverC.commOption != 0){//
			//Console.OUT.println("lminJ = "+ lminJ+ " alMinJ = "+alMinJ);
			var altConf : Rail[Int] = new Rail[Int](0..(size-1));
			Array.copy(csp.variables, altConf);
			// swap var
			val aux = altConf(alMinJ);
			altConf(alMinJ) = altConf(maxI);
			altConf(maxI) = aux;
			
			val res = solverC.communicate( newCost, altConf);
		}*/
		
		return lminJ;
	}
	
	/**
	 * 	doReset( var n : Int, csp : SMTIModel )
	 * 	Performs the reset over the problem model csp
	 *  @param n number of variables to reset
	 * 	@param csp Model to reset
	 */
	public def doReset(n:Int, csp_ : SMTIModel ) {
		
		var cost : Int = -1n;		//reset(n, csp);
				
		cost = csp_.reset( n, totalCost );
		nbSwap += n ; //I don't know what happened here with costas reset
		
		mark.clear();
		nbReset++;
		//Console.OUT.println("Do reset...: "+ nbReset);
		totalCost = (cost < 0n) ? csp_.costOfSolution(true) : cost; //Arg costofsol(1)
	}
	
// 	public def changeVector(csp : SMTIModel){
// 		var ipVector : Int = -1;
// 		
// 		//Main.show("antes= ",csp.variables);
// 		ipVector = solverC.getIPVector(csp, totalCost, commRefs);
// 		//Main.show("despues= ",csp.variables);
// 		
// 		if (ipVector == 1){
// 			nbChangeV++;
// 			nbSwap += size;
// 			//Console.OUT.println("do change vector");
// 			mark.clear();
// 			totalCost = csp.costOfSolution(1); //Arg costofsol(1)
// 		}
// 
// 	}
	
	
	/**
	 * 	Clear function
	 */
	public def clear(){
	}
	
	/**
	 *  Computes maxI and minJ, the 2 variables to swap.
	 *  All possible pairs are tested exhaustively.
	 */
	public def selectVarsToSwap(csp : SMTIModel) {
		var i : Int;
		var j : Int;
		var x : Int;
		
		nbListIJ = 0n;
		newCost = x10.lang.Int.MAX_VALUE ;
		nbVarMarked = 0n;
		
		//Console.OUT.println("TC =>"+totalCost);
 
 		i = -1n;
 		while(++i < size) { // false if i < 0
			if ( nbSwap < mark(i) ) nbVarMarked++;
 			j = i; //j = -1;
 			while(++j < size) { //while((unsigned) (j = Next_J(i, j, i + 1)) < (unsigned) ad.size) // false if j < 0
 				//Console.OUT.println("SWAP "+i+" <-> "+j);
 				x = csp.costIfSwap(totalCost, i, j);
 				//Console.OUT.println("cost = "+x);
 
 				if (x <= newCost) {
 					if (x < newCost) {
 						newCost = x;
 						nbListIJ = 0n;
 						if (solverP.firstBest == true && x < totalCost) {
 							maxI = i;
 							minJ = j;
 							return; 
 						}
 					}
 					listIJ(nbListIJ) = new PairAS();
 					listIJ(nbListIJ).i = i;
 					listIJ(nbListIJ).j = j;
 					nbListIJ = (nbListIJ + 1n) % size;
 				}
 			}
 		}
 
 		nbSameVar += nbListIJ;
 
 		if (newCost >= totalCost) {
 			if (nbListIJ == 0n || 
 					(( solverP.probSelectLocMin <= 100n) 
 					        && random.randomInt(100n) < solverP.probSelectLocMin)) {
 				for(i = 0n; nbSwap < mark(i); i++)
 				{}
 				maxI = minJ = i;
 				return;//goto end;
 			}
 
 			if (!(solverP.probSelectLocMin <= 100n) 
 			        && (x = random.randomInt(nbListIJ + size)) < size) {
 				maxI = minJ = x;
 				return;//goto end;
 			}
 		}
 
 		x = random.randomInt(nbListIJ);
 		maxI = listIJ(x).i;
 		minJ = listIJ(x).j;
 		return;
	}
	
	public def testSelectVarHighCost(csp_: SMTIModel){ 
		var test:Int;
		
		csp_.setParameters(solverP);
		csp_.initialize(solverP.baseValue); //Set_Init_Configuration Random Permut
		
		mark.clear();
		csp_.costOfSolution(true);
		var timeStart :Long = x10.lang.System.nanoTime();
		test = selectVarHighCost(csp_);
		var timeEnd :Long = x10.lang.System.nanoTime(); 
		
		Console.OUT.println("maxI= "+test);
		
		return timeEnd-timeStart;
	}
	
	public def forceRestart(){
		forceRestart = true;
	}
	
	public def restartVar(){
		mark.clear();
		//nbRestart++;			
		//Update Total statistics
		nbIterTot += nbIter;
		nbResetTot += nbReset;        
		nbSwapTot += nbSwap;
		nbSameVarTot += nbSameVar;
		nbLocalMinTot += nbLocalMin; 
		//Restart local var
		nbSwap = 0n;
		nbIter = 0n;
		nbSameVar = 0n;
		nbLocalMin = 0n;
		nbReset = 0n;
		
	}
	
}
public type ASSolverPermut(s:Long)=ASSolverPermut{self.sz==s};