import x10.array.Array_2;

public class SMTIModel (sz:Long, seed:Long){
	/** length:size of the problem  **/
	protected val length = sz as Int;
	/** variables:array with current configuration (index man - value woman) **/
	protected val variables = new Rail[Int]( sz, (i:Long) => i as Int);
	/** r:random number generator **/
	protected val r  = new RandomTools(seed);
	/** solverParams:parameters of the problem **/
	protected val solverParams = new ASSolverParameters();
	
	/** menPref:Matrix with the men preferences **/
	val menPref : Rail[Rail[Int]];
	/** womenPref:Matrix with the women preferences **/
	val womenPref : Rail[Rail[Int]];
	/** revpM:Matrix with the reverse men preferences **/
	val revpM : Rail[Rail[Int]];
	/** revpW:Matrix with the men preferences **/
	val revpW : Rail[Rail[Int]];
	
	/** nbBP:Current number of blocking pairs in the configuration **/
	var nbBP:Int = 0n;
	/** nbSingles:Current number of singles in the configuration **/
	var nbSingles:Int = 0n;
	/** variablesW:array with current inverted configuration (index woman - value man) **/
	val variablesW = new Rail[Int](length,0n);
	/** errV:array with current individual cost (index man - value cost)**/
    val errV = new Rail[Int](length,-1n);
    /** bpi:Blocking par indexes (index man - value bp index to swap)**/
    val bpi = new Rail[Int](length,-1n);
    /** singV: array that contains all the indexes of the single men **/
    val singV = new Rail[Int](length,0n);
    /** weight: weight to compute the total cost (cost = bp*weight + singles)**/
    var weight:Int = length;
    
    
	public def this (lengthProblem : Long , seed : Long, mPrefs:Rail[Rail[Int]], wPrefs:Rail[Rail[Int]]){
		property(lengthProblem, seed);
		this.initParameters();
		
		val rr=r, l=length as Int;
		menPref = mPrefs;
		womenPref = wPrefs;
		revpM = new Rail[Rail[Int]](l, (Long) => new Rail[Int](l,0n));
		revpW = new Rail[Rail[Int]](l, (Long) => new Rail[Int](l,0n));
		//Logger.debug(()=>{"Preferences"});
		
		// Creating Reverse Matrixes
		var mw:Int,pos:Int;
		var level:Int=0n;
		for(mw = 0n; mw < length; mw++){
			level = 0n;
			for(pos = 0n; pos < length; pos++){
				var man:Int = womenPref(mw)(pos);
				if (man == 0n) //man deleted
					continue;
				if (man > 0n) 
					level++;
				else // if current value is negative = tie, same level as the previous one
					man = Math.abs(man);
				man--; //Convertion to an index	
				revpW(mw)(man) = level;
			}
		}
		
		for(mw = 0n; mw < length; mw++){
			level=0n;
			for(pos = 0n; pos < length; pos++){
				var woman:Int = menPref(mw)(pos);
				if (woman == 0n) //woman deleted
					continue;
				if (woman > 0n) 
					level++;
				else // if current value is negative = tie, same level as the previous one
					woman = Math.abs(woman);
				
				woman--; //Converting to an index	
				revpM(mw)(woman) = level;
			}
		}
		/// printPreferencesTables();
	}

	/** initParameters
	 *  It is necessary to fine tune the parameters for this problem
	 */
	private def initParameters(){
		
		solverParams.probSelectLocMin =  100n; // try ~80%  
		solverParams.freezeLocMin = 1n;
		solverParams.freezeSwap = 0n;
		solverParams.resetLimit =1n; //may be 1 is better for size 30 try()
		solverParams.resetPercent = 0n;
		solverParams.restartLimit = 1000000000n; //30n*length; //30//1000000000n;
		solverParams.restartMax = 0n;
		solverParams.baseValue = 1n;
		solverParams.exhaustive = false;
		solverParams.firstBest = true;
		
	    solverParams.probChangeVector = 10n;
	    
	    //weight = System.getenv().get("V")!=null?length:1n;
	    //Console.OUT.println("weight= "+weight);
	    
	}
	
	
	/**
	 * Determine if mi and wi is a BP return the error (or 0 if it's not a BP)  
	 * @param wi index of the woman 
	 * @param pwi index of current match of the woman
	 * @param mi index of the man
	 * @return the error if (mi,wi) is a BP (0 else)
	 */
	public def blockingPairError(wi:Int, pwi:Int, mi:Int) : Int {
		var err:Int ;
		val lvC = revpW(wi)(pwi);
		val lvD = revpW(wi)(mi);
		if (lvC == 0n){
            //current assignment of w (pw) is invalid, not present in Wprefs 
			//err = length;
			err = 1n;
		}else if (lvD == 0n){
            // m is not present in the preferences of w
			err = 0n;
		}else{
			err = lvC - lvD;	// computing distance between current and desired assignment
			err = err > 0n ? err + 1n : err;
		}
		
		if (err < 0n)		/* (m,w) is a BP (blocking pair) */
			err = 0n;
		
		//Console.OUT.println("bpE in w "+w+", pw "+pw+", m "+m +" = "+err);
		return err;
	}

	
	/**
	 * 	Computes the cost of the solution
	 * 	count the number of blocking pairs and singles for the current Match
	 *  if shouldBeRecorded is true the global variables are updated
 	 *  @param shouldBeRecorded if true saves the computatuon in global variables
	 * 	@return cost
	 */
     public def costOfSolution( shouldBeRecorded : Boolean ) : Int {	
         var w:Int;
         var pmi:Int = 0n; // index of current match of mi 
         var pwi:Int = 0n; // index of current match of wi
         var bpnumber:Int = 0n;
         var singles:Int = 0n;
         var singleF:Boolean = false;
         
         /// if(shouldBeRecorded){
        	///  Console.OUT.println("cost of Sol");
        	///  Utils.show("conf:",variables);
         /// }
         
         variablesW.clear();
         for (mi in variables.range()){ // mi -> man index (man number = mi + 1)
        	 if (variables(mi)==0n) continue;
        	 variablesW(variables(mi)-1) = mi as Int + 1n;
         }
         
         // verify existence of undomminated BP's for each man 
         for (mi in variables.range()){  // mi -> man index (man number = mi + 1)
        	pmi = variables(mi) - 1n; // pm current match of mi (if pm = is not valid, mi is single)
        	
        	var bpMi:Int = -1n;		/* NB: -1 to avoid false positive in the test of costIfSwap */
        	var e:Int = 0n; 	 	
        	var prefPM:Int = -1n; // m's current match level of preference  
        	 
        	if(revpM(mi)(pmi) == 0n ){ //verify if m-pm is single (have a not valid match)
        		prefPM = length;
        		singleF=true;
        		singles++;
        	}else{ // m has a valid assignment pm
        		prefPM = revpM(mi)(pmi);
        	}
        	 
        	var prefW:Int = 0n;
        	for(li in menPref(mi).range()){ //li level of preference index
        		w = menPref(mi)(li);
        		if (w == 0n) continue;		// entry deleted
        		else if(w > 0n)			// new level of preference
        			prefW++;
        		else						// if w < 0 -> same level of preference (tie) 
        			w = -w;
        		 
        		if (prefW >= prefPM) break; //stop if cuerrent level of pref is bigger or equal 
        		// than the level of pref of pm (current match) "stop condition"
        		 
        		pwi = variablesW(w-1)-1n; //pwi index of the current match of the woman w
      			
        		// Verify if w prefers m to pw
        		e = blockingPairError(w-1n, pwi, mi as int); 
        		if (e > 0n){	
           			bpMi = pwi; 
           			bpnumber++;     /* count the errors (number of BP) */
        			break; 			//only consider undominated BP
        		}
        	}
        	if (shouldBeRecorded){
        		// val bpm = bpM; val vale = e;
        		// if (r == 1n) Logger.info(()=>{"bp mi="+mi+" bpM "+bpm+" err="+vale});
        		if(singleF){
        			singV(singles-1) = mi as Int;
        			singleF = false;
        		}
        		errV(mi) = e;
        		bpi(mi) = bpMi;
        		///Console.OUT.println("mi= "+mi+" e= "+e+" bpMi= "+bpMi);
        	}
        	
        }
        if (shouldBeRecorded) {
        	nbBP = bpnumber;
        	nbSingles = singles;
        	///Console.OUT.println("totalCost= "+(bpnumber*weight+singles));
        }
        return bpnumber*weight+singles;	
    }
	
	/** 
	 * 	costOnVariable( i : Int ) : Int
	 * 	This function computes the cost of individual variable i
     *  For SMP returns the distance in the blocking pair
	 * 	@param i This is the variable that we want to know the cost
	 *  @return Int value with the cost of this variable
	 */
	public def costOnVariable( i : Int ) : Int {		
		return errV(i);
	}
	
	
	public def nextJ(i:Int, j:Int, exhaustive:Int) : Int {
		///Console.OUT.println("i= "+i+"  j= "+j+"  bp-i= "+bpi(i));
		return (j < 0n) ? bpi(i) : -1n;
	}
	
	
	/**
	 *  executedSwap( i1 : Int, i2 : Int)
	 *  This function updates the values of the object data structures for the problem due to the 
	 *  completion of a swap between two variables
	 *  @param i1 First variable already swapped
	 *  @param i2 Second variable already swapped
	 */
	public def executedSwap ( i1 : Int, i2 : Int) {
		this.costOfSolution(true);
	}

	/**
	 *  costIfSwap(current_cost : Int, i1 : Int, i2 : Int) : Int
	 *  This function computes the cost of the problem if there is a swap between variable
	 *  i1 and i2.
	 * 	@param current_cost The current cost of the problem
	 *  @param i1 first variable to swap
	 *  @param i2 second variable to swap
	 *  @return cost of the problem if the swap is done
	 */
	public def costIfSwap(current_cost:Int,var i1:Int, var i2:Int) : Int {
		swapVariables(i1, i2);
		var r : Int = costOfSolution(false);
		swapVariables(i1, i2);
		return r;
	}
	
	
	public def findMax(pvalue1:Int, pvalue2:Int):Int { //pvalue is an man index
		var maxi:Int = -1n;		/* return -1 if none found */
		var maxErr:Int = 0n;
		var maxNb:Int = 0n;
		var i:Int;

		for(i = 0n; i < length; i++){
			var e:Int = errV(i);
			if (e == 0n || i == pvalue1 || i == pvalue2 ||  bpi(i) == pvalue1 || bpi(i) == pvalue2) 
				continue;
			if (e > maxErr){ 	
				maxi = i;
				maxErr = e;
				maxNb = 1n;
			} 
			else if (e == maxErr && r.randomInt(++maxNb) == 0n){
				maxi = i;
			}
		}
		// val mi = maxi;
		// Logger.debug(()=>"maxi:"+mi);
		return maxi;
	}
	
	public def reset ( var n : Int, totalCost : Int ) : Int {			
		 
// 		var maxi:Int = findMax(-1n,-1n); /* find max */
// 		var bpMaxi:Int= (maxi >= 0n ? bpi(maxi): -1n);
// 		var otheri:Int;
// 		
// 
// 		//Console.OUT.println("Reset maxi= "+maxi+" bpMaxi = "+ bpMaxi);
// 		if (bpMaxi > 0n){
// 			swapVariables(maxi, bpMaxi);
// 		}else{
// 			//Console.OUT.println("Reset fail maxi= "+maxi+" bpMaxi = "+ bpMaxi);
// 			// I dont know if it is possible to do something here
// 		}
// 
// 		/* find second max if possible (random is to avoid to be trapped in the same local min) */
// 		// if (nbBP > 1n && r.randomDouble() < 0.98 && (otheri = findMax(maxi,bpMaxi)) >= 0n){
// 		// 	if (bpi(otheri) >= 0n){
// 		// 		swapVariables(otheri, bpi(otheri));
// 		// 	}
// 		// }
// 		// else {
// 			val i = r.randomInt(length);
// 			val j = r.randomInt(length);
// 			swapVariables(i, j);
// 		// }
// 		return -1n;
		
		// 1st BLOCKING PAIRS
		if (nbBP > 0n){
			var maxi:Int = findMax(-1n, -1n);	/* find max */
			var bpiMaxi:Int = bpi(maxi);
			var otheri:Int; 
			///Console.OUT.println("Reset maxi= "+maxi+" bpiMaxi = "+ bpiMaxi);
			swapVariables(maxi, bpiMaxi);
		 	if (nbBP > 1n && r.randomDouble() < 0.98 &&  (otheri = findMax(maxi,bpiMaxi)) >= 0n){
		 		///Console.OUT.println("Reset otheri= "+otheri+" bpi(otheri) = "+ bpi(otheri));
				swapVariables(otheri, bpi(otheri));
			}else {
				val i = r.randomInt(length);
				val j = r.randomInt(length);
				///Console.OUT.println("Reset no 2nd BP i= "+i+" j = "+ j);
				swapVariables(i, j);
			}
		}else {
			if(nbSingles > 0n){
				val i = r.randomInt(nbSingles);
				val j = r.randomInt(length);
				///Console.OUT.println("Reset single singV(0)= "+singV(0)+" random j = "+ j+"  nbSingles="+nbSingles);
				swapVariables(singV(i), j);
			}else{
				val i = r.randomInt(length);
				val j = r.randomInt(length);
				///Console.OUT.println("Reset no BP i= "+i+" j = "+ j);
				swapVariables(i, j);
			}
		}
		
		//2nd SINGLES
		// if(nbSingles >= 2n){
		// 	val i = r.randomInt(nbSingles);
		// 	val j = r.randomInt(nbSingles);
		// 	///Console.OUT.println("Reset out single singV(i)= "+singV(i)+"singV(j) = "+ singV(j)+"  nbSingles="+nbSingles);
		// 	swapVariables(singV(i), singV(i));
		//  } else if(nbSingles < 2n){
		// 	val j = r.randomInt(length);
		// 	///Console.OUT.println("Reset single singV(0)= "+singV(0)+" random j = "+ j+"  nbSingles="+nbSingles);
		// 	swapVariables(singV(0), j);
		// }
		
		// if(nbSingles > 0n){
		// 	val i = r.randomInt(nbSingles);
		// 	val j = r.randomInt(length);
		// 	///Console.OUT.println("Reset single singV(0)= "+singV(0)+" random j = "+ j+"  nbSingles="+nbSingles);
		// 	swapVariables(singV(i), j);
		// }

		// // 3rd RANDOMNESS
		// if(r.randomDouble() < 0.05){ // size 100 -> 1   size 1000 -> 0.1
		// 	val i = r.randomInt(length);
		// 	val j = r.randomInt(length);
		// 	Console.OUT.println("Reset out i= "+i+" j = "+ j);
		// 	swapVariables(i, j);
		// }
		
		// if(r.randomDouble() < (100.0 / length)){ // size 100 -> 1   size 1000 -> 0.1
		// 	val i = r.randomInt(length);
		// 	val j = r.randomInt(length);
		// 	//Console.OUT.println("Reset out i= "+i+" j = "+ j);
		// 	swapVariables(i, j);
		// }
		
		return -1n;
	}
	
	
	/**
	 *  CHECK_SOLUTION
	 * 
	 *  Checks if the solution is valid.
	 */

	public  def verified(match:Valuation(sz)):Boolean {
		var w:Int;
		var pmi:Int = 0n;
		var pwi:Int = 0n; //w_of_m, m_of_w;
		var r:Int = 0n;
		var singles:Int = 0n;
		
		variablesW.clear();
		for (mi in match.range()){
			if (match(mi)==0n)	continue;
			variablesW(match(mi)-1) = mi as Int + 1n;
		}
		
		// verify existence of undomminated BP's for each man 
		for (mi in match.range()){  // mi -> man index (man number = mi + 1)
			pmi = match(mi)-1n; // pm current match of mi (if pm=0, mi is single)
			var e:Int = 0n; 	 	
			var bF:Int = 0n;
			var prefPM:Int = -1n; //m's current match level of preference  
				
			if( revpM(mi)(pmi)==0n ){
				prefPM = length; //put some value
				singles++;
				Console.OUT.println("m="+ (mi+1n) +" w="+(pmi+1n)+" is not a valid match (single)");
			} else{ // m has a valid assignment pm
				prefPM = revpM(mi)(pmi);
			}
			
			var prefW:Int = 0n;
			for(li in menPref(mi).range()){ //li level of preference index
				
				w = menPref(mi)(li);
				if (w == 0n) continue;	// entry deleted
				else if(w > 0n)			// new level of preference
					prefW++;
				else						// if w < 0 -> same level of preference (tie) 
					w = Math.abs(w);
				if (prefW >= prefPM) break; //stop if cuerrent level of pref is bigger or equal 
				// than the level of pref of pm (current match) "stop condition"
				pwi = variablesW(w-1)-1n; //pw current match of the current
				// 	// Verify if w prefers m to pw
				e = blockingPairError(w-1n, pwi, mi as Int);
				
				if (e > 0n){
					r++;
					Console.OUT.println("blocking pair m="+(mi+1n)+" w="+w+" pw= "+(pwi+1n) +" with error= "+e);
					/* count the errors (number of BP) */
					break; 			//only consider undominated BP
				}
			}
		}
		return (r + nbSingles == 0n);
	}
	
	static def createPrefs(l:Long,seed:Long):Rail[Rail[Int]]{
		val r = new RandomTools(seed);
		val prefs = new Rail[Rail[Int]](l, (Long) => new Rail[Int](r.randomPermut(l, 1n)));
		return prefs;
	}
	/**
	 *  Create SMTI problem 
	 */
	static def createPrefs(p1:Int, p2:Int, l:Int, seed:Long,  mP:Rail[Rail[Int]], wP:Rail[Rail[Int]]){
		val r = new RandomTools(seed);
		var mPref:Rail[Rail[Int]] ;
		var wPref:Rail[Rail[Int]] ;
		val wDel = new Rail[Int](l as Long, 0n);
		
		// delete some entries with some probability p1
		var del:Int;
		var rep:Int = 1n;
		
		 do{
			//Console.OUT.println("Creating SMTI");
			rep = 0n;
			wDel.clear();
			mPref = SMTIModel.createPrefs(l as Long, r.randomLong());
			wPref = SMTIModel.createPrefs(l as Long, r.randomLong());
			loop:for (m in 0..(l-1)){
				del = 0n;
				for (p in 0..(l-1)){
					if (r.randomInt(100n) <= p1){
						del++;
						val dw = mPref(m)(p) - 1n;
						// deleting in mPref
						mPref(m)(p) = 0n;
						for(k in wPref(dw).range())
							if (wPref(dw)(k) - 1n == m as Int){
								// deleting in corresponding wPref
								wPref(dw)(k) = 0n;
								wDel(dw)++;
								if(wDel(dw)==l){
									Logger.debug(()=>{"Error: all W preferences deleted"});
									//TODO: Restart random problem generator 
									rep=1n;
									
									break loop;
								}
							}
					}
				}
				if (del == l){
					Logger.debug(()=>{"Error: all M preferences deleted"});
					//TODO: Restart random problem generator 
					rep=1n;
					
					break;
				}
				
			} 
			
		}while(rep == 1n);
		
		// create some ties in entries with some probabilities p2
		var noFirst:Int;
		for (m in 0..(l-1)){
			noFirst=0n;
			for (p in 0..(l-1)){
				if(mPref(m)(p)==0n)
					continue;
				if(noFirst==0n){
					noFirst=1n;
					continue;
				}
				if (r.randomInt(100n) <= p2){
					//val dw = mPref(m)(p);
					mPref(m)(p) *= -1n; 	
				}
			}
		}
		
		for (w in 0..(l-1)){
			noFirst=0n;
			for (p in 0..(l-1)){
				if(mPref(w)(p)==0n) continue;
				if(noFirst==0n){
					noFirst=1n;
					continue;
				}
				if (r.randomInt(100n) <= p2){
					//val dw = mPref(m)(p);
					wPref(w)(p) *= -1n; 	
				}
			}
		}
		
		Rail.copy(mPref, mP);
		Rail.copy(wPref, wP);	
	}
	
	
	static def createSMTI(l:Long,seed:Long):Rail[Rail[Int]]{
		val r = new RandomTools(seed);
		val prefs = new Rail[Rail[Int]](l, (Long) => new Rail[Int](r.randomPermut(l, 1n)));
		return prefs;
	}
	
	/**
	 * 	Set the parameter in the solver
	 * 	@param solverParameters Solver parameter from the model
	 */
	public def setParameters(solverParameters : ASSolverParameters):void{
		solverParameters.setValues(solverParams);
	}
	
	public def initialize( baseValue : Int ) {
		for(k in variables.range()){
			variables(k) = baseValue + k as Int;
		}
		//Main.show("before ini",variables);
		for( var i:Int = length - 1n ; i >	0n ; i-- ) {
			val j = r.randomInt( i + 1n );
			swapVariables(i,j);
		}
	}
	
	public def swapVariables(i:Int, j:Int):void{
		//Console.OUT.println("swap func i: "+i+" j: "+j);
		val x = variables(i);
		variables(i) = variables(j); 
		variables(j) = x;
	}
	
	public def setVariables(array : Rail[Int]{self.size==variables.size}){
		Rail.copy(array,this.variables);
	}
	
	public def getVariables():Valuation(sz){
		return variables;
	}
	
	public def getnbSingles():Int{
		return nbSingles;
	}
	
	public def getnbBP():Int{
		return nbBP;
	}
	
	public def displaySolution(){
		Console.OUT.println("\nMatching  m->w:");
		for (i in variables.range()){
			Console.OUT.printf("%4d->%-4d",(i+1),variables(i));
		}
		Console.OUT.print("\n");
	}

	public def displaySolution(match:Valuation(sz)){		
		Console.OUT.println("\nMatching  m->w:");
		for (i in match.range()){
			if(revpM(i)(match(i)-1n)==0n){
				Console.OUT.printf("%4d->%-4d",(i+1),0n);
			}else
				Console.OUT.printf("%4d->%-4d",(i+1),variables(i));
		}
		Console.OUT.print("\n");
	}
	
	public def displaySolution2 (match:Valuation(sz)){	
		Console.OUT.print("#Sol in  "+here);
		for (i in match.range()){
			if(revpM(i)(match(i)-1n)==0n){
				Console.OUT.print(" - ");
			}else
				Console.OUT.print(" "+variables(i)+" ");
		}
		//Console.OUT.print("\n");
	}
	
	private def printPreferencesTables(){
		Console.OUT.println("\nMen Preferences");
		var i:Int = 0n;
		for (i=0n; i<length; i++){
			Console.OUT.print(i+1+": ");
			for(j in menPref(i))
				Console.OUT.print(j+" ");
			Console.OUT.println("");
		}
		Console.OUT.println("Women Preferences");
		for (i=0n; i<length; i++){
			Console.OUT.print(i+1+": ");
			for(j in womenPref(i))
				Console.OUT.print(j+" ");
			Console.OUT.println("");
		}
		
		Console.OUT.println("Men rev Preferences");
		for (i=0n; i<length; i++){
			Console.OUT.print(i+1+": ");
			for(j in revpM(i))
				Console.OUT.print(j+" ");
			Console.OUT.println("");
		}
		
		Console.OUT.println("Women rev Preferences");
		for (i=0n; i<length; i++){
			Console.OUT.print(i+1+": ");
			for(j in revpW(i))
				Console.OUT.print(j+" ");
			Console.OUT.println("");
		}
	}
}

public type SMTIModel(s:Long)=SMTIModel{self.sz==s};
