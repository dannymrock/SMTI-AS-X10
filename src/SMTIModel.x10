import x10.array.Array_2;

public class SMTIModel (sz:Long, seed:Long){
	protected val length = sz as Int;
	protected val variables = new Rail[Int]( sz, (i:Long) => i as Int);
	protected val r  = new RandomTools(seed);
	protected val solverParams = new ASSolverParameters();
	/** number of blocking pairs
	 */
	var nbBP:Int;
	
	var nbSingles:Int = 0n;
	
	//val nbBPperMan : Rail[Int];
	val variablesW : Rail[Int];
    val errV : Rail[Int];
    val bp : Rail[Int];
    val bpw : Rail[Int];
	
    val menPref : Rail[Rail[Int]];
    
    val womenPref : Rail[Rail[Int]];
    val revpM : Rail[Rail[Int]];
    
    val revpW : Rail[Rail[Int]];

	public def this (lengthProblem : Long , seed : Long, mPrefs:Rail[Rail[Int]],wPrefs:Rail[Rail[Int]]){
		property(lengthProblem, seed);
		this.initParameters();
		nbBP = 0n;
		//nbBPperMan = new Rail[Int](length,0n);
		variablesW = new Rail[Int](length,0n);
		errV = new Rail[Int](length,0n);
		bp = new Rail[Int](length,0n);
		initParameters();
		val rr=r, l=length as Int;
		menPref = mPrefs;
		womenPref = wPrefs;
		
		bpw = new Rail[Int](length, 0n); 
		
		revpM = new Rail[Rail[Int]](l, (Long) => new Rail[Int](l,0n));
		revpW = new Rail[Rail[Int]](l, (Long) => new Rail[Int](l,0n));
		//Logger.debug(()=>{"Preferences"});
		
		var mw:Int,pos:Int;
		var level:Int=0n;
		for(mw = 0n; mw < length; mw++){
			level=0n;
			for(pos = 0n; pos < length; pos++){
				//revpM(m)(menPref(m)(w)-1) = w;
				var man:Int = womenPref(mw)(pos);
				if (man == 0n) //man deleted
					continue;
				if (man > 0n) 
					level++;
				else // tie, same level as the previous one
					man = Math.abs(man);
				
				man--; //Convertion to an index	
				revpW(mw)(man) = level;
			}
		}
		for(mw = 0n; mw < length; mw++){
			level=0n;
			for(pos = 0n; pos < length; pos++){
				//revpM(m)(menPref(m)(w)-1) = w;
				var woman:Int = menPref(mw)(pos);
				if (woman == 0n) //woman deleted
					continue;
				if (woman > 0n) 
					level++;
				else // tie, same level as the previous one
					woman = Math.abs(woman);
				
				woman--; //Convertion to an index	
				revpM(mw)(woman) = level;
			}
		}
		//printPreferencesTables();
	}

	/** initParameters
	 *  It is necessary to fine tune the parameters for this problem
	 */
	private def initParameters(){
		
		solverParams.probSelectLocMin =  100n; // try ~80%  
		solverParams.freezeLocMin = 1n;
		solverParams.freezeSwap = 0n;
		solverParams.resetLimit =2n; //may be 1 is better for size 30 try()
		solverParams.resetPercent = 0n;
		solverParams.restartLimit = 100000n;
		solverParams.restartMax = 1n;
		solverParams.baseValue = 1n;
		solverParams.exhaustive = false;
		solverParams.firstBest = true;
		
	    solverParams.probChangeVector = 50n;
	}
	
	
	/**
	 * Returns the error if (m,w) forms a BP (0 else)
	 */
	public def blockingPairError(w:Int, pw:Int, m:Int) : Int {
		var err:Int ;
		val lvC = revpW(w-1)(pw-1);
		val lvD = revpW(w-1)(m-1);
		if (lvC == 0n)  		//current assignment of w (pw) is invalid, not present in Wprefs 
			err = length;
		else if (lvD == 0n) 	// m is not present in the preferences of w
			err = 0n;
		else
			err = lvC - lvD;	// computing distance between current and desired assignment
		
		if (err < 0n)		/* (m,w) is a BP (blocking pair) */
			err = 0n;
		
		//Console.OUT.println("bpE in w "+w+", pw "+pw+", m "+m +" = "+err);
		return err;
	}
	
	public def findNextPref(pref:Valuation(sz), index:Int):Int{
		var i:Int = 0n;
		for (i=index+1n; i < length; i++){
			if(pref(i) == 0n)
				continue;
			return pref(i);
		}
		return 0n;
	}
	
	/**
	 * 	Computes the cost of the solution
	 * 	count the number of blocking pairs for the current Match
	 * 	@return cost
	 */
     public def costOfSolution( shouldBeRecorded : Int ) : Int {	
         var w:Int;
         var pm:Int = 0n;
         var pw:Int = 0n; //w_of_m, m_of_w;
         var r:Int = 0n;
       
         
         variablesW.clear();
         for (mi in variables.range()){
        	 if (variables(mi)==0n) continue;
        	 variablesW(variables(mi)-1) = mi as Int + 1n;
         }
         var singles:Int = 0n;
         
         // verify existence of undomminated BP's for each man 
         for (mi in variables.range()){  // mi -> man index (man number = mi + 1)
        	 pm = variables(mi); // pm current match of mi (if pm=0, mi is single)
        	 
        	 var bpM:Int = -1n;		/* NB: -1 to avoid false positive in the test of Cost_If_Swap */
        	 var bpW:Int = -1n;
        	 var e:Int = 0n; 	 	
        	 var bF:Int = 0n;
        	 var singleF:Int = 0n;
        	 var prefPM:Int = -1n; //m's current match level of preference  
        	 
        	 if (pm == 0n){ // verify if m-pm is not a valid match or is single
        		singleF = 1n;
        		prefPM = length; //put some value
        		singles++;
        	 }else if(revpM(mi)(pm-1n)==0n ){
        		 singleF = 1n;
        		 prefPM = length; //put some value
        		 singles++;
        	 } else{ // m has a valid assignment pm
        		 singleF = 0n;
        		 prefPM = revpM(mi)(pm-1n);
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
        		 
        		 pw = variablesW(w-1); //pw current match of the current W
        		 
        		 if (pw == 0n){		// w is single
        			 //blocking pair (mi+1 and w)
        			 e = length;
        			 bpW = w;
        		 }else{ 
        			 // Verify if w prefers m to pw
        			 e = blockingPairError(w, pw, mi as Int + 1n);
        		 }      		 	 
        		 
        		 if (e > 0n){
        			 bpM = pw; // if pw is 0 the woman bp is single
        			 r++;              /* count the errors (number of BP) */
        			 break; 			//only consider undominated BP
        		 }
        	 }
        	 if (shouldBeRecorded != 0n){
        		 errV(mi) = e;
        		 bp(mi) = bpM;
        		 bpw(mi) = bpW;
        		 nbBP = r;
        		 nbSingles = singles;
        		 //val eval=e;
        		 //val bpval=bpM;
        		 //Logger.info(()=>"error="+eval+" bp="+bpval);
        	 }
         }
         //Console.OUT.println("nb BP= "+r+" nb Singles="+nbSingles);
         return r+singles;	
     }
	
	/** 
	 * 	costOnVariable( i : Int ) : Int
	 * 	This function computes the cost of individual variable i
	 * 	@param i This is the variable that we want to know the cost
	 *  @return Int value with the cost of this variable
	 */
	public def costOnVariable( i : Int ) : Int {		
		return errV(i);
	}
	
	
	// public def swap2(i:Int, j:Int){
	// 	var x:Int = variables(i);
	// 	variables(i) = variables(j);
	// 	variables(j) = x;
	// }
	
	public def nextJ(i:Int, j:Int, exhaustive:Int) : Int {
		if (j >= 0n) return -1n;
		else{
			val bpM = bp(i);
			if (bpM == 0n){
				// w is single is not assigned to any m, so how to assign it to this variable?
				return -1n;
			}
			return bpM;
		}
		//return (j < 0n) ? bp(i) : -1n;
	}
	
	
	/**
	 *  executedSwap( i1 : Int, i2 : Int)
	 *  This function updates the values of the object data structures for the problem due to the 
	 *  completion of a swap between two variables
	 *  @param i1 First variable already swapped
	 *  @param i2 Second variable already swapped
	 */
	public def executedSwap ( i1 : Int, i2 : Int) {
		this.costOfSolution(1n);
	}
	
	public def displaySolution(){
		Console.OUT.println("\nMatching  m->w:");
		for (i in variables.range()){
			Console.OUT.print(i+1+"->"+variables(i)+"   ");
		}
		Console.OUT.println(" ");
	}
	
	
	public def displaySolution(match:Valuation(sz)){
		
		Console.OUT.println("\nMatching  m->w:");
		
		for (i in match.range()){
			if(revpM(i)(match(i)-1n)==0n){
				Console.OUT.print(i+1+"-> 0 ");
			}else
				Console.OUT.print(i+1+"->"+variables(i)+"   ");
		}
		Console.OUT.println(" ");
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
		var r : Int = costOfSolution(0n);
		swapVariables(i1, i2);
		return r;
	}
	
	/**
	 *  CHECK_SOLUTION
	 * 
	 *  Checks if the solution is valid.
	 */

	public  def verified(match:Valuation(sz)):Boolean {
		var w:Int;
		var pm:Int = 0n;
		var pw:Int = 0n; //w_of_m, m_of_w;
		var r:Int = 0n;
		
		
		variablesW.clear();
		for (mi in match.range()){
			if (match(mi)==0n)	continue;
			variablesW(match(mi)-1) = mi as Int + 1n;
		}
		
		//Console.OUT.println("Solution with "+nbSingles+" singles");
		var singles:Int = 0n;
		// verify existence of undomminated BP's for each man 
		for (mi in match.range()){  // mi -> man index (man number = mi + 1)
			pm = match(mi); // pm current match of mi (if pm=0, mi is single)
			
			var e:Int = 0n; 	 	
			var bF:Int = 0n;
			var prefPM:Int = -1n; //m's current match level of preference  
			
			if (pm == 0n){ // verify if m-pm is not a valid match or is single
				prefPM = length; //put some value
				singles++;
				Console.OUT.println("m= "+ mi+1n +" is single");
			}else if( revpM(mi)(pm-1n)==0n ){
				prefPM = length; //put some value
				singles++;
				Console.OUT.println("m= "+ (mi+1n) +" w= "+pm+"is not a valid match (single)");
			} else{ // m has a valid assignment pm
				prefPM = revpM(mi)(pm-1n);
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
				
				pw = variablesW(w-1); //pw current match of the current W
				
				if (pw == 0n){		// w is single
					//blocking pair (mi+1 and w)
					e = length;
					Console.OUT.println("blocking pair m= "+(mi+1n)+" w= "+w);
				}else{ 
					// Verify if w prefers m to pw
					e = blockingPairError(w, pw, mi as Int + 1n);
				}      		 	 
				
				if (e > 0n){
					r++;
					Console.OUT.println("blocking pair m= "+(mi+1n)+" w= "+w);
					/* count the errors (number of BP) */
					break; 			//only consider undominated BP
				}
			}
		}
		return (r + nbSingles == 0n) ? true : false;
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
		var mPref:Rail[Rail[Int]] = SMTIModel.createPrefs(l as Long, r.randomLong());
		var wPref:Rail[Rail[Int]] = SMTIModel.createPrefs(l as Long, r.randomLong());
		val wDel = new Rail[Int](l as Long, 0n);
		
		// delete some entries with some probability p1
		var del:Int = 0n;
		var rep:Int = 1n;
		
		 while(rep == 1n){
			Console.OUT.println("Creating SMTI");
			rep = 0n;
			wDel.clear();
			mPref = SMTIModel.createPrefs(l as Long, r.randomLong());
			wPref = SMTIModel.createPrefs(l as Long, r.randomLong());
			loop:for (m in 0..(l-1)){
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
									Logger.info(()=>{"Error: all W preferences deleted"});
									//TODO: Restart random problem generator 
									rep=1n;
									
									break loop;
								}
							}
					}
				}
				if (del == l){
					Logger.info(()=>{"Error: all M preferences deleted"});
					//TODO: Restart random problem generator 
					rep=1n;
					
					break;
				}
				del = 0n;
			}
			
		}
		
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
		noFirst=0n;
		for (w in 0..(l-1)){
			
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
			noFirst=0n;
		}
		
		Rail.copy(mPref, mP);
		Rail.copy(wPref, wP);	
	}
	
	
	static def createSMTI(l:Long,seed:Long):Rail[Rail[Int]]{
		val r = new RandomTools(seed);
		val prefs = new Rail[Rail[Int]](l, (Long) => new Rail[Int](r.randomPermut(l, 1n)));
		
		// delete components in prefs
		
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
		
		// //variables.clear();
		// var w:Int=0n;
		// for (manI in variables.range()){
		// 	w = 0n;
		// 	for(prefw in menPref(manI))
		// 		if(prefw==variables(manI))
		// 			w=prefw;
		// 	
		// 	if (w==0n) 
		// 		nbSingles++;
		// 	variables(manI)=w;
		// }
		//Utils.show("varM",variables);
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
	
	
	public def findMax(pvalue:Int):Int {
		var maxi:Int = -1n;		/* return -1 if none found */
		var maxErr:Int = 0n;
		var maxNb:Int = 0n;
		var i:Int;

		for(i = 0n; i < length; i++)
		{
			var e:Int = errV(i);
			// val ival = i;
			// val eval = e;
			// Logger.debug(()=>"error "+ival+":"+eval);
			
			if (e == 0n || i == pvalue || bp(i) == pvalue) 
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
		val mi = maxi;
		Logger.debug(()=>"maxi:"+mi);
		return maxi;
	}
	
	public def reset ( var n : Int, totalCost : Int ) : Int {	
 		var i:Int, j:Int;
		var maxi:Int = findMax(-1n);	/* find max */
		var bpMaxi:Int= (maxi >= 0n ?  bp(maxi): -1n);
		var otheri:Int;
		var nbErr:Int = totalCost; /* the cost is the number of errors (ie. BP) */

		if (bpMaxi > 0n){
 			swapVariables(maxi, bpMaxi -1n);
 		}else{
 			//variables(maxi) = 0n; //man maxi is now single
 		}

 		/* find second max if possible (random is to avoid to be trapped in the same local min) */
		if (nbErr > 1n && r.randomDouble() < 0.98 &&  (otheri = findMax(bpMaxi)) >= 0n){
 			if (bp(otheri) > 0n){
 				swapVariables(otheri, (bp(otheri)-1n));
 			}
 		}
 		else {
			i = r.randomInt(length);
			j = r.randomInt(length);
			swapVariables(i, j);
		}
		return -1n;
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
}

public type SMTIModel(s:Long)=SMTIModel{self.sz==s};
