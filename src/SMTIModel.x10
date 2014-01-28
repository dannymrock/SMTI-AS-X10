import x10.array.Array_2;

public class SMTIModel (sz:Long, seed:Long){
	protected val length = sz as Int;
	protected val variables = new Rail[Int]( sz, (i:Long) => i as Int);
	protected val r  = new RandomTools(seed);
	protected val solverParams = new ASSolverParameters();
	/** number of blocking pairs
	 */
	var nbBlockingPairs:Int;
	
	var nbSingles:Int = 0n;
	
	val nbBPperMan : Rail[Int];
	val variablesW : Rail[Int];
    val errV : Rail[Int];
    val bp : Rail[Int];
	
    val menPref : Rail[Rail[Int]];
    
    val womenPref : Rail[Rail[Int]];
    val revpM : Rail[Rail[Int]];
    
    val revpW : Rail[Rail[Int]];

	public def this (lengthProblem : Long , seed : Long, mPrefs:Rail[Rail[Int]],wPrefs:Rail[Rail[Int]]){
		property(lengthProblem, seed);
		this.initParameters();
		nbBlockingPairs = 0n;
		nbBPperMan = new Rail[Int](length,0n);
		variablesW = new Rail[Int](length,0n);
		errV = new Rail[Int](length,0n);
		bp = new Rail[Int](length,0n);
		initParameters();
		val rr=r, l=length as Int;
		menPref = mPrefs;
		womenPref = wPrefs;
		
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
		printPreferencesTables();
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
		solverParams.restartLimit = 1000000000n;
		solverParams.restartMax = 10n;
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
		if (pw == 0n){ //w is single
			err = 1n;    //min distance
			//err = length;//max distance
		}else{
			val lvC = revpW(w-1)(pw-1);
			val lvD = revpW(w-1)(m-1);
			if (lvC == 0n) 
				err=length;
			else if (lvD == 0n) 
				err=0n;
			else
				err = revpW(w-1)(pw-1) - revpW(w-1)(m-1);
		}
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
         var m1:Int, w:Int;
         var j:Int;
         var pm:Int = 0n;
         var pw:Int = 0n; //w_of_m, m_of_w;
         var r:Int = 0n;
       
         //errV.clear();
         variablesW.clear();
         for (m in variables.range()){
        	 //Console.OUT.println("m="+m);
        	 if (variables(m)==0n) continue;
        		 
             variablesW(variables(m)-1) = m as Int + 1n;
         }
         //Utils.show("varW",variablesW);
         
         // verify BP for each man 
         for (mi in variables.range()){  // mi -> man index (man number = mi +1)
        	 //Console.OUT.println("men="+m);
        	 pm = variables(mi);
        	 //if pm=0, mi is single
        	 
        	 var bpM:Int = -1n;		/* NB: -1 to avoid false positive in the test of Cost_If_Swap */
        	 var e:Int = 0n; 	 	
        	 var bflag:Int = 0n;
        	 
        	 
        	 // verify if m-pm is not a valid match
        	 if (revpM(mi)(pm-1n)==0n){
        		e = 1n;
        		errV(mi) = e;
        		continue;
        	 }
        	 
        	 for(li in menPref(mi).range()){ //li level index
        		 w = Math.abs(menPref(mi)(li));
        		 if (w != 0n){
        			 if(w == pm) 
        				 bflag=1n; // w is the current match check if there is other 
        			 else{
        				 pw = variablesW(w-1);
        				 e = blockingPairError(w, pw, mi as Int + 1n);
        			 }
        			 //Check if there is other pref entries with the same level of preference
        			 var nw:Int = findNextPref(menPref(mi) as Valuation(sz),li as Int); 
        			 if (nw >= 0n && bflag == 1n) 	//If nw is 0 means no more entries in pref list
        				 break;     			 	//If nw is positive means the next one have other level of pref.
        		 }
        		 	 
        		 
        		 if (e > 0n){
        			 bpM = pw; // if pw is 0 the woman bp is single
        			 r++;              /* count the errors (number of BP) */
        			 break; 			//only consider undominated BP
        		 }
        		 
        		 if(li == length-1 && bflag==0n){
        			 //last element and pm was not found in preference list
        			 r++;
        		 }
        		 
        	 }
        	 if (shouldBeRecorded != 0n){
        		 errV(mi) = e;
        		 bp(mi) = bpM;
        		 //val eval=e;
        		 //val bpval=bpM;
        		 //Logger.info(()=>"error="+eval+" bp="+bpval);
        	 }
         }
         return r+nbSingles;	
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
		return (j < 0n) ? bp(i) : -1n;
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

	public  def verified():Boolean {
// 		var w:Int, m1:Int;
// 		var i:Int, j:Int;
// 		var pm:Int, pw:Int; //w_of_m, m_of_w;
// 		var r:Int = 0n;
// 
// 		variablesW.clear();
// 		for (m in variables.range())
// 			variablesW(variables(m)-1) = m as Int + 1n;
// 		
// 		for (m in variables.range()){
// 			pm = variables(m);
// 			for(i = 0n; (w = menPref(m)(i)) != pm; i++){
// 				pw = variablesW(w-1);
// 				for(j = 0n; (m1 = womenPref(w-1)(j)) != pw; j++){
// 					if ((m1-1n) == m as Int){
// 						Console.OUT.println("Not stable marriage, blocking pair m: "+m+", w: "+w);
// 						return false;
// 					}
// 				}
// 				
// 			}
// 		}
		return true;
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
		Utils.show("varM",variables);
	}
	
	public def swapVariables(i:Int, j:Int):void{
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
		var bpMaxi:Int = bp(maxi);
		var otheri:Int;
		var nbErr:Int = totalCost; /* the cost is the number of errors (ie. BP) */

		//Console.OUT.println("swap "+maxi+" y "+bpMaxi );
		if (bpMaxi != 0n)
			swapVariables(maxi, bpMaxi -1n);
		else{
			variables(maxi) = 0n; //man maxi is now single
			nbSingles++;
		}

// 		/* find second max if possible (random is to avoid to be trapped in the same local min) */
// 
// 		if (nbErr > 1n && r.randomDouble() < 0.98 && (otheri = findMax(bpMaxi)) >= 0n)
// 			swapVariables(otheri, bp(otheri)-1n);
// 		else {
// 			i = r.randomInt(length);
// 			j = r.randomInt(length);
// 			swapVariables(i, j);
// 		}
		return -1n;
	}
	
	
	public def getVariables():Valuation(sz){
		return variables;
	}
}

public type SMTIModel(s:Long)=SMTIModel{self.sz==s};
