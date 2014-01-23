import x10.array.Array_2;

public class SMTIModel (sz:Long, seed:Long){
	protected val length = sz as Int;
	protected val variables = new Rail[Int]( sz, (i:Long) => i as Int);
	protected val r  = new RandomTools(seed);
	protected val solverParams = new ASSolverParameters();
	/** number of blocking pairs
	 */
	var nbBlockingPairs:Int;
	
	val nbBPperMan : Rail[Int];
	val variablesW : Rail[Int];
    val errV : Rail[Int];
    val bp : Rail[Int];
	
    val menPref : Rail[Rail[Int]];
    
    val womenPref : Rail[Rail[Int]];
    //val revpM : Rail[Rail[Int]];
    
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
		
		//revpM = new Rail[Rail[Int]](l, (Long) => new Rail[Int](l,0n));
		revpW = new Rail[Rail[Int]](l, (Long) => new Rail[Int](l,0n));
		//Logger.debug(()=>{"Preferences"});
		
		var m:Int, w:Int;
		for(m = 0n; m < length; m++)
			for(w = 0n; w < length; w++){
				//revpM(m)(menPref(m)(w)) = w;
				revpW(m)(womenPref(m)(w)) = w;
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
		solverParams.resetLimit =1n; //may be 1 is better for size 30 try()
		solverParams.resetPercent = 0n;
		solverParams.restartLimit = 1000000000n;
		solverParams.restartMax = 10n;
		solverParams.baseValue = 0n;
		solverParams.exhaustive = false;
		solverParams.firstBest = true;
		
	    solverParams.probChangeVector = 50n;
	}
	
	
	/**
	 * Returns the error if (m,w) forms a BP (0 else)
	 */
	public def blockingPairError(w:Int, pw:Int, m:Int) : Int{
		var err:Int = revpW(w)(pw) - revpW(w)(m);
		if (err < 0n)		/* (m,w) is a BP (blocking pair) */
			err = 0n;
		return err;
	}
	
	/**
	 * 	Computes the cost of the solution
	 * 	count the number of blocking pairs for the current Match
	 * 	@return cost
	 */
     public def costOfSolution( shouldBeRecorded : Int ) : Int {	
         var w:Int, m1:Int;
         var i:Int, j:Int;
         var pm:Int, pw:Int; //w_of_m, m_of_w;
         var r:Int = 0n;
       
         //errV.clear();
         for (m in variables.range())
             variablesW(variables(m)) = m as Int;
         
         for (m in variables.range()){
        	 pm = variables(m);
        	 var bpM:Int = -1n;		/* NB: -1 to avoid false positive in the test of Cost_If_Swap */
        	 var e:Int = 0n; 	 	
        	 
        	 for(i = 0n; (w = menPref(m)(i)) != pm; i++){
        		 pw = variablesW(w);
        		 e = blockingPairError(w, pw, m as Int);	 
        		 
        		 if (e > 0n){
        			 bpM = pw;
        			 r++;              /* count the errors (number of BP) */
        			 break; 			//only consider undominated BP
        		 }
        	 }
        	 if (shouldBeRecorded != 0n){
        		 errV(m) = e;
        		 bp(m) = bpM;
        		 val eval=e;
        		 val bpval=bpM;
        		 Logger.debug(()=>"error="+eval+" bp="+bpval);
        	 }
         }
         return r;	
     }
	
	/** 
	 * 	costOnVariable( i : Int ) : Int
	 * 	This function computes the cost of individual variable i
	 * 	@param i This is the variable that we want to know the cost
	 *  @return Int value with the cost of this variable
	 */
	public def costOnVariable( i : Int ) : Int{		
		return errV(i);
	}
	
	
	// public def swap2(i:Int, j:Int){
	// 	var x:Int = variables(i);
	// 	variables(i) = variables(j);
	// 	variables(j) = x;
	// }
	
	public def nextJ(i:Int, j:Int, exhaustive:Int):Int
	{
		return (j < 0n) ? bp(i) : -1n;
	}
	
	
	/**
	 *  executedSwap( i1 : Int, i2 : Int)
	 *  This function updates the values of the object data structures for the problem due to the 
	 *  completion of a swap between two variables
	 *  @param i1 First variable already swapped
	 *  @param i2 Second variable already swapped
	 */
	public def executedSwap( i1 : Int, i2 : Int) {
		this.costOfSolution(1n);
	}
	
	public def displaySolution(){
		var i:Int=0n;
		Console.OUT.println("\nMatching  m->w:");
		for (i=0n; i<length;i++){
			Console.OUT.print(i+"->"+variables(i)+"   ");
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
	public def costIfSwap(current_cost:Int,var i1:Int, var i2:Int):Int
	{
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
		var w:Int, m1:Int;
		var i:Int, j:Int;
		var pm:Int, pw:Int; //w_of_m, m_of_w;
		var r:Int = 0n;

		variablesW.clear();
		for (m in variables.range())
			variablesW(variables(m)) = m as Int;
		
		for (m in variables.range()){
			pm = variables(m);
			for(i = 0n; (w = menPref(m)(i)) != pm; i++){
				pw = variablesW(w);
				for(j = 0n; (m1 = womenPref(w)(j)) != pw; j++){
					if (m1 == m as Int){
						Console.OUT.println("Not stable marriage, blocking pair m: "+m+", w: "+w);
						return false;
					}
				}
				
			}
		}
		return true;
	}
	
	
	private def printPreferencesTables(){
		Console.OUT.println("\nMen Preferences");
		var i:Int = 0n;
		for (i=0n; i<length; i++){
			Console.OUT.print(i+": ");
			for(j in menPref(i))
				Console.OUT.print(j+" ");
			Console.OUT.println("");
		}
		Console.OUT.println("Women Preferences");
		for (i=0n; i<length; i++){
			Console.OUT.print(i+": ");
			for(j in womenPref(i))
				Console.OUT.print(j+" ");
			Console.OUT.println("");
		}
		
		// Console.OUT.println("Men rev Preferences");
		// for (i=0n; i<length; i++){
		// 	Console.OUT.print(i+": ");
		// 	for(j in revpM(i))
		// 		Console.OUT.print(j+" ");
		// 	Console.OUT.println("");
		// }
		
		Console.OUT.println("Women rev Preferences");
		for (i=0n; i<length; i++){
			Console.OUT.print(i+": ");
			for(j in revpW(i))
				Console.OUT.print(j+" ");
			Console.OUT.println("");
		}
	}
	
	static def createPrefs(l:Long,seed:Long):Rail[Rail[Int]]{
		val r = new RandomTools(seed);
		val prefs = new Rail[Rail[Int]](l, (Long) => new Rail[Int](r.randomPermut(l, 0n)));
		return prefs;
	}
	
	static def createSMTI(l:Long,seed:Long):Rail[Rail[Int]]{
		val r = new RandomTools(seed);
		val prefs = new Rail[Rail[Int]](l, (Long) => new Rail[Int](r.randomPermut(l, 0n)));
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
		//Main.show("after ini",variables);
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
			val ival= i;
			val eval=e;
			Logger.debug(()=>"error "+ival+":"+eval);
			if (e == 0n || i == pvalue || bp(i) == pvalue) 
				continue;
			
			
			if (e > maxErr)
			{ 	
				maxi = i;
				maxErr = e;
				maxNb = 1n;
			} 
			else if (e == maxErr && r.randomInt(++maxNb) == 0n)
			{
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

		swapVariables(maxi, bpMaxi);

		/* find second max if possible (random is to avoid to be trapped in the same local min) */

		if (nbErr > 1n && r.randomDouble() < 0.98 && (otheri = findMax(bpMaxi)) >= 0n)
			swapVariables(otheri, bp(otheri));
		else {
			i = r.randomInt(length);
			j = r.randomInt(length);
			swapVariables(i, j);
		}

		return -1n;
	}
}

public type SMTIModel(s:Long)=SMTIModel{self.sz==s};
