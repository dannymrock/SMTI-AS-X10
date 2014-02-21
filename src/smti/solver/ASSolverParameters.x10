package smti.solver;
import smti.util.*;
/** ASSolverParameters
 * 	Encapsulate all the parameters for AS Solver
 * 
 *  Based on the C implementation of Adaptive Search algoritm by Daniel Diaz
 * 
 * 	@author Danny Munera
 *  @version 0.1 April 9, 2013 First Version
 */

public class ASSolverParameters{
    
	/** perform an exhausitve search */ 
	public var exhaustive : Boolean;
	/** stop as soon as a better swap is found */
	public var firstBest : Boolean;
	/** % to select local min instead of staying on a plateau (or >100 to not use)*/
	public var probSelectLocMin : Int;	
	/** nb swaps to freeze a (local min) public var */
	public var freezeLocMin : Int;
	/** nb swaps to freeze 2 swapped public vars */
	public var freezeSwap : Int;
	/** nb of frozen public vars before reset */
	public var resetLimit : Int;
	/** nb public variables to reset */
	public var nbVarToReset : Int;
	/** nb of iterations before restart */
	public var restartLimit : Int;	
	/** max nb of times to restart (to retry) */
	public var restartMax : Int;
	/** true if Cost_Of_Solution must be called twice */
	public var reinitAfterIfSwap : Int;	
	/** percentage of public variables to reset */
	public var resetPercent : Int;		

	public var baseValue : Int;
	/** Probability to change variables vector for a vector in the pool (Comm Enable) */
	public var probChangeVector : Int;
	
	/** minimum permisible distance between places */
	public var minDistance : Double;
	
	
	/**
	 * 	Constructor
	 */
	public def this(){
		firstBest = false; //revisar val por default
		nbVarToReset = -1n;
		probChangeVector = 100n;
		minDistance = 0.3;
	}
	
	/**
	 *  set the values of the parameters to the solver
	 * 	@param toSet parameters to set
	 */
	public def setValues(toSet: ASSolverParameters):void{
		this.exhaustive = toSet.exhaustive;
		this.firstBest = toSet.firstBest;
		this.probSelectLocMin = toSet.probSelectLocMin;
		this.freezeLocMin = toSet.freezeLocMin;
		this.freezeSwap = toSet.freezeSwap;
		this.resetLimit = toSet.resetLimit;
		this.nbVarToReset = toSet.nbVarToReset;
		this.restartLimit = toSet.restartLimit;
		this.restartMax = toSet.restartMax;
		this.reinitAfterIfSwap = toSet.reinitAfterIfSwap;	
		this.resetPercent = toSet.resetPercent;
		this.baseValue = toSet.baseValue;
		this.probChangeVector = toSet.probChangeVector;
		this.minDistance = toSet.minDistance;
	}	
}