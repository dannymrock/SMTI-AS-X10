/**
 * A solver runs a local solver in every place, within the frame of a
 * ParallelSolverI instance. All communication by a solver with other solvers
 * in the team is mediated through the frame.
 *
 * <p> Therefore to design a new parallel solver, simply have it implement
 * ParallelSolverI.
 *
 */
public interface ParallelSolverI {
    property sz():Long;

    /**
     * Clear whatever internal state is stored about the current problem.
     */
    def clear():void;

    /**
     * Solves the problem, which is specified by cspGen. We expect (but this is not checked in the code) that
     * all instances of the ParallelSolverI frame (one in each place) is solving the same problem.
     */
    def solve(st:PlaceLocalHandle[ParallelSolverI(sz)], cspGen:()=>SMTIModel(sz) ):void;

    /**
     * Get some best solution from the communication partner or my local pool. If its
     * cost is less than myCost, update csp_ in place, and return true,
     * else return false.
     */
    def getIPVector(csp_:SMTIModel(sz), myCost:Int):Boolean;

    /**
     * Send this configuration (cost, current assignment of values to variables) to
     * communication partner(s).
     */
    def communicate(totalCost:Int, variables:Valuation(sz)):void;

    /**
     * Insert this configuration (sent from place) into the pool P at the current place,
     * if the cost is lower than the best cost in P.
     */
    def tryInsertVector(cost:Int, variables:Valuation(sz), place:Int):void;

    /** Return the value of the parameter used to control communication within the team
     * (intraTeamInterval).
     *
     */

    def intraTI():Int;

    /**
     * Send a signal to the associated solver to kill it. The solver will
     * kill itself the next time it checks for the kill signal.
     *
     */
    def kill():void;

    /**
     * When a place p has a solution, it invokes this method. The first place
     * to execute this method during the program run will be declared the winner;
     * for it, the method will return true. Any subsequent invocation will
     * return false.
     *
     * <p> In the invocation that returns true, kill() is invoked at every place.
     */
    def announceWinner(ss:PlaceLocalHandle[ParallelSolverI(sz)], p:Long):Boolean;

    def setStats(co : Int, p : Int, e : Int, t:Double, it:Int, loc:Int, sw:Int, re:Int, sa:Int, rs:Int, ch:Int,
            fr : Int, bp:Int, sg:Int):void;

    def getPoolData():Maybe[CSPSharedUnit(sz)];
    
    def getCurrentData():Maybe[CSPSharedUnit(sz)];

    def accStats(CSPStats):void;
    def printStats(count:Int):void;
    def printAVG(count:Int):void;
    
    def verifyWinner(ss:PlaceLocalHandle[ParallelSolverI(sz)]):void;
    public def getBP():Int;
    public def getCost():Int;
    def setStats1(ss:PlaceLocalHandle[ParallelSolverI(sz)]):void;
    


}
public type ParallelSolverI(s:Long)=ParallelSolverI{self.sz==s};