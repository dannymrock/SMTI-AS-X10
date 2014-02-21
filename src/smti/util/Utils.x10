package smti.util;

public class Utils {
	public static def copy[T](a:Rail[T]):Rail[T]{self.size==a.size} =new Rail[T](a.size, (i:Long)=>a(i));
	
	public static def show(s:String, d: Rail[Int]) {
	    Console.OUT.print(s + " in "+here.id+" : ");
	    for(k in d.range()) 
	        Console.OUT.print(" " + d(k));		
	    Console.OUT.println("");
	}
	
	
	/*
	 *  RANDOM_PERMUT_CHECK
	 * 
	 *  Check if a vector of size elements is a permutation of
	 *  - of values in base_value..base_value+size-1 (if actual_value == NULL)
	 *  - of values in actual_value[] + base_value
	 * 
	 *  Returns -1 if OK or the index of a found error (maybe not the first)
	 */

	public def permutCheck():Boolean
	// 	int
// 	Random_Permut_Check(int *vec, int size, const int *actual_value, int base_value)
 	{
 		var i:Int, j:Int, v:Int;
 		var ret:Int = -1n;

 		/* step 1: transform all values of vec[] to values in 0..size-1 */
// 
// 		if (actual_value == NULL)
// 		{
// 			for(i = 0; i < size; i++)
// 			{
// 				v = vec[i] - base_value;
// 				if (v < 0 || v >= size) /* error */
// 				{
// 					if (base_value != 0)
// 						for(j = 0; j < i; j++)	/* restore: re-add base_value */
// 							vec[j] += base_value;
// 
// 					return i;
// 				}
// 				vec[i] = v;
// 			}
// 		}
// 		else
// 		{
// 			for(i = 0; i < size; i++)
// 			{
// 				v = vec[i] - base_value;
// 				for(j = 0; ; j++)
// 				{
// 					if (j >= size)	/* not found: error */
// 					{
// 						for(j = 0; j < i; j++)	/* restore: replace index by values */
// 							vec[j] = actual_value[vec[j]] + base_value;
// 
// 						return i;
// 					}
// 					if (actual_value[j] == v)
// 						break;
// 				}
// 
// 				vec[i] = j;		/* found: replace value by index in actual_value[] */
// 			}
// 		}
// 
// 		/* step 2: check nothing is taken more than once */
// 
// 		for(i = 0; i < size; i++)
// 		{
// 			v = Value(i);
// 
// 			/* if v is taken it is an error except if actual_value[v] appears several times
// 			 * (since repeated elements must be grouped thus the next one is at v+1)
// 			 * then replace all following refs to v by v+1 (and v becomes v+1)
// 			 */
// 
// 			if (IsTaken(v))
// 			{
// 				int v0 = v++;
// 				if (actual_value == NULL || v >= size || actual_value[v0] != actual_value[v])
// 				{
// 					ret = i;		/* error at i */
// 					break;
// 				}
// 
// 				for(j = i; j < size; j++)
// 					if (Value(j) == v0)
// 						Assign(j, v);
// 			}
// 			
// 			SetTaken(v);
// 		}
// 
// 		/* step 3: remove 'taken' marks and restore initial values */
// 
// 		if (actual_value == NULL)
// 		{				/* reset taken and re-add base_value */
// 			for(i = 0; i < size; i++)
// 				vec[i] = Value(i) + base_value; 
// 		}
// 		else
// 		{				/* reset taken and set to actual_value[] + base_value */
// 			for(i = 0; i < size; i++)
// 				vec[i] = actual_value[Value(i)] + base_value; 
// 		} 
 		return ret==1n;
 }
	
}