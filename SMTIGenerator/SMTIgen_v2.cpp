#include <fstream>
#include <iostream>
#include <ctime>
#include <cstdlib> 
#include <sstream>
#include <string>

using namespace std;

double randomDouble(void);
unsigned random(unsigned );
void printPref(int **, size_t );
void initPref(int **, size_t);
void createPrefs(int, int, int, int**, int**);
void writePref(int, int, int, int**, int **, string);

int main (int argc, char *argv[]) {
  int i, j;
  if ( argc != 6 ) { 
    cout << argv[0]<<" usage: smtigen <size> <p1> <p2> <n-problems> <seed>\n";
    return -1;
  }

  // size of the SMTI problem
  int size = atoi(argv[1]);
  // p1 probability of incompleteness
  int p1 = atoi(argv[2]);
  // p2 probability of ties
  int p2 = atoi(argv[3]);
  // number of problems to generate
  int nProblems = atoi(argv[4]);
  // seed
  long seed = atol(argv[5]);
  //seed
  srand(seed);


  cout << "size="<<size<<" p1="<<p1<<" p2="<<p2<<" number_of_problems="
       <<nProblems<<" seed:"<<seed<<endl ;

  // creating Women and Men Prefs
  int **womenPrefs = new int* [size]();
  int **menPrefs = new int* [size](); 
  for (size_t i = 0; i < size; ++i){
    womenPrefs[i] = new int [size]();
    menPrefs[i] = new int [size]();
  }
  
  int pn;
  for(pn=0; pn<nProblems; pn++){
    ostringstream os;
    os << "SMTI"<<size<<"-D"<<p1<<"-T"<<p2<<"-"<<pn<<".smp";
    string s = os.str();
    // Initializing the matrix with random permutations
    createPrefs(p1, p2, size, menPrefs, womenPrefs);
    //creating the smti problems with a probability p1 and p2
    //printPref(menPrefs, size);
    //printPref(womenPrefs, size);
    writePref(p1, p2, size, menPrefs, womenPrefs, s);
  }

  // free memory
  for(int i = 0; i < size; i++){
    free(menPrefs[i]);
    free(womenPrefs[i]);
  }
  free(menPrefs); free(womenPrefs); 
}


double randomDouble(void) {
  return ((double) rand() / (RAND_MAX + 1.0));
}

unsigned random(unsigned n) {
  return (unsigned) (randomDouble() * n);
}

void printPref(int **pref, size_t sizeP){
    cout << __func__ << endl;
    for (size_t i = 0; i < sizeP; ++i)
    {
        cout << i << ": ";
        for (size_t j = 0; j < sizeP; ++j)
            cout << pref[i][j] << '\t';
        cout << endl;
    }
}

void writePref(int p1, int p2, int sizeP, int** mp, int **wp, string fileName ){
    cout << __func__ << endl;
    
    ofstream myfile;
    myfile.open (fileName.c_str());
    //myfile << "# SMTI size:"<<sizeP<<" p1:"<<p1<<" p2:"<<p2<<endl;
    myfile <<sizeP<<" "<<p1<<" "<<p2<<endl<<endl;
    //myfile << "# MenPrefs:"<<endl;
    for (size_t i = 0; i < sizeP; ++i){
      for (size_t j = 0; j < sizeP; ++j){
	if(mp[i][j]!=0)
	  myfile << mp[i][j] <<" ";
      }
      myfile <<endl;
    }
    myfile <<endl;//<<"# WomenPrefs:"<<endl<<endl;
    for (size_t i = 0; i < sizeP; ++i){
      for (size_t j = 0; j < sizeP; ++j){
	if(wp[i][j]!=0)
	  myfile << wp[i][j] <<" ";
      }
      myfile << endl;
    }
    myfile.close();
}

void initPref(int **pref, size_t sizeP){
 //Creating random permutation for each row
  int r, c;
  //val vec  = new Rail[Int] (size, (k:Long) => baseValue + k as Int);
  for (r=0; r<sizeP; r++){
    for(c=0; c<sizeP; c++)
      pref[r][c] = c+1;	
  }  
  for(r=0; r<sizeP; r++)
    for(c=sizeP-1; c>0; c--){
      int k = random(c+1);
      int z = pref[r][c];
      pref[r][c] = pref[r][k];
      pref[r][k] = z;
    }
}


void createPrefs( int p1, int p2, int sizeP, int** mP, int** wP){
  //val r = new RandomTools(seed);
  //var mPref:Rail[Rail[Int]] ;
  //var wPref:Rail[Rail[Int]] ;
  int wDel[sizeP]; 
  //= new Rail[Int](l as Long, 0n);
  
  // delete some entries with some probability p1
  int del, m, w, p, k;
  //int rep = 1;
 rep: 
  //Console.OUT.println("Creating SMTI");
  //rep = 0;
  fill(wDel, wDel+sizeP, 0);  //wDel.clear();
  initPref(mP, sizeP); // mPref = SMTIModel.createPrefs(l as Long, r.randomLong());
  initPref(wP, sizeP); // wPref = SMTIModel.createPrefs(l as Long, r.randomLong());
  for (m=0; m<sizeP; m++){
    del = 0;
    for (p=0; p<sizeP; p++){
      if ((random(100)+1) <= p1){
	del++;
	int dw = mP[m][p]-1;
	// deleting in mPref
	mP[m][p] = 0;
	for(k=0; k<sizeP; k++)
	  if (wP[dw][k] - 1 == m ){
	    // deleting in corresponding wPref
	    wP[dw][k] = 0;
	    wDel[dw]++;
	    if(wDel[dw]==sizeP){
	      //cout<<"Error: all W preferences deleted"<<endl;
	      goto rep;
	    }
	  }
      }
    }
    if (del == sizeP){
      goto rep;
    }
  } 
  //}while(rep == 1);
  
  // create some ties in entries with some probabilities p2
  int noFirst;
  for (m=0; m<sizeP; m++){
    noFirst=0;
    for (p=0; p<sizeP; p++){
      if(mP[m][p]==0)
	continue;
      if(noFirst==0){
	noFirst=1;
	continue;
      }
      if ((random(100)+1) <= p2){
	//val dw = mPref(m)(p);
	mP[m][p] *= -1; 	
      }
    }
  }
  
  for (w=0; w<sizeP; w++){
    noFirst=0;
    for (p=0; p<sizeP; p++){
      if(wP[w][p]==0) 
	continue;
      if(noFirst==0){
	noFirst=1;
	continue;
      }
      if ((random(100)+1) <= p2){
	//val dw = mPref(m)(p);
	wP[w][p] *= -1; 	
      }
    }
  }
}

