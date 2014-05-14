#include <fstream>
#include <iostream>
#include <cstdlib> 
#include <sstream>
#include <string>
#include <cstring> 
#include <climits>

using namespace std;      

struct Stats{
  int posFirstTie;
  int lengthFirstTie;
  int numberTies;
  int lengthTies;
  int lengthPL;
  
  Stats(int pFT, int lFT, int nT, int lT, int lPL, int mPL):
    posFirstTie(pFT), lengthFirstTie(lFT), numberTies(nT), lengthTies(lT), lengthPL(lPL){}
  
void AccStats(Stats l){
    this->posFirstTie += l.posFirstTie;
    this->lengthFirstTie += l.lengthFirstTie;
    this->numberTies += l.numberTies;
    this->lengthTies += l.lengthTies;
    this->lengthPL += l.lengthPL;
  }

  void PrintStats(int size){
    if(numberTies==0){
      cout<<"Number Ties = 0 \t AVG Length PrefList = "<<lengthPL/size;
    }else{
      cout<<posFirstTie/size<<"\t"<<lengthFirstTie/size<<"\t"<<numberTies<<"\t"
	  <<lengthTies/numberTies << "\t" <<lengthPL/size;
    }
  }
};

void parseFLine(stringstream&, int&, int&, int&);
Stats processLine (stringstream&);

int max_tie = 0;
double pos_max_tie = -1;
int min_pos = INT_MAX;
int minLengthMPL = INT_MAX;
int minLengthWPL = INT_MAX;

int main (int argc, char *argv[]) {
  int i, j;
  if ( argc != 2 ) { 
    cout << argv[0]<<" usage: smtif <file>\n";
    return -1;
  }

  // name of the file to analyse
  char file[100];
  strcpy(file,argv[1]);
  //cout << "File= "<< file;

  ifstream infile(file);
  string line;
  int lNb = 0;
  int size, p1, p2;
  Stats totalMen(0,0,0,0,0,INT_MAX);
  Stats totalWomen(0,0,0,0,0,INT_MAX);
  Stats total(0,0,0,0,0,INT_MAX);

  while (getline(infile, line)){
    lNb++;
    stringstream linestream(line);
    if(lNb==1){
      parseFLine(linestream, size, p1, p2);
      //cout<<" Size= "<<size<<" P1= "<<p1<<" P2= "<< p2 << endl; 
    }
    if(lNb >= 3 && lNb < size+3){
      //cout<<"Men: ";
      Stats line_stats = processLine(linestream);
      totalMen.AccStats(line_stats);
      total.AccStats(line_stats);
      if (line_stats.lengthPL < minLengthMPL){
	  minLengthMPL = line_stats.lengthPL;
      }
      //Men Pref List
    }
    else if(lNb > size+3 && lNb <= size*2+3){
      //cout<<"Women: ";
      //Women Pref List 
      Stats line_stats = processLine(linestream);
      totalWomen.AccStats(line_stats);
      total.AccStats(line_stats);
      if (line_stats.lengthPL < minLengthWPL){
	  minLengthWPL = line_stats.lengthPL;
      }
    }      
  }
  // cout << "Men:"<<endl;
  // cout<<"PFT \t LFT \t NT \t LT \t LPL "<<endl;
  // totalMen.PrintStats(size);
  // cout << "Women:"<<endl;
  // cout<<"PFT \t LFT \t NT \t LT \t LPL "<<endl;
  // totalWomen.PrintStats(size);
  // cout << "Total:"<<endl;
   cout<<"file , PFT , LFT , NT , LT , LPL , MaxTieL , MaxTiePos , minLengthMPL ,  minLengthWPL"<<endl;
  cout <<file<<"\t";
  total.PrintStats(size*2);
  cout << "\t"<<max_tie<< "\t"<<pos_max_tie<< "\t"<< minLengthMPL<<"\t"<< minLengthWPL<<endl;
  
}

void parseFLine (stringstream &l, int &size, int &p1, int &p2){
  string item;
  int iNb = 0;
  while (getline(l, item, ' '))
    {
      iNb++;
      switch(iNb){
      case 1: 
	size = atoi(item.c_str());
	//cout << "size = "<<  size << endl;
	break;
      case 2:
	p1 = atoi(item.c_str());
	//cout << "p1 = "<<  p1 << endl;
	break;
      case 3:
	p2 = atoi(item.c_str());
	//cout << "p2 = "<<  p2 << endl;
	break;
      }
    }
}

Stats processLine (stringstream &line){
  string item;
  int iNb = 0;
  int ltie = -1;
  int ntie = 0;
  int accLT = 0;
  bool ftie = false;
  Stats l(0,0,0,0,0,INT_MAX);
  int pref;

  while (getline(line, item, ' ')){
    iNb++;
    pref = atoi(item.c_str());
    //cout<<pref<<" ";
    if (pref < 0){
      if (ltie == -1){
	//First Tie
	//cout<< "Pos First Tie: "<<iNb-1<<endl;
	l.posFirstTie = iNb-1;
	ltie = 2;
	ftie = true;
      } 
      else if (ltie == 0){
	//Another Tie
	ltie = 2;
      }
      else{
	ltie++;
      }
    }
    else{
      if (ftie){
	//End First Tie
	//cout<<"Length first tie= "<< ltie << endl;
	l.lengthFirstTie = ltie;
	if(ltie > max_tie){
	  max_tie = ltie;
	  pos_max_tie = iNb - ltie + 0.1;
	}
	accLT += ltie;
	ftie = false;
	ltie = 0; ntie++;
      }else if (ltie > 1){
	//cout<<"Length tie= "<< ltie << endl;
	if(ltie > max_tie){
	  max_tie = ltie;
	  pos_max_tie = iNb - ltie;
	}
	accLT += ltie;
	ltie = 0; ntie++;
      }
    }  
  }

  // If the last entry in the pref. list is negative, It's necessariy
  // to compute that tie.
  if (pref < 0){ // commpute last tie
    if (ftie){
      //End First Tie
      //cout<<"Length first tie= "<< ltie << endl;
      l.lengthFirstTie = ltie;
      if(ltie > max_tie){
	max_tie = ltie;
	pos_max_tie = iNb - ltie + 0.1;
      }
      accLT += ltie;
      ftie = false;
      ltie = 0; ntie++;
    }else if (ltie > 1){
      //cout<<"Length tie= "<< ltie << endl;
      if(ltie > max_tie){
	max_tie = ltie;
	pos_max_tie = iNb - ltie;
      }
      accLT += ltie;
      ltie = 0; ntie++;
    }
  }


  l.numberTies = ntie;
  l.lengthTies = accLT;
  l.lengthPL = iNb;
  //cout <<"Total number of ties = "<< ntie << "total length = "<<accLT 
  //<<" length pref line= "<<iNb<<endl;
  return(l);
}
