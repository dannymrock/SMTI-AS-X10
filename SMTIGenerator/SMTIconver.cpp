#include <fstream>
#include <iostream>
#include <cstdlib> 
#include <sstream>
#include <string>
#include <cstring> 
#include <climits>

using namespace std;      

void parseFLine(stringstream&, int&, int&, int&);
void processLine (stringstream&);

int main (int argc, char *argv[]) {
  int i, j;
  if ( argc != 2 ) { 
    cout << argv[0]<<" usage: smtif <file>\n";
    return -1;
  }

  char file[100];
  strcpy(file,argv[1]);

  ifstream infile(file);
  string line;
  int lNb = 0;
  int size, p1, p2;

  while (getline(infile, line)){
    lNb++;
    stringstream linestream(line);
    if(lNb==1){
      parseFLine(linestream, size, p1, p2);
      // cout<<" Size= "<<size<<" P1= "<<p1<<" P2= "<< p2 << endl; 
    }
    if(lNb >= 3 && lNb < size+3){
      cout<<(lNb-2)<<": ";
      processLine(linestream);
    }
    else if(lNb > size+3 && lNb <= size*2+3){
      cout<<(lNb - size - 3)<<": ";
      processLine(linestream);
    }
    if(lNb==size+3){
      cout<<endl; 
    }   
  }
  //cout<<endl; 
}

void parseFLine (stringstream &l, int &size, int &p1, int &p2){
  string item;
  int iNb = 0;
  while (getline(l, item, ' ')) {
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

void processLine (stringstream &line){
  string item;
  int iNb = 0;
  int prefAnt = 0;
  int pref = 0;

  while (getline(line, item, ' ')){ //Divide items
    iNb++;
    pref = atoi(item.c_str());
    if (iNb == 1){ //first Element
      prefAnt = pref;
      continue;
    }

    if ( pref < 0 && prefAnt > 0 ){ // start of a tie
      cout<<"( ";
    }
    
    cout << (prefAnt < 0 ? (prefAnt*-1) : prefAnt) <<" ";
    
    if ( pref > 0 && prefAnt < 0 ){ // end of a tie
      cout<<") ";
    }

    
    prefAnt = pref;
  }
  if (pref < 0){
    cout<<(pref*-1)<<" )"<< endl;
  }else{
    cout<<pref<<" "<< endl;
  }
}
