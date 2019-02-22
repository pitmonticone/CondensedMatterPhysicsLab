#include <iostream>
#include <TGraphErrors.h>
#include <TAxis.h>
#include <TCanvas.h>
#include <TF1.h>
#include <iomanip>
#include <cmath>

using namespace std;

void BIcre2(){

const int nmisure = 13;
double I[]={0.00, 0.99, 1.97, 3.03, 4.02, 5.04, 6, 7.02, 8.06, 8.46, 9.02, 9.57, 10.06}; //A
double sI[nmisure]; //= {0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02};
double B[] = {0.009,0.072, 0.138, 0.208, 0.277, 0.345, 0.409, 0.475, 0.54, 0.561, 0.59, 0.616, 0.637};//T
double sB[nmisure];

for(int i = 0;i<nmisure;i++){
  sI[i]=0.02;
  sB[i]=0.001;
}

TCanvas *canvas = new TCanvas("canvas","B(I)",0,0,600,400);
canvas->SetFillColor(0);
canvas->cd();

TGraphErrors *TGE = new TGraphErrors(nmisure,I,B,sI,sB);
TGE->SetMarkerSize(0.6);
TGE->SetMarkerStyle(21);
TGE->SetTitle("B(I)");
TGE->GetXaxis()->SetTitle("I [A]");
TGE->GetYaxis()->SetTitle("B [T]");
TGE->Draw("AP");

TF1 *func = new TF1("func","[0]*x+[1]",0,7.02);
func->SetParNames("a","b");
func->SetParameter(0,0.06);
func ->SetParameter(1,0);
func->SetLineColor(2);
func->Draw("same");
TGE->Fit("func","RM+");

cout << "Chi^2:" << func->GetChisquare() << ", number of DoF: " << func->GetNDF() << " (Probability: " << func->GetProb() << ")." << endl;
cout << "--------------------------------------------------------------------------------------------------------" << endl;

TF1 *func1 = new TF1("func1","[0]*pow(x,2.)+[1]*x+[2]",7.02,10.06);
func1->SetParNames("ap","bp","cp");
func1->SetParameter(0,0.06);
func1 ->SetParameter(1,0);
func1->SetLineColor(3);
func1->Draw("same");
TGE->Fit("func1","RM+");

cout << "Chi^2:" << func1->GetChisquare() << ", number of DoF: " << func1->GetNDF() << " (Probability: " << func1->GetProb() << ")." << endl;
cout << "--------------------------------------------------------------------------------------------------------" << endl;

/*par0 = func->GetParameter(0);
errpar0 = func->GetParError(0);
cout<<"Il coefficiente angolare, da confrontare con varp, è: "<<par0<<"+-"<<errpar0<<endl<<"varp = "<<varp<<"+-"<<errvarp<<endl<<"Test zeta tra il parametro zero e varp: z= "<<abs(par0-varp)/sqrt(pow(errpar0,2.)+pow(errvarp,2.))<<endl;
for(int i =0; i<7;i++){
  cout<<varr[i]<<" ";
}
cout<<endl;
for(int i =0; i<7;i++){
  cout<<errvarr[i]<<" ";
}*/
}
